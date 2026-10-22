;;; magento-cache-clean.el --- Emacs integration for magento-cache-clean -*- lexical-binding: t; -*-

;; Copyright (C) 2026  Piotr Kwiecinski

;; Author: Piotr Kwiecinski <piotr.kwiecinski@codemanufacture.com>
;; Version: 0.1.0
;; Package-Requires: ((emacs "30.1") (transient "0.7.2"))
;; Keywords: tools, processes
;; URL: https://github.com/emacs-magento/magento-cache-clean.el

;; This file is not part of GNU Emacs.

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;; Emacs integration for mage-os/magento-cache-clean, a file watcher
;; based cache cleaner for Magento 2.
;;
;; Features:
;; - One-shot cache cleaning via `magento-cache-clean'
;; - Watch mode via `magento-cache-clean-watch' (comint)
;; - Transient UI via `magento-cache-clean-transient'
;; - Auto-detection of Magento root directory
;; - Per-project configuration via .dir-locals.el
;;
;; Commands take a list of cache-clean command line arguments, so they
;; can also be called from Lisp:
;;
;;   (magento-cache-clean '("--keep-generated" "layout"))

;;; Code:

(require 'ansi-color)
(require 'comint)
(require 'compile)
(require 'transient)

;;; Customization

(defgroup magento-cache-clean nil
  "Interface to magento-cache-clean."
  :group 'tools
  :prefix "magento-cache-clean-"
  :link '(url-link "https://github.com/mage-os/magento-cache-clean"))

(defconst magento-cache-clean--candidates
  '(("vendor/bin/cache-clean.js" . t)
    ("cache-clean" . nil)
    ("cache-clean.js" . t))
  "Executables auto-detection tries, in order, and whether they need node.")

(defcustom magento-cache-clean-executable 'auto
  "The cache-clean executable.
`auto' uses the first one found of the Magento root's
vendor/bin/cache-clean.js, cache-clean and cache-clean.js; the .js
scripts are only used when node is found too.

A string is a program name, looked up in variable `exec-path', or a
file name.  A relative file name containing a slash is relative to the
Magento root.  A file name starting with ~ is expanded on the local
host, even for a remote root.

A function is called with the Magento root and returns such a string,
or nil to auto-detect.

Only `auto' and the predefined names are safe as directory local
values, since any other value could run any program."
  :type '(choice (const :tag "Auto-detect" auto)
                 (const "vendor/bin/cache-clean.js")
                 (const "cache-clean")
                 (const "cache-clean.js")
                 (string :tag "Other")
                 (function :tag "Function"))
  :safe (lambda (v)
          (or (eq v 'auto)
              (assoc v magento-cache-clean--candidates))))

(defcustom magento-cache-clean-directory nil
  "Magento root directory.
When nil, auto-detected by searching for app/etc/env.php.

A relative name is relative to the current buffer's directory, so only
absolute names are safe as directory local values."
  :type '(choice (const :tag "Auto-detect" nil)
                 (directory :tag "Directory"))
  :safe (lambda (v) (or (null v) (and (stringp v) (file-name-absolute-p v)))))

(defcustom magento-cache-clean-watch-query-on-exit nil
  "Whether to ask before killing a running watcher.
When non-nil, exiting Emacs or killing a watch buffer asks first.
The value takes effect for watchers started afterwards."
  :type 'boolean)

;;; Directory detection

(defun magento-cache-clean--known-roots ()
  "Return the Magento roots that have a cache-clean buffer."
  (delete-dups
   (mapcar (lambda (buf) (buffer-local-value 'default-directory buf))
           (match-buffers '(or (derived-mode . magento-cache-clean-mode)
                               (derived-mode . magento-cache-clean-watch-mode))))))

(defun magento-cache-clean--read-root ()
  "Read a Magento root directory in the minibuffer.
The roots that already have a cache-clean buffer are offered as
defaults, reachable with \\<minibuffer-local-map>\\[next-history-element].
Ask for confirmation when the directory has no app/etc/env.php, and
signal a `user-error' when it isn't given."
  (let ((dir (file-name-as-directory
              (expand-file-name
               (read-directory-name "Magento root directory: " nil
                                    (cons default-directory
                                          (magento-cache-clean--known-roots))
                                    t)))))
    (unless (or (file-exists-p (expand-file-name "app/etc/env.php" dir))
                (yes-or-no-p (format "%s has no app/etc/env.php.  Use it anyway? "
                                     (abbreviate-file-name dir))))
      (user-error "Not a Magento root: %s" (abbreviate-file-name dir)))
    dir))

(defun magento-cache-clean--root (&optional prompt)
  "Return the configured or detected Magento root.
The root is detected by looking for app/etc/env.php upward.  When it
can't be found, prompt for it if PROMPT is non-nil, else return nil.
Signal a `user-error' when the configured root isn't a directory."
  (if-let* ((dir (or magento-cache-clean-directory
                     (locate-dominating-file default-directory
                                             "app/etc/env.php"))))
      (let ((dir (file-name-as-directory (expand-file-name dir))))
        (unless (file-directory-p dir)
          (user-error "Magento root %s is not a directory" dir))
        dir)
    (and prompt (magento-cache-clean--read-root))))

(defun magento-cache-clean--menu-root ()
  "Return the root chosen in `magento-cache-clean-transient', or nil.
Only non-nil while a command runs from that menu."
  (and (eq transient-current-command 'magento-cache-clean-transient)
       (plist-get (transient-scope) :root)))

(defun magento-cache-clean--directory ()
  "Return the Magento root to run a command in, prompting if needed."
  (or (magento-cache-clean--menu-root)
      (magento-cache-clean--root t)))

;;; Arguments

(defun magento-cache-clean--transient-to-cli (args)
  "Convert transient ARGS to cache-clean command line arguments.
Each --types=TYPE becomes a positional TYPE and --file-list=FILE
becomes two arguments, as cache-clean doesn't accept the = form."
  (mapcan (lambda (arg)
            (pcase arg
              ((rx bos "--types=" (let type (+ anything)))
               (list type))
              ((rx bos "--file-list=" (let file (+ anything)))
               ;; The menu's reader already gives a local name, but a
               ;; value set from Lisp may still be remote.
               (list "--file-list" (file-local-name file)))
              (_ (list arg))))
          args))

(defun magento-cache-clean--arguments ()
  "Return the cache-clean arguments set in `magento-cache-clean-transient'.
Outside the transient this is its saved or default value."
  (magento-cache-clean--transient-to-cli
   (transient-args 'magento-cache-clean-transient)))

(defun magento-cache-clean--found-p (name root)
  "Return non-nil if executable NAME is found for Magento ROOT.
NAME is resolved as in `magento-cache-clean-executable'; on a remote
ROOT it is looked up on the remote host."
  (if (string-search "/" name)
      (file-executable-p (expand-file-name name root))
    (let ((default-directory root))
      (executable-find name t))))

(defun magento-cache-clean--auto-detect (root)
  "Return the name of the first cache-clean executable found for ROOT.
Signal a `user-error' when there is none.  Node is only looked up when
a script that needs it is found."
  (let* ((node 'unchecked)
         (node-p (lambda ()
                   (when (eq node 'unchecked)
                     (setq node (magento-cache-clean--found-p "node" root)))
                   node))
         (missing-node nil))
    (or (seq-some (pcase-lambda (`(,name . ,needs-node))
                    (when (magento-cache-clean--found-p name root)
                      (if (or (not needs-node) (funcall node-p))
                          name
                        (push name missing-node)
                        nil)))
                  magento-cache-clean--candidates)
        (user-error "No cache-clean found (tried %s)%s"
                    (mapconcat #'car magento-cache-clean--candidates ", ")
                    (if missing-node
                        (format "; %s found but node is missing"
                                (string-join (nreverse missing-node) " and "))
                      "")))))

(defun magento-cache-clean--configured-executable (root)
  "Return the cache-clean executable configured for Magento ROOT.
Return nil when it is to be auto-detected.  See
`magento-cache-clean-executable'.  Signal an error when the option
has an invalid value."
  (pcase magento-cache-clean-executable
    ('auto nil)
    ((and (pred stringp) name) name)
    ((and (pred functionp) fn)
     (pcase (funcall fn root)
       ((and (or 'nil (pred stringp)) name) name)
       (other (error "`magento-cache-clean-executable' returned %S, \
not a string or nil" other))))
    (other (user-error "Invalid `magento-cache-clean-executable': %S" other))))

(defun magento-cache-clean--executable (root)
  "Return the name of the cache-clean executable for Magento ROOT.
See `magento-cache-clean-executable'.  Signal a `user-error' when
auto-detection finds none or the option has an invalid value."
  (or (magento-cache-clean--configured-executable root)
      (magento-cache-clean--auto-detect root)))

(defun magento-cache-clean--program (name root)
  "Return the program to run for executable NAME and Magento ROOT.
A NAME containing a slash is a file name relative to ROOT; any other
NAME is looked up in variable `exec-path' when the process starts."
  (if (string-search "/" name)
      (file-local-name (expand-file-name name root))
    name))

(defun magento-cache-clean--command (root args)
  "Return the cache-clean command line for ROOT and ARGS as a list.
ROOT may be remote; cache-clean gets its local name.  The executable is
resolved in the current buffer, whose variable `exec-path' may be
local.  From `magento-cache-clean-transient', the executable the menu
found for ROOT is reused, as looking it up again is slow when remote."
  (let ((name (or (and (equal root (magento-cache-clean--menu-root))
                       (plist-get (transient-scope) :executable))
                  (magento-cache-clean--executable root))))
    `(,(magento-cache-clean--program name root)
      "-d" ,(file-local-name root) ,@args)))

;;; Buffers

(defun magento-cache-clean--buffer-name (root &optional watch)
  "Return the output buffer name for Magento ROOT.
When WATCH is non-nil, return the watch buffer name."
  (format "*magento-cache-clean%s: %s*"
          (if watch "-watch" "")
          (abbreviate-file-name (directory-file-name root))))

(defun magento-cache-clean--watch-buffers ()
  "Return the watch buffers with a running watcher."
  (match-buffers `(and (derived-mode . magento-cache-clean-watch-mode)
                       ,(lambda (buf) (process-live-p (get-buffer-process buf))))))

(defun magento-cache-clean--watch-buffer (&optional root)
  "Return the watch buffer for Magento ROOT, or nil if there is none.
ROOT defaults to the menu's root, the current watch buffer's root or
the current buffer's root, in that order."
  (let ((root (or root (magento-cache-clean--menu-root))))
    (cond (root (get-buffer (magento-cache-clean--buffer-name root t)))
          ((derived-mode-p 'magento-cache-clean-watch-mode) (current-buffer))
          ((when-let* ((root (magento-cache-clean--root)))
             (get-buffer (magento-cache-clean--buffer-name root t)))))))

(defun magento-cache-clean--watch-process (&optional root)
  "Return the live watch process for Magento ROOT, or nil.
ROOT defaults as in `magento-cache-clean--watch-buffer'."
  (when-let* ((buf (magento-cache-clean--watch-buffer root))
              (proc (get-buffer-process buf)))
    (and (process-live-p proc) proc)))

;; Tramp passes the variables that differ from the global environment
;; to the remote host, so a local buffer's environment must not follow
;; a remote root, or vice versa.
(defun magento-cache-clean--same-host-p (root)
  "Return non-nil if Magento ROOT is on the current buffer's host."
  (equal (file-remote-p root) (file-remote-p default-directory)))

;;; Modes

(defvar magento-cache-clean-error-regexp-alist nil
  "Value of `compilation-error-regexp-alist' in `magento-cache-clean-mode'.
cache-clean output has no file:line locations by default, so this is
empty.  Add entries to make parts of the output clickable.")

(define-compilation-mode magento-cache-clean-mode "MagentoCacheClean"
  "Major mode for one-shot magento-cache-clean output."
  ;; `compilation-start' sets the root as `default-directory' first.
  ;; The root's options then apply when the menu is opened from here.
  (hack-dir-local-variables-non-file-buffer)
  (add-hook 'compilation-filter-hook #'ansi-color-compilation-filter nil t))

(defvar-local magento-cache-clean--watch-args nil
  "Command line arguments the watch process in this buffer was started with.")

(defun magento-cache-clean-watch-send-key ()
  "Send the key just pressed to the watch process.
cache-clean reads stdin in raw mode and handles the key itself."
  (interactive)
  (if-let* ((proc (get-buffer-process (current-buffer))))
      (process-send-string proc (string last-command-event))
    (user-error "No watch process running")))

(defvar-keymap magento-cache-clean-watch-mode-map
  :doc "Keymap for `magento-cache-clean-watch-mode'."
  :parent comint-mode-map
  "<remap> <self-insert-command>" #'magento-cache-clean-watch-send-key
  ;; Comint would send the line at point, typing its characters as hotkeys.
  "RET" #'magento-cache-clean-watch-send-key
  "C-c C-r" #'magento-cache-clean-watch-restart)

(defun magento-cache-clean--watch-interrupt ()
  "Interrupt the watcher running in the current buffer, if any.
Deleting the process, as `comint-exec' and `kill-buffer' do, only ends
Tramp's local end of a remote watcher, while interrupting it reaches
the remote host."
  (when-let* ((proc (get-buffer-process (current-buffer)))
              ((process-live-p proc)))
    (interrupt-process proc)))

(defun magento-cache-clean--interrupt-watchers ()
  "Interrupt all running watchers, as `kill-emacs-hook'.
Emacs only deletes processes when exiting; see
`magento-cache-clean--watch-interrupt'."
  (dolist (buf (magento-cache-clean--watch-buffers))
    ;; An error would keep Emacs from exiting.
    (with-demoted-errors "Error interrupting watcher: %S"
      (with-current-buffer buf
        (magento-cache-clean--watch-interrupt)))))

(defun magento-cache-clean--watch-revert (_ignore-auto _noconfirm)
  "Restart the watcher, as `revert-buffer-function' of watch buffers."
  (magento-cache-clean-watch-restart))

(define-derived-mode magento-cache-clean-watch-mode comint-mode
  "MagentoCacheClean:Watch"
  "Major mode for magento-cache-clean watch buffers.
Printable keys are sent straight to the cache-clean process, which
handles them as hotkeys; see the cache-clean README for the list.
\\[comint-interrupt-subjob] stops the watcher, and \\[magento-cache-clean-watch-restart] or \
\\[revert-buffer] restarts it with the same arguments.
The buffer is truncated to `comint-buffer-maximum-size' lines.

\\{magento-cache-clean-watch-mode-map}"
  (setq-local revert-buffer-function #'magento-cache-clean--watch-revert)
  (add-hook 'kill-buffer-hook #'magento-cache-clean--watch-interrupt nil t)
  (add-hook 'comint-output-filter-functions #'comint-truncate-buffer nil t))

(defun magento-cache-clean--watch-start (root args)
  "Start cache-clean watching Magento ROOT with ARGS and return its buffer.
When ROOT is on the current buffer's host, the process inherits the
current buffer's local `process-environment' and variable `exec-path',
as with `compilation-start'.  Any watcher already running for ROOT is
restarted."
  (let* ((same-host (magento-cache-clean--same-host-p root))
         (env (and same-host (local-variable-p 'process-environment)
                   process-environment))
         (path (and same-host (local-variable-p 'exec-path) exec-path))
         ;; Before creating the buffer, as no executable may be found.
         (command (magento-cache-clean--command root (cons "--watch" args)))
         (buf (get-buffer-create (magento-cache-clean--buffer-name root t))))
    (with-current-buffer buf
      (unless (derived-mode-p 'magento-cache-clean-watch-mode)
        (magento-cache-clean-watch-mode))
      ;; Set after the mode change, which kills local variables.
      (setq default-directory root)
      ;; So that a restart or the menu opened from here use the root's
      ;; options, not the global ones.
      (hack-dir-local-variables-non-file-buffer)
      (if env
          (setq-local process-environment env)
        (kill-local-variable 'process-environment))
      (if path
          (setq-local exec-path path)
        (kill-local-variable 'exec-path))
      (setq magento-cache-clean--watch-args args)
      ;; `comint-exec' would only delete a running watcher.
      (magento-cache-clean--watch-interrupt)
      (goto-char (point-max))
      (insert (propertize (concat "$ " (combine-and-quote-strings command) "\n")
                          'font-lock-face 'comint-highlight-input))
      (comint-exec buf (buffer-name buf) (car command) nil (cdr command))
      (set-process-query-on-exit-flag (get-buffer-process buf)
                                      magento-cache-clean-watch-query-on-exit)
      (add-hook 'kill-emacs-hook #'magento-cache-clean--interrupt-watchers))
    buf))

;;; Commands

;;;###autoload
(defun magento-cache-clean (&optional args)
  "Run cache-clean once with command line ARGS.
Interactively, ARGS come from `magento-cache-clean-transient'."
  (interactive (list (magento-cache-clean--arguments)))
  (let* ((root (magento-cache-clean--directory))
         (name (magento-cache-clean--buffer-name root))
         ;; A remote shell is POSIX, whatever the local system is.
         (posix (file-remote-p root))
         (command (mapconcat (lambda (arg) (shell-quote-argument arg posix))
                             (magento-cache-clean--command root args)
                             " "))
         (start (lambda ()
                  (let ((default-directory root))
                    (compilation-start command #'magento-cache-clean-mode
                                       (lambda (_) name))))))
    ;; `compilation-start' inherits the current buffer's environment.
    (if (magento-cache-clean--same-host-p root)
        (funcall start)
      (with-temp-buffer (funcall start)))))

;;;###autoload
(defun magento-cache-clean-watch (&optional args)
  "Start cache-clean in watch mode with command line ARGS.
Interactively, ARGS come from `magento-cache-clean-transient'.
A running watcher is restarted, after confirmation unless this is run
from that menu, where the command reads \"Restart watch\".  Declining
just shows it.  Return the watch buffer."
  (interactive (list (magento-cache-clean--arguments)))
  (let* ((root (magento-cache-clean--directory))
         (proc (magento-cache-clean--watch-process root))
         (buf (if (and proc
                       (not (magento-cache-clean--menu-root))
                       (not (yes-or-no-p
                             "Watch process already running.  Restart? ")))
                  (process-buffer proc)
                (magento-cache-clean--watch-start root args))))
    (display-buffer buf)
    buf))

(defun magento-cache-clean-watch-restart ()
  "Restart the watcher in the current watch buffer with the same arguments."
  (interactive nil magento-cache-clean-watch-mode)
  (magento-cache-clean--watch-start default-directory
                                    magento-cache-clean--watch-args))

(defun magento-cache-clean--read-watch-buffer ()
  "Return the watch buffer a stop command acts on.
This is the watcher of the current Magento root when there is one.
Otherwise, read one of the running watchers in the minibuffer."
  (if-let* ((proc (magento-cache-clean--watch-process)))
      (process-buffer proc)
    (let ((watchers (magento-cache-clean--watch-buffers)))
      (unless watchers
        (user-error "No watch process running"))
      (get-buffer
       (read-buffer "Stop watcher: " (car watchers) t
                    (lambda (name)
                      (memq (get-buffer (if (consp name) (car name) name))
                            watchers)))))))

;;;###autoload
(defun magento-cache-clean-watch-stop (buffer)
  "Stop the watcher running in watch BUFFER, a buffer or its name.
Interactively, BUFFER is the watcher of the current Magento root, or
else one of the running watchers read in the minibuffer."
  (interactive (list (magento-cache-clean--read-watch-buffer)))
  (setq buffer (window-normalize-buffer buffer))
  (let ((proc (get-buffer-process buffer)))
    (unless (process-live-p proc)
      (user-error "No watch process running in %s" buffer))
    (interrupt-process proc))
  (message "magento-cache-clean watch stopped in %s"
           (abbreviate-file-name (buffer-local-value 'default-directory buffer))))

;;;###autoload
(defun magento-cache-clean-watch-stop-all ()
  "Stop all running watchers."
  (interactive)
  (let ((buffers (magento-cache-clean--watch-buffers)))
    (unless buffers
      (user-error "No watch process running"))
    (dolist (buf buffers)
      (interrupt-process (get-buffer-process buf)))
    (message "Stopped %d magento-cache-clean watcher%s"
             (length buffers) (if (cdr buffers) "s" ""))))

;;; Transient

(transient-define-infix magento-cache-clean--infix-verbosity ()
  :class 'transient-switches
  :key "-v"
  :description "Verbosity"
  :argument-format "--%s"
  :argument-regexp "\\`--\\(verbose\\|debug\\|silent\\)\\'"
  :choices '("verbose" "debug" "silent"))

(defconst magento-cache-clean--core-cache-types
  '("config" "layout" "block_html" "collections" "reflection" "db_ddl"
    "compiled_config" "eav" "customer_notification" "config_integration"
    "config_integration_api" "graphql_query_resolver_result" "full_page"
    "config_webservice" "translate")
  "Cache types of Magento Open Source, completed without an env.php.")

(defun magento-cache-clean--env-cache-types (root)
  "Return the cache types listed in Magento ROOT's app/etc/env.php, or nil.
Return nil too when the list isn't closed, rather than reading the
keys of the rest of the file as cache types."
  (with-temp-buffer
    (insert-file-contents (expand-file-name "app/etc/env.php" root))
    (when-let* (((re-search-forward
                  (rx (in "'\"") "cache_types" (in "'\"") (* (in " \t\n")) "=>"
                      (* (in " \t\n")) (or "[" (seq "array" (* (in " \t")) "(")))
                  nil t))
                (end (save-excursion (re-search-forward (rx (in "])")) nil t))))
      (let (types)
        (while (re-search-forward
                (rx (in "'\"") (group (+ (not (in "'\"")))) (in "'\"")
                    (* (in " \t\n")) "=>")
                end t)
          (push (match-string 1) types))
        (nreverse types)))))

(defun magento-cache-clean--cache-types ()
  "Return the cache types to complete in `magento-cache-clean-transient'.
They are read from the menu root's env.php, falling back to the core
types.  Other names can still be entered, since cache-clean takes any."
  (or (ignore-errors
        (magento-cache-clean--env-cache-types (plist-get (transient-scope) :root)))
      magento-cache-clean--core-cache-types))

(transient-define-infix magento-cache-clean--infix-cache-types ()
  :class 'transient-option
  :key "-t"
  :description "Cache types"
  :argument "--types="
  :multi-value 'repeat
  :choices #'magento-cache-clean--cache-types)

(defun magento-cache-clean--transient-scope (root)
  "Return the scope of `magento-cache-clean-transient' for Magento ROOT.
The executable is looked up once, since doing it on every menu refresh
and command is slow on a remote ROOT.  The scope has the executable's
name and whether it was auto-detected, or the message of the error
looking it up.  It is computed in the buffer the menu is opened from,
since that is where directory local values apply."
  (condition-case err
      (let ((name (magento-cache-clean--configured-executable root)))
        (list :root root
              :executable (or name (magento-cache-clean--auto-detect root))
              :auto (not name)))
    (error (list :root root :error (error-message-string err)))))

(defun magento-cache-clean--transient-executable (scope)
  "Describe the cache-clean executable of the menu SCOPE."
  (or (plist-get scope :error)
      (concat (plist-get scope :executable)
              (and (plist-get scope :auto) " (auto)"))))

(defun magento-cache-clean--transient-description ()
  "Return the heading of `magento-cache-clean-transient'."
  (let ((scope (transient-scope)))
    (format "Magento dir: %s   cache-clean: %s"
            (abbreviate-file-name (plist-get scope :root))
            (magento-cache-clean--transient-executable scope))))

(defun magento-cache-clean--transient-watching-p ()
  "Return non-nil if the menu's Magento root has a running watcher."
  (magento-cache-clean--watch-process (plist-get (transient-scope) :root)))

(defun magento-cache-clean--read-file-list (prompt initial-input history)
  "Read an existing file for --file-list, starting in the menu's root.
PROMPT, INITIAL-INPUT and HISTORY are as for a transient reader."
  (let ((default-directory (plist-get (transient-scope) :root)))
    (transient-read-existing-file prompt initial-input history)))

;;;###autoload (autoload 'magento-cache-clean-transient "magento-cache-clean" nil t)
(transient-define-prefix magento-cache-clean-transient (&optional choose-root)
  "Transient interface for magento-cache-clean.
The Magento root is detected when the menu opens.  With a prefix
argument CHOOSE-ROOT, or when it can't be detected, prompt for it."
  [:description magento-cache-clean--transient-description
   ["Options"
    (magento-cache-clean--infix-verbosity)
    ("-k" "Keep generated" "--keep-generated")
    ("-n" "No flood guard" "--no-flood-guard")
    ("-f" "File list" "--file-list=" :reader magento-cache-clean--read-file-list)]
   ["Cache types"
    (magento-cache-clean--infix-cache-types)]
   ["Actions"
    ("c" "Clean" magento-cache-clean)
    ("w" magento-cache-clean-watch
     :description (lambda ()
                    (if (magento-cache-clean--transient-watching-p)
                        "Restart watch"
                      "Watch")))
    ("s" "Stop watch" magento-cache-clean-watch-stop
     :inapt-if-not magento-cache-clean--transient-watching-p)
    ("S" "Stop all watchers" magento-cache-clean-watch-stop-all
     :inapt-if-not magento-cache-clean--watch-buffers)]]
  (interactive "P")
  (transient-setup 'magento-cache-clean-transient nil nil
                   :scope (magento-cache-clean--transient-scope
                           (if choose-root
                               (magento-cache-clean--read-root)
                             (magento-cache-clean--root t)))))

(provide 'magento-cache-clean)
;;; magento-cache-clean.el ends here
