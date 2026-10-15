;;; magento-cache-clean.el --- Emacs integration for magento-cache-clean -*- lexical-binding: t; -*-

;; Copyright (C) 2026  Piotr Kwiecinski

;; Author: Piotr Kwiecinski <piotr.kwiecinski@codemanufacture.com>
;; Version: 0.1.0
;; Package-Requires: ((emacs "30.1"))
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
Magento root.

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
When nil, auto-detected by searching for app/etc/env.php."
  :type '(choice (const :tag "Auto-detect" nil)
                 (directory :tag "Directory"))
  :safe (lambda (v) (or (null v) (stringp v))))

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
defaults, reachable with \\<minibuffer-local-map>\\[next-history-element]."
  (file-name-as-directory
   (expand-file-name
    (read-directory-name "Magento root directory: " nil
                         (cons default-directory
                               (magento-cache-clean--known-roots))
                         t))))

(defun magento-cache-clean--root (&optional prompt)
  "Return the configured or detected Magento root.
The root is detected by looking for app/etc/env.php upward.  When it
can't be found, prompt for it if PROMPT is non-nil, else return nil."
  (if-let* ((dir (or magento-cache-clean-directory
                     (locate-dominating-file default-directory
                                             "app/etc/env.php"))))
      (file-name-as-directory (expand-file-name dir))
    (and prompt (magento-cache-clean--read-root))))

(defun magento-cache-clean--menu-root ()
  "Return the root chosen in `magento-cache-clean-transient', or nil.
Only non-nil while a command runs from that menu."
  (and (eq transient-current-command 'magento-cache-clean-transient)
       (transient-scope)))

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
  (let ((default-directory root))
    (if (string-search "/" name)
        (file-executable-p (expand-file-name name root))
      (executable-find name t))))

(defun magento-cache-clean--auto-detect (root)
  "Return the name of the first cache-clean executable found for ROOT.
Signal a `user-error' when there is none."
  (let ((node (magento-cache-clean--found-p "node" root))
        (missing-node nil))
    (or (seq-some (pcase-lambda (`(,name . ,needs-node))
                    (when (magento-cache-clean--found-p name root)
                      (if (and needs-node (not node))
                          (ignore (push name missing-node))
                        name)))
                  magento-cache-clean--candidates)
        (user-error "No cache-clean found (tried %s)%s"
                    (mapconcat #'car magento-cache-clean--candidates ", ")
                    (if missing-node
                        (format "; %s found but node is missing"
                                (string-join (nreverse missing-node) " and "))
                      "")))))

(defun magento-cache-clean--executable (root)
  "Return the name of the cache-clean executable for Magento ROOT.
See `magento-cache-clean-executable'.  Signal a `user-error' when
auto-detection finds none."
  (let ((exe magento-cache-clean-executable))
    (or (cond ((stringp exe) exe)
              ((and (not (eq exe 'auto)) (functionp exe))
               (funcall exe root)))
        (magento-cache-clean--auto-detect root))))

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
local."
  `(,(magento-cache-clean--program (magento-cache-clean--executable root) root)
    "-d" ,(file-local-name root) ,@args))

;;; Buffers

(defun magento-cache-clean--buffer-name (root &optional watch)
  "Return the output buffer name for Magento ROOT.
When WATCH is non-nil, return the watch buffer name."
  (format "*magento-cache-clean%s: %s*"
          (if watch "-watch" "")
          (abbreviate-file-name (directory-file-name root))))

(defun magento-cache-clean--watch-buffers ()
  "Return the watch buffers with a running watcher."
  (seq-filter (lambda (buf) (process-live-p (get-buffer-process buf)))
              (match-buffers '(derived-mode . magento-cache-clean-watch-mode))))

(defun magento-cache-clean--watch-process (&optional root)
  "Return the watch process for Magento ROOT, or nil.
ROOT defaults to the current watch buffer's root, the menu's root or
the current buffer's root, in that order."
  (if (and (not root) (derived-mode-p 'magento-cache-clean-watch-mode))
      (get-buffer-process (current-buffer))
    (when-let* ((root (or root
                          (magento-cache-clean--menu-root)
                          (magento-cache-clean--root)))
                (buf (get-buffer (magento-cache-clean--buffer-name root t))))
      (get-buffer-process buf))))

;;; Modes

(defvar magento-cache-clean-error-regexp-alist nil
  "Value of `compilation-error-regexp-alist' in `magento-cache-clean-mode'.
cache-clean output has no file:line locations by default, so this is
empty.  Add entries to make parts of the output clickable.")

(define-compilation-mode magento-cache-clean-mode "MagentoCacheClean"
  "Major mode for one-shot magento-cache-clean output."
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
  "<remap> <self-insert-command>" #'magento-cache-clean-watch-send-key)

(define-derived-mode magento-cache-clean-watch-mode comint-mode
  "MagentoCacheClean:Watch"
  "Major mode for magento-cache-clean watch buffers.
Printable keys are sent straight to the cache-clean process, which
handles them as hotkeys; see the cache-clean README for the list.
\\[revert-buffer] restarts the watcher with the same arguments.

\\{magento-cache-clean-watch-mode-map}"
  (setq-local revert-buffer-function
              (lambda (&rest _) (magento-cache-clean-watch-restart))))

(defun magento-cache-clean--watch-start (root args)
  "Start cache-clean watching Magento ROOT with ARGS and return its buffer.
The process inherits the current buffer's environment and variable
`exec-path'.  Any watcher already running for ROOT is restarted."
  (let ((env process-environment)
        (path exec-path)
        (buf (get-buffer-create (magento-cache-clean--buffer-name root t)))
        (command (magento-cache-clean--command root (cons "--watch" args))))
    (with-current-buffer buf
      (unless (derived-mode-p 'magento-cache-clean-watch-mode)
        (magento-cache-clean-watch-mode))
      ;; Set after the mode change, which kills local variables.
      (setq default-directory root)
      (setq-local process-environment env
                  exec-path path)
      (setq magento-cache-clean--watch-args args)
      (goto-char (point-max))
      (insert (propertize (concat "$ " (combine-and-quote-strings command) "\n")
                          'font-lock-face 'comint-highlight-input))
      ;; `comint-exec' deletes any process still running in BUF.
      (comint-exec buf (buffer-name buf) (car command) nil (cdr command))
      (set-process-query-on-exit-flag (get-buffer-process buf) nil))
    buf))

;;; Commands

;;;###autoload
(defun magento-cache-clean (&optional args)
  "Run cache-clean once with command line ARGS.
Interactively, ARGS come from `magento-cache-clean-transient'."
  (interactive (list (magento-cache-clean--arguments)))
  (let* ((root (magento-cache-clean--directory))
         (name (magento-cache-clean--buffer-name root))
         (command (mapconcat #'shell-quote-argument
                             (magento-cache-clean--command root args)
                             " "))
         ;; `compilation-start' inherits this buffer's environment.
         (default-directory root))
    (compilation-start command #'magento-cache-clean-mode (lambda (_) name))))

;;;###autoload
(defun magento-cache-clean-watch (&optional args)
  "Start cache-clean in watch mode with command line ARGS.
Interactively, ARGS come from `magento-cache-clean-transient'."
  (interactive (list (magento-cache-clean--arguments)))
  (let ((root (magento-cache-clean--directory)))
    (when (and (magento-cache-clean--watch-process root)
               (not (yes-or-no-p "Watch process already running.  Restart? ")))
      (user-error "Watch process already running"))
    (let ((buf (magento-cache-clean--watch-start root args)))
      (display-buffer buf)
      buf)))

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
    (let ((watchers (mapcar (lambda (buf)
                              (cons (buffer-local-value 'default-directory buf)
                                    buf))
                            (magento-cache-clean--watch-buffers))))
      (unless watchers
        (user-error "No watch process running"))
      (cdr (assoc (completing-read "Stop watcher: " watchers nil t nil nil
                                   (caar watchers))
                  watchers)))))

;;;###autoload
(defun magento-cache-clean-watch-stop (&optional buffer)
  "Stop the watcher running in watch BUFFER.
BUFFER defaults to the watcher of the current Magento root, or else
one of the running watchers read in the minibuffer."
  (interactive)
  (setq buffer (or buffer (magento-cache-clean--read-watch-buffer)))
  (interrupt-process (get-buffer-process buffer))
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

(transient-define-infix magento-cache-clean--infix-cache-types ()
  :class 'transient-option
  :key "-t"
  :description "Cache types"
  :argument "--types="
  :multi-value 'repeat)

(defun magento-cache-clean--transient-executable (root)
  "Describe the cache-clean executable for Magento ROOT in the menu.
It is resolved in the buffer the menu was opened from."
  (transient-with-shadowed-buffer
    (condition-case err
        (concat (magento-cache-clean--executable root)
                (and (eq magento-cache-clean-executable 'auto) " (auto)"))
      (user-error "not found")
      (error (error-message-string err)))))

(defun magento-cache-clean--transient-description ()
  "Return the heading of `magento-cache-clean-transient'."
  (let ((root (transient-scope)))
    (format "Magento dir: %s   cache-clean: %s"
            (abbreviate-file-name root)
            (magento-cache-clean--transient-executable root))))

(defun magento-cache-clean--transient-watching-p ()
  "Return non-nil if the menu's Magento root has a running watcher."
  (magento-cache-clean--watch-process (transient-scope)))

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
    ("-f" "File list" "--file-list=" :reader transient-read-existing-file)]
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
                   :scope (if choose-root
                              (magento-cache-clean--read-root)
                            (magento-cache-clean--root t))))

(provide 'magento-cache-clean)
;;; magento-cache-clean.el ends here
