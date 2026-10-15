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
;;   (magento-cache-clean \\='("--keep-generated" "layout"))

;;; Code:

(require 'ansi-color)
(require 'comint)
(require 'compile)
(require 'transient)

;;; Customization

(defgroup magento-cache-clean nil
  "Interface to magento-cache-clean."
  :group 'tools
  :prefix "magento-cache-clean-")

;; Not marked safe: a .dir-locals.el could otherwise run any program.
(defcustom magento-cache-clean-executable "cache-clean"
  "Path to the cache-clean executable."
  :type 'string)

(defcustom magento-cache-clean-directory nil
  "Magento root directory.
When nil, auto-detected by searching for app/etc/env.php."
  :type '(choice (const :tag "Auto-detect" nil)
                 (directory :tag "Directory"))
  :safe (lambda (v) (or (null v) (stringp v))))

;;; Directory detection

(defun magento-cache-clean--detect-directory ()
  "Detect Magento root by looking for app/etc/env.php upward."
  (when-let* ((dir (locate-dominating-file default-directory "app/etc/env.php")))
    (file-name-as-directory (expand-file-name dir))))

(defun magento-cache-clean--root ()
  "Return the configured or detected Magento root, or nil."
  (if magento-cache-clean-directory
      (file-name-as-directory (expand-file-name magento-cache-clean-directory))
    (magento-cache-clean--detect-directory)))

(defun magento-cache-clean--directory ()
  "Return the Magento root directory, prompting if it can't be found."
  (or (magento-cache-clean--root)
      (file-name-as-directory
       (expand-file-name
        (read-directory-name "Magento root directory: " nil nil t)))))

;;; Arguments

(defun magento-cache-clean--transient-to-cli (args)
  "Convert transient ARGS to cache-clean command line arguments.
Switches are passed through and each --types=TYPE becomes a positional
TYPE after them."
  (let (flags types)
    (dolist (arg args)
      (if (string-prefix-p "--types=" arg)
          (push (string-remove-prefix "--types=" arg) types)
        (push arg flags)))
    (nconc (nreverse flags) (nreverse types))))

(defun magento-cache-clean--arguments ()
  "Return the cache-clean arguments set in `magento-cache-clean-transient'.
Outside the transient this is its saved or default value."
  (magento-cache-clean--transient-to-cli
   (transient-args 'magento-cache-clean-transient)))

;;; Buffers

(defun magento-cache-clean--buffer-name (root &optional watch)
  "Return the output buffer name for Magento ROOT.
When WATCH is non-nil, return the watch buffer name."
  (format "*magento-cache-clean%s: %s*"
          (if watch "-watch" "")
          (abbreviate-file-name (directory-file-name root))))

(defun magento-cache-clean--watch-process ()
  "Return the watch process for the current buffer's Magento root, or nil."
  (if (derived-mode-p 'magento-cache-clean-watch-mode)
      (get-buffer-process (current-buffer))
    (when-let* ((root (magento-cache-clean--root))
                (buf (get-buffer (magento-cache-clean--buffer-name root t))))
      (get-buffer-process buf))))

;;; Modes

(define-compilation-mode magento-cache-clean-mode "MagentoCacheClean"
  "Major mode for one-shot magento-cache-clean output."
  ;; Output has no file:line locations worth jumping to.
  (setq-local compilation-error-regexp-alist nil)
  (add-hook 'compilation-filter-hook #'ansi-color-compilation-filter nil t))

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

(define-derived-mode magento-cache-clean-watch-mode comint-mode "MagentoCacheClean"
  "Major mode for magento-cache-clean watch buffers.
Printable keys are sent straight to the cache-clean process, which
handles them as hotkeys; see the cache-clean README for the list.

\\{magento-cache-clean-watch-mode-map}")

;;; Commands

;;;###autoload
(defun magento-cache-clean (&optional args)
  "Run cache-clean once with command line ARGS.
Interactively, ARGS come from `magento-cache-clean-transient'."
  (interactive (list (magento-cache-clean--arguments)))
  (let* ((root (magento-cache-clean--directory))
         (name (magento-cache-clean--buffer-name root))
         (command (mapconcat #'shell-quote-argument
                             `(,magento-cache-clean-executable "-d" ,root ,@args)
                             " "))
         ;; `compilation-start' inherits this buffer's environment.
         (default-directory root))
    (compilation-start command #'magento-cache-clean-mode (lambda (_) name))))

;;;###autoload
(defun magento-cache-clean-watch (&optional args)
  "Start cache-clean in watch mode with command line ARGS.
Interactively, ARGS come from `magento-cache-clean-transient'."
  (interactive (list (magento-cache-clean--arguments)))
  (let* ((root (magento-cache-clean--directory))
         (env process-environment)
         (path exec-path)
         (buf (get-buffer-create (magento-cache-clean--buffer-name root t))))
    (when (and (get-buffer-process buf)
               (not (yes-or-no-p "Watch process already running.  Restart? ")))
      (user-error "Watch process already running"))
    (with-current-buffer buf
      (unless (derived-mode-p 'magento-cache-clean-watch-mode)
        (magento-cache-clean-watch-mode))
      ;; Set after the mode change, which kills local variables.
      (setq default-directory root)
      (setq-local process-environment env
                  exec-path path)
      ;; `comint-exec' deletes any process still running in BUF.
      (comint-exec buf "magento-cache-clean" magento-cache-clean-executable
                   nil `("-d" ,root "--watch" ,@args))
      (set-process-query-on-exit-flag (get-buffer-process buf) nil))
    (display-buffer buf)
    buf))

;;;###autoload
(defun magento-cache-clean-watch-stop ()
  "Stop the running watch process."
  (interactive)
  (if-let* ((proc (magento-cache-clean--watch-process)))
      (progn
        (interrupt-process proc)
        (message "magento-cache-clean watch stopped"))
    (user-error "No watch process running")))

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

;;;###autoload (autoload 'magento-cache-clean-transient "magento-cache-clean" nil t)
(transient-define-prefix magento-cache-clean-transient ()
  "Transient interface for magento-cache-clean."
  [:description
   (lambda ()
     (format "Magento dir: %s"
             (or (magento-cache-clean--root) "not detected")))
   ["Options"
    (magento-cache-clean--infix-verbosity)
    ("-k" "Keep generated" "--keep-generated")
    ("-n" "No flood guard" "--no-flood-guard")]
   ["Cache types"
    (magento-cache-clean--infix-cache-types)]
   ["Actions"
    ("c" "Clean" magento-cache-clean)
    ("w" "Watch" magento-cache-clean-watch)
    ("s" "Stop watch" magento-cache-clean-watch-stop)]])

(provide 'magento-cache-clean)
;;; magento-cache-clean.el ends here
