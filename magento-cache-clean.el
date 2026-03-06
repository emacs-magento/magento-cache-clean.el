;;; magento-cache-clean.el --- Emacs integration for magento-cache-clean -*- lexical-binding: t; -*-

;; Copyright (C) 2026  Piotr

;; Author: Piotr
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

;;; Code:

(require 'comint)
(require 'project)
(require 'transient)

;;; Customization

(defgroup magento-cache-clean nil
  "Interface to magento-cache-clean."
  :group 'tools
  :prefix "magento-cache-clean-")

(defcustom magento-cache-clean-executable "cache-clean"
  "Path to the cache-clean executable."
  :type 'string
  :safe #'stringp)

(defcustom magento-cache-clean-directory nil
  "Magento root directory.
When nil, auto-detected by searching for app/etc/env.php."
  :type '(choice (const :tag "Auto-detect" nil)
                 (directory :tag "Directory"))
  :safe (lambda (v) (or (null v) (stringp v))))

;;; Cache types

(defconst magento-cache-clean-types
  '("config"
    "layout"
    "block_html"
    "collections"
    "reflection"
    "db_ddl"
    "compiled_config"
    "eav"
    "customer_notification"
    "config_integration"
    "config_integration_api"
    "config_webservice"
    "full_page"
    "translate")
  "List of Magento 2 cache types.")

;;; Directory detection

(defun magento-cache-clean--detect-directory ()
  "Detect Magento root by looking for app/etc/env.php upward."
  (when-let* ((dir (locate-dominating-file default-directory "app/etc/env.php")))
    (file-name-as-directory (expand-file-name dir))))

(defun magento-cache-clean--directory ()
  "Return the Magento root directory.
Uses `magento-cache-clean-directory' if set, otherwise auto-detects.
Prompts the user as a last resort."
  (or (and magento-cache-clean-directory
           (file-name-as-directory (expand-file-name magento-cache-clean-directory)))
      (magento-cache-clean--detect-directory)
      (file-name-as-directory
       (read-directory-name "Magento root directory: "))))

;;; Command building

(defun magento-cache-clean--build-args (verbosity keep-generated no-flood-guard watch cache-types)
  "Build command line arguments.
VERBOSITY is one of nil, \"verbose\", \"debug\", \"silent\".
KEEP-GENERATED, NO-FLOOD-GUARD, and WATCH are booleans.
CACHE-TYPES is a list of cache type strings."
  (let (args)
    (pcase verbosity
      ("verbose" (push "--verbose" args))
      ("debug"   (push "--debug" args))
      ("silent"  (push "--silent" args)))
    (when keep-generated
      (push "--keep-generated" args))
    (when no-flood-guard
      (push "--no-flood-guard" args))
    (when watch
      (push "--watch" args))
    (append (nreverse args) cache-types)))

;;; Buffer management

(defun magento-cache-clean--project-name ()
  "Return the current project name, or nil."
  (when-let* ((proj (project-current)))
    (project-name proj)))

(defun magento-cache-clean--buffer-name ()
  "Return the buffer name for one-shot mode."
  (if-let* ((name (magento-cache-clean--project-name)))
      (format "*magento-cache-clean[%s]*" name)
    "*magento-cache-clean*"))

(defun magento-cache-clean--watch-buffer-name ()
  "Return the buffer name for watch mode."
  (if-let* ((name (magento-cache-clean--project-name)))
      (format "*magento-cache-clean-watch[%s]*" name)
    "*magento-cache-clean-watch*"))

(defun magento-cache-clean--watch-running-p ()
  "Return non-nil if a watch process is running for the current project."
  (when-let* ((buf (get-buffer (magento-cache-clean--watch-buffer-name))))
    (get-buffer-process buf)))

;;; Watch mode

(defun magento-cache-clean-watch-send-key ()
  "Send the last key pressed directly to the watch process."
  (interactive)
  (if-let* ((proc (get-buffer-process (current-buffer))))
      (process-send-string proc (string last-command-event))
    (user-error "No watch process running")))

(defvar-keymap magento-cache-clean-watch-mode-map
  :doc "Keymap for `magento-cache-clean-watch-mode'."
  :parent comint-mode-map
  "c" #'magento-cache-clean-watch-send-key
  "b" #'magento-cache-clean-watch-send-key
  "l" #'magento-cache-clean-watch-send-key
  "t" #'magento-cache-clean-watch-send-key
  "f" #'magento-cache-clean-watch-send-key
  "v" #'magento-cache-clean-watch-send-key
  "m" #'magento-cache-clean-watch-send-key
  "a" #'magento-cache-clean-watch-send-key
  "G" #'magento-cache-clean-watch-send-key
  "I" #'magento-cache-clean-watch-send-key
  "F" #'magento-cache-clean-watch-send-key
  "A" #'magento-cache-clean-watch-send-key)

(define-derived-mode magento-cache-clean-watch-mode comint-mode "MagentoCacheClean"
  "Major mode for magento-cache-clean watch buffers.
Single-key hotkeys are sent directly to the process:

  \\[magento-cache-clean-watch-send-key] c  config
  \\[magento-cache-clean-watch-send-key] b  block_html
  \\[magento-cache-clean-watch-send-key] l  layout
  \\[magento-cache-clean-watch-send-key] t  translate
  \\[magento-cache-clean-watch-send-key] f  full_page
  \\[magento-cache-clean-watch-send-key] v  view
  \\[magento-cache-clean-watch-send-key] m  misc
  \\[magento-cache-clean-watch-send-key] a  all
  \\[magento-cache-clean-watch-send-key] G  clean generated
  \\[magento-cache-clean-watch-send-key] I  clean integration test sandboxes
  \\[magento-cache-clean-watch-send-key] F  clean frontend static content
  \\[magento-cache-clean-watch-send-key] A  clean adminhtml static content")

;;; Interactive commands

;;;###autoload
(defun magento-cache-clean (&optional verbosity keep-generated no-flood-guard cache-types)
  "Run cache-clean once.
VERBOSITY is one of nil, \"verbose\", \"debug\", \"silent\".
KEEP-GENERATED and NO-FLOOD-GUARD are booleans.
CACHE-TYPES is a list of cache type strings."
  (interactive)
  (let* ((directory (magento-cache-clean--directory))
         (env process-environment)
         (buf-name (magento-cache-clean--buffer-name))
         (args (magento-cache-clean--build-args
                verbosity keep-generated no-flood-guard nil cache-types))
         (full-args (append (list "-d" directory) args))
         (command (mapconcat #'shell-quote-argument
                             (cons magento-cache-clean-executable full-args)
                             " ")))
    (with-current-buffer (get-buffer-create buf-name)
      (let ((inhibit-read-only t))
        (erase-buffer))
      (setq-local process-environment env)
      (compilation-start command nil (lambda (_) buf-name)))))

;;;###autoload
(defun magento-cache-clean-watch (&optional verbosity keep-generated no-flood-guard cache-types)
  "Start cache-clean in watch mode using comint.
VERBOSITY is one of nil, \"verbose\", \"debug\", \"silent\".
KEEP-GENERATED and NO-FLOOD-GUARD are booleans.
CACHE-TYPES is a list of cache type strings."
  (interactive)
  (when (magento-cache-clean--watch-running-p)
    (if (yes-or-no-p "Watch process already running.  Restart? ")
        (magento-cache-clean-watch-stop)
      (user-error "Watch process already running")))
  (let* ((directory (magento-cache-clean--directory))
         (env process-environment)
         (watch-buf-name (magento-cache-clean--watch-buffer-name))
         (args (magento-cache-clean--build-args
                verbosity keep-generated no-flood-guard t cache-types))
         (full-args (append (list "-d" directory) args))
         (buf (with-current-buffer (get-buffer-create watch-buf-name)
                (setq-local process-environment env)
                (apply #'make-comint-in-buffer
                       "magento-cache-clean"
                       (current-buffer)
                       magento-cache-clean-executable
                       nil
                       full-args))))
    (with-current-buffer buf
      (magento-cache-clean-watch-mode)
      (setq-local default-directory directory))
    (display-buffer buf)
    buf))

;;;###autoload
(defun magento-cache-clean-watch-stop ()
  "Stop the running watch process."
  (interactive)
  (if-let* ((proc (magento-cache-clean--watch-running-p)))
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
  :multi-value 'repeat
  :choices magento-cache-clean-types)

(defun magento-cache-clean--transient-parse-args (args)
  "Parse transient ARGS into components.
Returns a plist with :verbosity, :keep-generated, :no-flood-guard, :cache-types."
  (let (verbosity keep-generated no-flood-guard cache-types)
    (dolist (arg args)
      (cond
       ((member arg '("--verbose" "--debug" "--silent"))
        (setq verbosity (substring arg 2)))
       ((equal arg "--keep-generated")
        (setq keep-generated t))
       ((equal arg "--no-flood-guard")
        (setq no-flood-guard t))
       ((string-prefix-p "--types=" arg)
        (setq cache-types (split-string (substring arg 8) ",")))))
    (list :verbosity verbosity
          :keep-generated keep-generated
          :no-flood-guard no-flood-guard
          :cache-types cache-types)))

(defun magento-cache-clean--do-clean (&optional args)
  "Run one-shot clean with transient ARGS."
  (interactive (list (transient-args 'magento-cache-clean-transient)))
  (let ((parsed (magento-cache-clean--transient-parse-args args)))
    (magento-cache-clean
     (plist-get parsed :verbosity)
     (plist-get parsed :keep-generated)
     (plist-get parsed :no-flood-guard)
     (plist-get parsed :cache-types))))

(defun magento-cache-clean--do-watch (&optional args)
  "Start watch mode with transient ARGS."
  (interactive (list (transient-args 'magento-cache-clean-transient)))
  (let ((parsed (magento-cache-clean--transient-parse-args args)))
    (magento-cache-clean-watch
     (plist-get parsed :verbosity)
     (plist-get parsed :keep-generated)
     (plist-get parsed :no-flood-guard)
     (plist-get parsed :cache-types))))

;;;###autoload (autoload 'magento-cache-clean-transient "magento-cache-clean" nil t)
(transient-define-prefix magento-cache-clean-transient ()
  "Transient interface for magento-cache-clean."
  [:description
   (lambda ()
     (format "Magento dir: %s"
             (or magento-cache-clean-directory
                 (magento-cache-clean--detect-directory)
                 "not detected")))
   ["Options"
    (magento-cache-clean--infix-verbosity)
    ("-k" "Keep generated" "--keep-generated")
    ("-n" "No flood guard" "--no-flood-guard")]
   ["Cache types"
    (magento-cache-clean--infix-cache-types)]
   ["Actions"
    ("c" "Clean" magento-cache-clean--do-clean)
    ("w" "Watch" magento-cache-clean--do-watch)
    ("s" "Stop watch" magento-cache-clean-watch-stop)]])

(provide 'magento-cache-clean)
;;; magento-cache-clean.el ends here
