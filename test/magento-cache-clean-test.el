;;; magento-cache-clean-test.el --- Tests for magento-cache-clean -*- lexical-binding: t; -*-

;;; Commentary:

;; ERT tests for magento-cache-clean.

;;; Code:

(require 'ert)
(require 'magento-cache-clean)

;;; Helpers

(defmacro magento-cache-clean-test--with-root (var &rest body)
  "Bind VAR to a temporary Magento root while running BODY.
The root has an empty app/etc/env.php and is deleted afterwards."
  (declare (indent 1))
  `(let ((,var (file-name-as-directory (make-temp-file "magento-test" t))))
     (unwind-protect
         (progn
           (make-directory (expand-file-name "app/etc" ,var) t)
           (write-region "" nil (expand-file-name "app/etc/env.php" ,var))
           ,@body)
       (delete-directory ,var t))))

(defun magento-cache-clean-test--fake-executable (dir script)
  "Write SCRIPT as an executable named fake-cache-clean in DIR.
Return its absolute path."
  (let ((file (expand-file-name "fake-cache-clean" dir)))
    (write-region (concat "#!/bin/sh\n" script "\n") nil file)
    (set-file-modes file #o755)
    file))

(defun magento-cache-clean-test--wait (buffer)
  "Wait up to 5 seconds for BUFFER's process to exit."
  (let ((deadline (+ (float-time) 5)))
    (while (and (process-live-p (get-buffer-process buffer))
                (< (float-time) deadline))
      (accept-process-output nil 0.05))
    ;; Let the sentinel and filter flush the remaining output.
    (accept-process-output nil 0.1)))

(defun magento-cache-clean-test--wait-for-output (buffer regexp)
  "Wait up to 5 seconds for REGEXP to appear in BUFFER."
  (let ((deadline (+ (float-time) 5)))
    (while (and (not (with-current-buffer buffer
                       (save-excursion
                         (goto-char (point-min))
                         (re-search-forward regexp nil t))))
                (< (float-time) deadline))
      (accept-process-output nil 0.05))))

;;; Transient argument conversion

(ert-deftest magento-cache-clean-test-transient-to-cli-empty ()
  "No transient args give no CLI args."
  (should (equal nil (magento-cache-clean--transient-to-cli nil))))

(ert-deftest magento-cache-clean-test-transient-to-cli-switches ()
  "Switches pass through unchanged and in order."
  (should (equal '("--debug" "--keep-generated" "--no-flood-guard")
                 (magento-cache-clean--transient-to-cli
                  '("--debug" "--keep-generated" "--no-flood-guard")))))

(ert-deftest magento-cache-clean-test-transient-to-cli-types-after-flags ()
  "Cache types become positional args after the switches."
  (should (equal '("--verbose" "--keep-generated" "config" "layout")
                 (magento-cache-clean--transient-to-cli
                  '("--verbose" "--types=config" "--keep-generated"
                    "--types=layout")))))

(ert-deftest magento-cache-clean-test-cache-types-infix-keeps-all-types ()
  "Every cache type selected in the transient reaches the CLI."
  (let ((obj (clone (get 'magento-cache-clean--infix-cache-types
                         'transient--suffix))))
    (oset obj value '("config" "layout"))
    (should (equal '("config" "layout")
                   (magento-cache-clean--transient-to-cli
                    (ensure-list (transient-infix-value obj)))))))

;;; Directory detection

(ert-deftest magento-cache-clean-test-detect-directory-found ()
  "Detect directory when app/etc/env.php exists."
  (magento-cache-clean-test--with-root root
    (let ((default-directory root)
          (magento-cache-clean-directory nil))
      (should (equal root (magento-cache-clean--root))))))

(ert-deftest magento-cache-clean-test-detect-directory-from-subdirectory ()
  "Detect directory from a subdirectory of the Magento root."
  (magento-cache-clean-test--with-root root
    (let ((sub (expand-file-name "app/code/Vendor/Module/" root))
          (magento-cache-clean-directory nil))
      (make-directory sub t)
      (let ((default-directory sub))
        (should (equal root (magento-cache-clean--root)))))))

(ert-deftest magento-cache-clean-test-detect-directory-not-found ()
  "Return nil when not in a Magento project and not prompting."
  (let ((temp-dir (make-temp-file "not-magento" t)))
    (unwind-protect
        (let ((default-directory (file-name-as-directory temp-dir))
              (magento-cache-clean-directory nil))
          (cl-letf (((symbol-function 'read-directory-name)
                     (lambda (&rest _) (error "Unexpected prompt"))))
            (should-not (magento-cache-clean--root))))
      (delete-directory temp-dir t))))

(ert-deftest magento-cache-clean-test-directory-uses-custom ()
  "Use custom directory when set."
  (let ((temp-dir (make-temp-file "magento-test" t)))
    (unwind-protect
        (let ((magento-cache-clean-directory temp-dir))
          (should (equal (file-name-as-directory (expand-file-name temp-dir))
                         (magento-cache-clean--directory))))
      (delete-directory temp-dir t))))

(ert-deftest magento-cache-clean-test-directory-prompt-is-expanded ()
  "A prompted directory is expanded, since the watch process gets no shell."
  (let ((temp-dir (make-temp-file "not-magento" t)))
    (unwind-protect
        (let ((default-directory (file-name-as-directory temp-dir))
              (magento-cache-clean-directory nil))
          (cl-letf (((symbol-function 'read-directory-name)
                     (lambda (&rest _) "~/magento")))
            (should (equal (file-name-as-directory (expand-file-name "~/magento"))
                           (magento-cache-clean--directory)))))
      (delete-directory temp-dir t))))

;;; Buffers

(ert-deftest magento-cache-clean-test-buffer-name-per-root ()
  "Buffer names identify the Magento root, so same-named projects don't clash."
  (should (equal "*magento-cache-clean: /a/src*"
                 (magento-cache-clean--buffer-name "/a/src/")))
  (should (equal "*magento-cache-clean-watch: /b/src*"
                 (magento-cache-clean--buffer-name "/b/src/" t))))

(ert-deftest magento-cache-clean-test-watch-not-running ()
  "No watch process when no watch buffer exists."
  (magento-cache-clean-test--with-root root
    (let ((default-directory root)
          (magento-cache-clean-directory nil))
      (should-not (magento-cache-clean--watch-process)))))

;;; Watch mode keys

(ert-deftest magento-cache-clean-test-watch-forwards-printable-keys ()
  "Printable keys go to the process; Emacs and comint keys keep working."
  (with-temp-buffer
    (magento-cache-clean-watch-mode)
    (dolist (key '("c" "G" "?" "1"))
      (should (eq 'magento-cache-clean-watch-send-key (key-binding (kbd key)))))
    (should (eq 'comint-interrupt-subjob (key-binding (kbd "C-c C-c"))))
    (should (eq 'find-file (key-binding (kbd "C-x C-f"))))))

;;; Processes

(ert-deftest magento-cache-clean-test-one-shot-runs-with-caller-env ()
  "One-shot runs in the root, with the caller's environment and the args."
  (magento-cache-clean-test--with-root root
    (let* ((magento-cache-clean-executable
            (magento-cache-clean-test--fake-executable
             root "echo \"FOO=$FOO PWD=$PWD ARGS=$*\""))
           (buf nil))
      (unwind-protect
          (with-temp-buffer
            (setq default-directory (expand-file-name "app/" root))
            (setq-local process-environment
                        (cons "FOO=from-caller" process-environment))
            (setq buf (magento-cache-clean '("--keep-generated" "config")))
            (magento-cache-clean-test--wait buf)
            (with-current-buffer buf
              (should (derived-mode-p 'magento-cache-clean-mode))
              (should (string-match-p
                       (regexp-quote
                        (format "FOO=from-caller PWD=%s ARGS=-d %s --keep-generated config"
                                (directory-file-name root) root))
                       (buffer-string)))))
        (when (buffer-live-p buf) (kill-buffer buf))))))

(ert-deftest magento-cache-clean-test-watch-runs-with-caller-env ()
  "Watch gets the caller's environment and exec-path on first start."
  (magento-cache-clean-test--with-root root
    (let* ((bin (expand-file-name "bin/" root))
           (buf nil))
      (make-directory bin)
      (magento-cache-clean-test--fake-executable
       bin "echo \"FOO=$FOO PWD=$PWD ARGS=$*\"")
      (unwind-protect
          (with-temp-buffer
            (setq default-directory root)
            (setq-local process-environment
                        (cons "FOO=from-caller" process-environment))
            ;; Only findable through the caller's buffer-local exec-path.
            (setq-local exec-path (cons bin exec-path))
            (let ((magento-cache-clean-executable "fake-cache-clean"))
              (setq buf (magento-cache-clean-watch '("config"))))
            (magento-cache-clean-test--wait buf)
            (with-current-buffer buf
              (should (derived-mode-p 'magento-cache-clean-watch-mode))
              (should (string-match-p
                       (regexp-quote
                        (format "FOO=from-caller PWD=%s ARGS=-d %s --watch config"
                                (directory-file-name root) root))
                       (buffer-string)))))
        (when (buffer-live-p buf) (kill-buffer buf))))))

(ert-deftest magento-cache-clean-test-watch-restart-starts-new-process ()
  "Restarting a running watcher leaves a new live process."
  (magento-cache-clean-test--with-root root
    (let* ((magento-cache-clean-executable
            (magento-cache-clean-test--fake-executable
             root "echo started; exec sleep 30"))
           (default-directory root)
           (magento-cache-clean-directory nil)
           (buf (magento-cache-clean-watch)))
      (unwind-protect
          (let ((first (get-buffer-process buf)))
            (cl-letf (((symbol-function 'yes-or-no-p) #'always))
              (magento-cache-clean-watch))
            (let ((second (get-buffer-process buf)))
              (should second)
              (should-not (eq first second))
              (accept-process-output nil 0.2)
              (should (process-live-p second))
              (should-not (process-live-p first))))
        (when (buffer-live-p buf) (kill-buffer buf))))))

(ert-deftest magento-cache-clean-test-watch-declined-restart-keeps-process ()
  "Declining the restart prompt keeps the running watcher."
  (magento-cache-clean-test--with-root root
    (let* ((magento-cache-clean-executable
            (magento-cache-clean-test--fake-executable root "exec sleep 30"))
           (default-directory root)
           (magento-cache-clean-directory nil)
           (buf (magento-cache-clean-watch)))
      (unwind-protect
          (let ((proc (get-buffer-process buf)))
            (cl-letf (((symbol-function 'yes-or-no-p) #'ignore))
              (should-error (magento-cache-clean-watch) :type 'user-error))
            (should (eq proc (get-buffer-process buf)))
            (should (process-live-p proc)))
        (when (buffer-live-p buf) (kill-buffer buf))))))

(ert-deftest magento-cache-clean-test-watch-send-key ()
  "A forwarded key reaches the process unchanged."
  (magento-cache-clean-test--with-root root
    (let* ((magento-cache-clean-executable
            (magento-cache-clean-test--fake-executable
             root "stty raw -echo; key=$(dd bs=1 count=1 2>/dev/null); echo \"got:$key\""))
           (default-directory root)
           (magento-cache-clean-directory nil)
           (buf (magento-cache-clean-watch)))
      (unwind-protect
          (with-current-buffer buf
            (accept-process-output nil 0.3)
            (let ((last-command-event ?G))
              (call-interactively #'magento-cache-clean-watch-send-key))
            (magento-cache-clean-test--wait-for-output buf "got:G")
            (should (string-match-p "got:G" (buffer-string))))
        (when (buffer-live-p buf) (kill-buffer buf))))))

(ert-deftest magento-cache-clean-test-watch-stop ()
  "Stopping interrupts the watcher for the current root."
  (magento-cache-clean-test--with-root root
    (let* ((magento-cache-clean-executable
            (magento-cache-clean-test--fake-executable root "exec sleep 30"))
           (default-directory root)
           (magento-cache-clean-directory nil)
           (buf (magento-cache-clean-watch)))
      (unwind-protect
          (progn
            (should (eq (get-buffer-process buf)
                        (magento-cache-clean--watch-process)))
            (magento-cache-clean-watch-stop)
            (magento-cache-clean-test--wait buf)
            (should-not (magento-cache-clean--watch-process)))
        (when (buffer-live-p buf) (kill-buffer buf))))))

(ert-deftest magento-cache-clean-test-one-shot-renders-ansi-colors ()
  "One-shot output has ANSI escapes turned into faces."
  (magento-cache-clean-test--with-root root
    (let* ((magento-cache-clean-executable
            (magento-cache-clean-test--fake-executable
             root "printf '\\033[33mwarn\\033[0m\\n'"))
           (default-directory root)
           (magento-cache-clean-directory nil)
           (buf (magento-cache-clean)))
      (unwind-protect
          (with-current-buffer buf
            (magento-cache-clean-test--wait buf)
            (should-not (string-search "\e[" (buffer-string)))
            (goto-char (point-min))
            (should (search-forward "warn" nil t)))
        (when (buffer-live-p buf) (kill-buffer buf))))))

;;; Safe local variables

(ert-deftest magento-cache-clean-test-executable-not-safe-local ()
  "Executable is never safe as a dir-local, since it is run as is."
  (should-not (safe-local-variable-p 'magento-cache-clean-executable "cache-clean"))
  (should-not (safe-local-variable-p 'magento-cache-clean-executable "/tmp/evil")))

(ert-deftest magento-cache-clean-test-directory-safe-local ()
  "Directory is safe as a dir-local when string or nil."
  (should (safe-local-variable-p 'magento-cache-clean-directory nil))
  (should (safe-local-variable-p 'magento-cache-clean-directory "/path/to/magento"))
  (should-not (safe-local-variable-p 'magento-cache-clean-directory 42)))

(provide 'magento-cache-clean-test)
;;; magento-cache-clean-test.el ends here
