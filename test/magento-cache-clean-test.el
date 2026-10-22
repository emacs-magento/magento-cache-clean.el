;;; magento-cache-clean-test.el --- Tests for magento-cache-clean -*- lexical-binding: t; -*-

;;; Commentary:

;; ERT tests for magento-cache-clean.

;;; Code:

(require 'cl-lib)
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

(defun magento-cache-clean-test--wait-until (predicate)
  "Wait up to 5 seconds for PREDICATE to return non-nil."
  (let ((deadline (+ (float-time) 5)))
    (while (and (not (funcall predicate))
                (< (float-time) deadline))
      (accept-process-output nil 0.05))))

(defun magento-cache-clean-test--wait (buffer)
  "Wait up to 5 seconds for BUFFER's process to exit."
  (magento-cache-clean-test--wait-until
   (lambda () (not (process-live-p (get-buffer-process buffer)))))
  ;; Let the sentinel and filter flush the remaining output.
  (accept-process-output nil 0.1))

(defun magento-cache-clean-test--wait-for-output (buffer regexp)
  "Wait up to 5 seconds for REGEXP to appear in BUFFER."
  (magento-cache-clean-test--wait-until
   (lambda ()
     (with-current-buffer buffer
       (save-excursion
         (goto-char (point-min))
         (re-search-forward regexp nil t))))))

;;; Transient argument conversion

(ert-deftest magento-cache-clean-test-transient-to-cli-empty ()
  "No transient args give no CLI args."
  (should (equal nil (magento-cache-clean--transient-to-cli nil))))

(ert-deftest magento-cache-clean-test-transient-to-cli-switches ()
  "Switches pass through unchanged and in order."
  (should (equal '("--debug" "--keep-generated" "--no-flood-guard")
                 (magento-cache-clean--transient-to-cli
                  '("--debug" "--keep-generated" "--no-flood-guard")))))

(ert-deftest magento-cache-clean-test-transient-to-cli-types-positional ()
  "Cache types become positional args; cache-clean takes them anywhere."
  (should (equal '("--verbose" "config" "--keep-generated" "layout")
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

(ert-deftest magento-cache-clean-test-transient-to-cli-file-list ()
  "The file list becomes two args, with the file's local name."
  (should (equal '("--file-list" "/tmp/files.txt")
                 (magento-cache-clean--transient-to-cli
                  '("--file-list=/tmp/files.txt"))))
  (should (equal '("--file-list" "/var/www/files.txt")
                 (magento-cache-clean--transient-to-cli
                  '("--file-list=/ssh:host:/var/www/files.txt")))))

;;; Command line

(ert-deftest magento-cache-clean-test-command ()
  "The command line has the executable, the root and the args."
  (let ((magento-cache-clean-executable "cache-clean"))
    (should (equal '("cache-clean" "-d" "/a/src/" "config")
                   (magento-cache-clean--command "/a/src/" '("config"))))))

(ert-deftest magento-cache-clean-test-command-remote-root ()
  "A remote root is passed to cache-clean as its local name."
  (let ((magento-cache-clean-executable "cache-clean"))
    (should (equal '("cache-clean" "-d" "/var/www/")
                   (magento-cache-clean--command "/ssh:host:/var/www/" nil)))))

(ert-deftest magento-cache-clean-test-command-relative-executable ()
  "A relative executable with a slash is relative to the root."
  (let ((magento-cache-clean-executable "vendor/bin/cache-clean.js"))
    (should (equal '("/a/src/vendor/bin/cache-clean.js" "-d" "/a/src/")
                   (magento-cache-clean--command "/a/src/" nil)))
    (should (equal '("/var/www/vendor/bin/cache-clean.js" "-d" "/var/www/")
                   (magento-cache-clean--command "/ssh:host:/var/www/" nil)))))

(ert-deftest magento-cache-clean-test-watch-relative-executable ()
  "Watch finds a relative executable from outside the root."
  (magento-cache-clean-test--with-root root
    (make-directory (expand-file-name "vendor/bin" root) t)
    (magento-cache-clean-test--fake-executable
     (expand-file-name "vendor/bin" root) "echo relative-ok")
    (let* ((magento-cache-clean-executable "vendor/bin/fake-cache-clean")
           (magento-cache-clean-directory root)
           (default-directory temporary-file-directory)
           (buf (magento-cache-clean-watch)))
      (unwind-protect
          (progn
            (magento-cache-clean-test--wait-for-output buf "relative-ok")
            (should (string-search "relative-ok"
                                   (with-current-buffer buf (buffer-string)))))
        (when (buffer-live-p buf) (kill-buffer buf))))))

;;; Executable resolution

(defmacro magento-cache-clean-test--with-programs (root programs vendor &rest body)
  "Run BODY with only PROGRAMS on `exec-path' for Magento ROOT.
PROGRAMS are names of fake executables.  When VENDOR is non-nil, ROOT
also gets vendor/bin/cache-clean.js."
  (declare (indent 3))
  `(let ((bin (expand-file-name "fake-bin/" ,root)))
     (make-directory bin t)
     (dolist (program ,programs)
       (let ((file (expand-file-name program bin)))
         (write-region "#!/bin/sh\n" nil file)
         (set-file-modes file #o755)))
     (when ,vendor
       (let ((dir (expand-file-name "vendor/bin/" ,root)))
         (make-directory dir t)
         (write-region "#!/bin/sh\n" nil (expand-file-name "cache-clean.js" dir))
         (set-file-modes (expand-file-name "cache-clean.js" dir) #o755)))
     (let ((exec-path (list bin))
           (magento-cache-clean-executable 'auto))
       ,@body)))

(ert-deftest magento-cache-clean-test-auto-prefers-vendor ()
  "Auto uses the root's vendor script first when node is found."
  (magento-cache-clean-test--with-root root
    (magento-cache-clean-test--with-programs root
        '("node" "cache-clean" "cache-clean.js") t
      (should (equal "vendor/bin/cache-clean.js"
                     (magento-cache-clean--executable root)))
      (should (equal (expand-file-name "vendor/bin/cache-clean.js" root)
                     (car (magento-cache-clean--command root nil)))))))

(ert-deftest magento-cache-clean-test-auto-skips-scripts-without-node ()
  "Without node, auto skips the .js scripts."
  (magento-cache-clean-test--with-root root
    (magento-cache-clean-test--with-programs root
        '("cache-clean.js" "cache-clean") t
      (should (equal "cache-clean" (magento-cache-clean--executable root))))))

(ert-deftest magento-cache-clean-test-auto-falls-back-to-js ()
  "Auto uses cache-clean.js when it is the only one found."
  (magento-cache-clean-test--with-root root
    (magento-cache-clean-test--with-programs root '("node" "cache-clean.js") nil
      (should (equal "cache-clean.js" (magento-cache-clean--executable root))))))

(ert-deftest magento-cache-clean-test-auto-none-found ()
  "Auto signals an error naming what it tried."
  (magento-cache-clean-test--with-root root
    (magento-cache-clean-test--with-programs root nil nil
      (should (equal (cadr (should-error (magento-cache-clean--executable root)
                                         :type 'user-error))
                     (concat "No cache-clean found (tried vendor/bin/cache-clean.js,"
                             " cache-clean, cache-clean.js)"))))))

(ert-deftest magento-cache-clean-test-auto-none-found-missing-node ()
  "The error says when a script was found but node wasn't."
  (magento-cache-clean-test--with-root root
    (magento-cache-clean-test--with-programs root '("cache-clean.js") t
      (should (string-suffix-p
               "; vendor/bin/cache-clean.js and cache-clean.js found but node is missing"
               (cadr (should-error (magento-cache-clean--executable root)
                                   :type 'user-error)))))))

(ert-deftest magento-cache-clean-test-executable-string-unchecked ()
  "A string is used as is, without checking that it exists."
  (magento-cache-clean-test--with-root root
    (magento-cache-clean-test--with-programs root nil nil
      (let ((magento-cache-clean-executable "no-such-cache-clean"))
        (should (equal "no-such-cache-clean"
                       (magento-cache-clean--executable root)))))))

(ert-deftest magento-cache-clean-test-executable-function ()
  "A function gets the root; nil from it means auto-detect."
  (magento-cache-clean-test--with-root root
    (magento-cache-clean-test--with-programs root '("cache-clean") nil
      (let* ((seen nil)
             (magento-cache-clean-executable
              (lambda (r) (setq seen r) "bin/my-cache-clean")))
        (should (equal "bin/my-cache-clean" (magento-cache-clean--executable root)))
        (should (equal root seen))
        (should (equal (expand-file-name "bin/my-cache-clean" root)
                       (car (magento-cache-clean--command root nil)))))
      (let ((magento-cache-clean-executable #'ignore))
        (should (equal "cache-clean" (magento-cache-clean--executable root)))))))

(ert-deftest magento-cache-clean-test-transient-executable ()
  "The menu shows the executable, marks auto and survives none found."
  (magento-cache-clean-test--with-root root
    (magento-cache-clean-test--with-programs root '("cache-clean") nil
      (should (equal "cache-clean (auto)"
                     (magento-cache-clean--transient-executable root)))
      (let ((magento-cache-clean-executable "cache-clean.js"))
        (should (equal "cache-clean.js"
                       (magento-cache-clean--transient-executable root))))
      (let ((exec-path nil))
        (should (equal "not found"
                       (magento-cache-clean--transient-executable root)))))))

(ert-deftest magento-cache-clean-test-transient-scope ()
  "The menu scope has the root and the executable, described once."
  (magento-cache-clean-test--with-root root
    (magento-cache-clean-test--with-programs root '("cache-clean") nil
      (should (equal (list :root root :executable "cache-clean (auto)")
                     (magento-cache-clean--transient-scope root))))))

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

(ert-deftest magento-cache-clean-test-directory-uses-menu-root ()
  "Commands run from the menu use the root it was opened with."
  (let ((transient-current-command 'magento-cache-clean-transient)
        (magento-cache-clean-directory "/elsewhere/"))
    (cl-letf (((symbol-function 'transient-scope) (lambda () '(:root "/menu/root/"))))
      (should (equal "/menu/root/" (magento-cache-clean--directory))))))

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

(ert-deftest magento-cache-clean-test-same-host ()
  "Roots are on the same host when both are local or on the same remote."
  (let ((default-directory "/a/"))
    (should (magento-cache-clean--same-host-p "/b/"))
    (should-not (magento-cache-clean--same-host-p "/ssh:host:/b/")))
  (let ((default-directory "/ssh:host:/a/"))
    (should (magento-cache-clean--same-host-p "/ssh:host:/b/"))
    (should-not (magento-cache-clean--same-host-p "/b/"))))

(ert-deftest magento-cache-clean-test-one-shot-other-host-skips-caller-env ()
  "A root on another host doesn't get the caller's environment."
  (let ((magento-cache-clean-executable "cache-clean")
        (local-env 'unset))
    (cl-letf (((symbol-function 'magento-cache-clean--buffer-name)
               (lambda (&rest _) "*magento-cache-clean-test*"))
              ((symbol-function 'compilation-start)
               (lambda (&rest _)
                 (setq local-env (local-variable-p 'process-environment)))))
      (with-temp-buffer
        (setq default-directory temporary-file-directory)
        (setq-local process-environment
                    (cons "FOO=from-caller" process-environment))
        (let ((magento-cache-clean-directory "/ssh:host:/var/www/"))
          (magento-cache-clean nil)
          (should-not local-env))
        (let ((magento-cache-clean-directory temporary-file-directory))
          (magento-cache-clean nil)
          (should local-env))))))

(ert-deftest magento-cache-clean-test-watch-other-host-skips-caller-env ()
  "A watcher on another host doesn't get the caller's environment."
  (magento-cache-clean-test--with-root root
    (let ((magento-cache-clean-executable
           (magento-cache-clean-test--fake-executable root "echo \"FOO=$FOO.\""))
          (magento-cache-clean-directory nil)
          (buf nil))
      (unwind-protect
          (with-temp-buffer
            (setq default-directory root)
            (setq-local process-environment
                        (cons "FOO=from-caller" process-environment))
            (cl-letf (((symbol-function 'magento-cache-clean--same-host-p)
                       #'ignore))
              (setq buf (magento-cache-clean-watch)))
            (magento-cache-clean-test--wait buf)
            (with-current-buffer buf
              (should-not (local-variable-p 'process-environment))
              (should (string-search "FOO=." (buffer-string)))))
        (when (buffer-live-p buf) (kill-buffer buf))))))

(ert-deftest magento-cache-clean-test-watch-shows-command ()
  "The watch buffer starts with the command line it runs."
  (magento-cache-clean-test--with-root root
    (let* ((magento-cache-clean-executable
            (magento-cache-clean-test--fake-executable root "exec sleep 30"))
           (default-directory root)
           (magento-cache-clean-directory nil)
           (buf (magento-cache-clean-watch '("config"))))
      (unwind-protect
          (with-current-buffer buf
            (goto-char (point-min))
            (should (looking-at-p
                     (regexp-quote
                      (format "$ %s -d %s --watch config\n"
                              magento-cache-clean-executable root)))))
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
              (magento-cache-clean-test--wait-until
               (lambda () (not (process-live-p first))))
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
             root "stty raw -echo; echo ready; key=$(dd bs=1 count=1 2>/dev/null); echo \"got:$key\""))
           (default-directory root)
           (magento-cache-clean-directory nil)
           (buf (magento-cache-clean-watch)))
      (unwind-protect
          (with-current-buffer buf
            (magento-cache-clean-test--wait-for-output buf "ready")
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
            (call-interactively #'magento-cache-clean-watch-stop)
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

(ert-deftest magento-cache-clean-test-watch-restart-keeps-args ()
  "Restarting from the watch buffer reuses the root and args."
  (magento-cache-clean-test--with-root root
    (let* ((magento-cache-clean-executable
            (magento-cache-clean-test--fake-executable
             root "echo \"ARGS=$*\"; exec sleep 30"))
           (default-directory root)
           (magento-cache-clean-directory nil)
           (buf (magento-cache-clean-watch '("config"))))
      (unwind-protect
          (with-current-buffer buf
            (let ((first (get-buffer-process buf)))
              (erase-buffer)
              (revert-buffer)
              (should-not (eq first (get-buffer-process buf)))
              (magento-cache-clean-test--wait-for-output
               buf (regexp-quote (format "ARGS=-d %s --watch config" root)))
              (should (string-search (format "ARGS=-d %s --watch config" root)
                                     (buffer-string)))))
        (when (buffer-live-p buf) (kill-buffer buf))))))

;;; Multiple instances

(defmacro magento-cache-clean-test--with-watchers (roots &rest body)
  "Start a sleeping watcher in each of ROOTS while running BODY.
ROOTS are bound to temporary Magento roots; their buffers are killed
afterwards."
  (declare (indent 1))
  (if (null roots)
      `(progn ,@body)
    `(magento-cache-clean-test--with-root ,(car roots)
       (let ((magento-cache-clean-executable
              (magento-cache-clean-test--fake-executable
               ,(car roots) "exec sleep 30"))
             (magento-cache-clean-directory nil))
         (unwind-protect
             (progn
               (let ((default-directory ,(car roots)))
                 (magento-cache-clean-watch))
               (magento-cache-clean-test--with-watchers ,(cdr roots)
                 ,@body))
           (when-let* ((buf (get-buffer (magento-cache-clean--buffer-name
                                         ,(car roots) t))))
             (kill-buffer buf)))))))

(defun magento-cache-clean-test--watch-buffer (root)
  "Return the watch buffer of ROOT."
  (get-buffer (magento-cache-clean--buffer-name root t)))

(ert-deftest magento-cache-clean-test-watchers-per-root ()
  "Each root gets its own watcher, named after its buffer."
  (magento-cache-clean-test--with-watchers (a b)
    (let ((bufs (magento-cache-clean--watch-buffers)))
      (should (memq (magento-cache-clean-test--watch-buffer a) bufs))
      (should (memq (magento-cache-clean-test--watch-buffer b) bufs))
      (dolist (root (list a b))
        (let ((buf (magento-cache-clean-test--watch-buffer root)))
          (should (equal (buffer-name buf)
                         (process-name (get-buffer-process buf)))))))))

(ert-deftest magento-cache-clean-test-stop-uses-current-root ()
  "Inside a watched root, stop acts on its watcher without asking."
  (magento-cache-clean-test--with-watchers (a b)
    (let ((default-directory a))
      (cl-letf (((symbol-function 'completing-read)
                 (lambda (&rest _) (error "Unexpected prompt"))))
        (call-interactively #'magento-cache-clean-watch-stop)))
    (magento-cache-clean-test--wait (magento-cache-clean-test--watch-buffer a))
    (should-not (get-buffer-process (magento-cache-clean-test--watch-buffer a)))
    (should (get-buffer-process (magento-cache-clean-test--watch-buffer b)))))

(ert-deftest magento-cache-clean-test-stop-elsewhere-reads-watcher ()
  "Outside a watched root, stop reads which watcher to stop."
  (magento-cache-clean-test--with-watchers (a b)
    (let ((default-directory temporary-file-directory)
          (offered nil))
      (cl-letf (((symbol-function 'completing-read)
                 (lambda (_prompt collection &rest _)
                   (setq offered (mapcar #'car collection))
                   b)))
        (call-interactively #'magento-cache-clean-watch-stop))
      (should (member a offered))
      (should (member b offered)))
    (magento-cache-clean-test--wait (magento-cache-clean-test--watch-buffer b))
    (should-not (get-buffer-process (magento-cache-clean-test--watch-buffer b)))
    (should (get-buffer-process (magento-cache-clean-test--watch-buffer a)))))

(ert-deftest magento-cache-clean-test-stop-from-menu-uses-menu-root ()
  "From the menu, stop acts on the menu's root, even in another watch buffer."
  (magento-cache-clean-test--with-watchers (a b)
    (with-current-buffer (magento-cache-clean-test--watch-buffer a)
      (let ((transient-current-command 'magento-cache-clean-transient))
        (cl-letf (((symbol-function 'transient-scope)
                   (lambda () (list :root b))))
          (call-interactively #'magento-cache-clean-watch-stop))))
    (magento-cache-clean-test--wait (magento-cache-clean-test--watch-buffer b))
    (should-not (get-buffer-process (magento-cache-clean-test--watch-buffer b)))
    (should (get-buffer-process (magento-cache-clean-test--watch-buffer a)))))

(ert-deftest magento-cache-clean-test-stop-nothing-running ()
  "Stopping with no watcher running is a user error."
  (let ((default-directory temporary-file-directory))
    (cl-letf (((symbol-function 'magento-cache-clean--watch-buffers) #'ignore))
      (should-error (call-interactively #'magento-cache-clean-watch-stop)
                    :type 'user-error))))

(ert-deftest magento-cache-clean-test-stop-without-live-process ()
  "Stopping a buffer without a live watcher is a user error."
  (with-temp-buffer
    (should-error (magento-cache-clean-watch-stop (current-buffer))
                  :type 'user-error)))

(ert-deftest magento-cache-clean-test-stop-all ()
  "Stop all interrupts every running watcher."
  (magento-cache-clean-test--with-watchers (a b)
    (magento-cache-clean-watch-stop-all)
    (dolist (root (list a b))
      (magento-cache-clean-test--wait (magento-cache-clean-test--watch-buffer root)))
    (should-not (magento-cache-clean--watch-buffers))))

(ert-deftest magento-cache-clean-test-read-root-offers-known-roots ()
  "The root prompt offers the roots that already have buffers."
  (magento-cache-clean-test--with-watchers (a)
    (let ((default-directory temporary-file-directory)
          (defaults nil))
      (cl-letf (((symbol-function 'read-directory-name)
                 (lambda (_prompt _dir default &rest _)
                   (setq defaults default)
                   a)))
        (should (equal a (magento-cache-clean--read-root))))
      (should (equal temporary-file-directory (car defaults)))
      (should (member a defaults)))))

;;; Error regexps

(ert-deftest magento-cache-clean-test-error-regexps-customizable ()
  "One-shot buffers take their error regexps from the package variable."
  (let ((magento-cache-clean-error-regexp-alist '(("^ERR \\(.+\\)$" 1))))
    (with-temp-buffer
      (magento-cache-clean-mode)
      (should (equal magento-cache-clean-error-regexp-alist
                     compilation-error-regexp-alist)))))

;;; Safe local variables

(ert-deftest magento-cache-clean-test-executable-safe-local ()
  "Only auto and the predefined executables are safe as dir-locals."
  (dolist (v '(auto "vendor/bin/cache-clean.js" "cache-clean" "cache-clean.js"))
    (should (safe-local-variable-p 'magento-cache-clean-executable v)))
  (should-not (safe-local-variable-p 'magento-cache-clean-executable "/tmp/evil"))
  (should-not (safe-local-variable-p 'magento-cache-clean-executable "bin/evil"))
  (should-not (safe-local-variable-p 'magento-cache-clean-executable 'ignore)))

(ert-deftest magento-cache-clean-test-directory-safe-local ()
  "Directory is safe as a dir-local when absolute or nil."
  (should (safe-local-variable-p 'magento-cache-clean-directory nil))
  (should (safe-local-variable-p 'magento-cache-clean-directory "/path/to/magento"))
  (should-not (safe-local-variable-p 'magento-cache-clean-directory "src"))
  (should-not (safe-local-variable-p 'magento-cache-clean-directory 42)))

(provide 'magento-cache-clean-test)
;;; magento-cache-clean-test.el ends here
