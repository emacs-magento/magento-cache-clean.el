;;; magento-cache-clean-test.el --- Tests for magento-cache-clean -*- lexical-binding: t; -*-

;;; Commentary:

;; ERT tests for magento-cache-clean.

;;; Code:

(require 'cl-lib)
(require 'ert)
(require 'ert-x)
(require 'magento-cache-clean)

;;; Helpers

(defmacro magento-cache-clean-test--with-root (var &rest body)
  "Bind VAR to a temporary Magento root while running BODY.
The root has an empty app/etc/env.php and is deleted afterwards."
  (declare (indent 1))
  `(ert-with-temp-directory ,var
     :prefix "magento-test"
     (make-directory (expand-file-name "app/etc" ,var) t)
     (write-region "" nil (expand-file-name "app/etc/env.php" ,var))
     ,@body))

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

(ert-deftest magento-cache-clean-test-file-list-reads-from-root ()
  "The file list is read starting in the menu's root."
  (let ((dir nil))
    (cl-letf (((symbol-function 'transient-scope)
               (lambda () '(:root "/menu/root/")))
              ((symbol-function 'read-file-name)
               (lambda (&rest _) (setq dir default-directory) "list.txt")))
      (should (equal "/menu/root/list.txt"
                     (magento-cache-clean--read-file-list "File: " nil nil))))
    (should (equal "/menu/root/" dir))))

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

(ert-deftest magento-cache-clean-test-command-home-executable ()
  "A ~ executable is expanded in the home of a local root's host.
See the Docker tests for a remote root."
  (let ((magento-cache-clean-executable "~/bin/cache-clean"))
    (should (equal (expand-file-name "~/bin/cache-clean")
                   (car (magento-cache-clean--command "/a/src/" nil))))))

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

(ert-deftest magento-cache-clean-test-auto-looks-up-node-only-when-needed ()
  "Auto doesn't look for node when what it finds doesn't need it."
  (magento-cache-clean-test--with-root root
    (magento-cache-clean-test--with-programs root '("node" "cache-clean") nil
      (let ((found-p (symbol-function 'magento-cache-clean--found-p))
            (looked-up nil))
        (cl-letf (((symbol-function 'magento-cache-clean--found-p)
                   (lambda (name r)
                     (push name looked-up)
                     (funcall found-p name r))))
          (should (equal "cache-clean" (magento-cache-clean--executable root))))
        (should-not (member "node" looked-up))))))

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

(ert-deftest magento-cache-clean-test-executable-function-bad-result ()
  "A function returning neither a string nor nil is an error."
  (let ((magento-cache-clean-executable (lambda (_) 42)))
    (should-not (eq 'user-error
                    (car (should-error
                          (magento-cache-clean--executable "/a/src/")))))
    (should (string-search "returned 42"
                           (magento-cache-clean--menu-executable
                            (magento-cache-clean--menu-scope "/a/src/"))))))

(ert-deftest magento-cache-clean-test-executable-function-empty-result ()
  "A function returning an empty string is an error, not a program name."
  (let ((magento-cache-clean-executable (lambda (_) "")))
    (should-not (eq 'user-error
                    (car (should-error
                          (magento-cache-clean--executable "/a/src/")))))
    (should (string-search "returned \"\""
                           (magento-cache-clean--menu-executable
                            (magento-cache-clean--menu-scope "/a/src/"))))))

(ert-deftest magento-cache-clean-test-menu-executable ()
  "The menu shows the executable, marks auto and explains none found."
  (magento-cache-clean-test--with-root root
    (magento-cache-clean-test--with-programs root '("cache-clean") nil
      (should (equal "cache-clean (auto)"
                     (magento-cache-clean--menu-executable
                      (magento-cache-clean--menu-scope root))))
      (let ((magento-cache-clean-executable "cache-clean.js"))
        (should (equal "cache-clean.js"
                       (magento-cache-clean--menu-executable
                        (magento-cache-clean--menu-scope root)))))
      (let ((exec-path nil))
        (should (string-prefix-p "No cache-clean found (tried"
                                 (magento-cache-clean--menu-executable
                                  (magento-cache-clean--menu-scope root))))))))

(ert-deftest magento-cache-clean-test-menu-scope ()
  "The menu scope has the root and the executable, looked up once."
  (magento-cache-clean-test--with-root root
    (magento-cache-clean-test--with-programs root '("cache-clean") nil
      (should (equal (list :root root :executable "cache-clean" :auto t)
                     (magento-cache-clean--menu-scope root))))))

(ert-deftest magento-cache-clean-test-menu-executable-auto-from-function ()
  "A function returning nil auto-detects, which the menu shows."
  (magento-cache-clean-test--with-root root
    (magento-cache-clean-test--with-programs root '("cache-clean") nil
      (let ((magento-cache-clean-executable #'ignore))
        (should (equal "cache-clean (auto)"
                       (magento-cache-clean--menu-executable
                        (magento-cache-clean--menu-scope root))))))))

(ert-deftest magento-cache-clean-test-menu-executable-dir-local ()
  "The menu heading follows the opening buffer's local value.
Transient draws the heading in another buffer, where only the global
value is visible."
  (magento-cache-clean-test--with-root root
    (magento-cache-clean-test--with-programs root '("cache-clean") nil
      (let ((scope (with-temp-buffer
                     (setq-local magento-cache-clean-executable "cache-clean")
                     (magento-cache-clean--menu-scope root))))
        (with-temp-buffer
          (should (eq 'auto magento-cache-clean-executable))
          (should (equal "cache-clean"
                         (magento-cache-clean--menu-executable scope))))))))

(defun magento-cache-clean-test--write-dir-locals (root executable)
  "Make EXECUTABLE ROOT's directory local `magento-cache-clean-executable'."
  (write-region (format "((nil . ((magento-cache-clean-executable . %S))))\n"
                        executable)
                nil (expand-file-name ".dir-locals.el" root)))

(ert-deftest magento-cache-clean-test-one-shot-buffer-uses-dir-locals ()
  "The one-shot buffer has the root's directory local values.
The menu opened from it must find the same executable as from a file."
  (magento-cache-clean-test--with-root root
    (magento-cache-clean-test--write-dir-locals root "cache-clean")
    (let* ((enable-local-variables :safe)
           (magento-cache-clean-executable
            (magento-cache-clean-test--fake-executable root "echo done"))
           (default-directory root)
           (magento-cache-clean-directory nil)
           (buf (magento-cache-clean)))
      (unwind-protect
          (with-current-buffer buf
            (should (equal "cache-clean" magento-cache-clean-executable)))
        (magento-cache-clean-test--wait buf)
        (kill-buffer buf)))))

(ert-deftest magento-cache-clean-test-one-shot-returns-buffer ()
  "A one-shot clean returns its output buffer, also for another host."
  (magento-cache-clean-test--with-root root
    (let ((magento-cache-clean-executable
           (magento-cache-clean-test--fake-executable root "echo done"))
          (default-directory root)
          (magento-cache-clean-directory nil))
      (dolist (same-host '(t nil))
        (cl-letf (((symbol-function 'magento-cache-clean--same-host-p)
                   (lambda (_) same-host)))
          (let ((buf (magento-cache-clean)))
            (unwind-protect
                (should (eq buf (get-buffer
                                 (magento-cache-clean--buffer-name root))))
              (magento-cache-clean-test--wait buf)
              (kill-buffer buf))))))))

(ert-deftest magento-cache-clean-test-one-shot-dir-local-error-regexps ()
  "The one-shot buffer uses the root's error regexps."
  (magento-cache-clean-test--with-root root
    (write-region (format "%S\n" '((nil . ((magento-cache-clean-error-regexp-alist
                                             . (("^ERR \\(.+\\)$" 1)))))))
                  nil (expand-file-name ".dir-locals.el" root))
    (let* ((enable-local-variables :all)
           (magento-cache-clean-executable
            (magento-cache-clean-test--fake-executable root "echo done"))
           (default-directory root)
           (magento-cache-clean-directory nil)
           (buf (magento-cache-clean)))
      (unwind-protect
          (with-current-buffer buf
            (should (equal '(("^ERR \\(.+\\)$" 1))
                           compilation-error-regexp-alist)))
        (magento-cache-clean-test--wait buf)
        (kill-buffer buf)))))

(ert-deftest magento-cache-clean-test-watch-restart-uses-dir-locals ()
  "Restarting from the watch buffer keeps the root's executable."
  (magento-cache-clean-test--with-root root
    (magento-cache-clean-test--with-programs root '("cache-clean" "node") t
      (magento-cache-clean-test--write-dir-locals root "cache-clean")
      (let* ((enable-local-variables :safe)
             (buf (magento-cache-clean--watch-start root nil)))
        (unwind-protect
            (with-current-buffer buf
              (should (equal "cache-clean" magento-cache-clean-executable))
              (should (equal "cache-clean"
                             (car (magento-cache-clean--command root nil)))))
          (let ((kill-buffer-query-functions nil))
            (kill-buffer buf)))))))

(ert-deftest magento-cache-clean-test-watch-restart-drops-removed-dir-locals ()
  "A value removed from .dir-locals.el is gone after a restart."
  (magento-cache-clean-test--with-root root
    (magento-cache-clean-test--with-programs root '("cache-clean" "node") t
      (magento-cache-clean-test--write-dir-locals root "cache-clean")
      (let* ((enable-local-variables :safe)
             (dir-locals-directory-cache nil)
             (buf (magento-cache-clean--watch-start root nil)))
        (unwind-protect
            (with-current-buffer buf
              (should (local-variable-p 'magento-cache-clean-executable))
              (write-region "()\n" nil (expand-file-name ".dir-locals.el" root))
              (setq dir-locals-directory-cache nil)
              (magento-cache-clean-watch-restart)
              (should-not (local-variable-p 'magento-cache-clean-executable))
              (should (equal (expand-file-name "vendor/bin/cache-clean.js" root)
                             (car (magento-cache-clean--command root nil)))))
          (let ((kill-buffer-query-functions nil))
            (kill-buffer buf)))))))

(ert-deftest magento-cache-clean-test-command-reuses-menu-executable ()
  "From the menu, the command uses the executable the menu found."
  (let ((transient-current-command 'magento-cache-clean-menu))
    (cl-letf (((symbol-function 'transient-scope)
               (lambda () '(:root "/a/src/" :executable "menu-cache-clean")))
              ((symbol-function 'magento-cache-clean--executable)
               (lambda (_) (error "Unexpected lookup"))))
      (should (equal '("menu-cache-clean" "-d" "/a/src/")
                     (magento-cache-clean--command "/a/src/" nil))))))

(ert-deftest magento-cache-clean-test-command-menu-error-looks-up-again ()
  "When the menu found no executable, the command signals the error."
  (magento-cache-clean-test--with-root root
    (magento-cache-clean-test--with-programs root nil nil
      (let ((transient-current-command 'magento-cache-clean-menu)
            (scope (magento-cache-clean--menu-scope root)))
        (cl-letf (((symbol-function 'transient-scope) (lambda () scope)))
          (should-error (magento-cache-clean--command root nil)
                        :type 'user-error))))))

(ert-deftest magento-cache-clean-test-executable-invalid-value ()
  "A value that is neither auto, a string nor a function is a user error."
  (let ((magento-cache-clean-executable 'no-such-function))
    (should-error (magento-cache-clean--executable "/a/src/")
                  :type 'user-error)))

(ert-deftest magento-cache-clean-test-executable-empty ()
  "An empty executable is a user error, not a program to run."
  (let ((magento-cache-clean-executable ""))
    (should-error (magento-cache-clean--executable "/a/src/")
                  :type 'user-error)))

;;; Cache types

(ert-deftest magento-cache-clean-test-cache-types-from-env ()
  "Cache types are completed from env.php, in its order."
  (magento-cache-clean-test--with-root root
    (write-region "<?php\nreturn [\n    'cache_types' => [\n        'config' => 1,\n        'my_custom' => 0\n    ],\n    'other' => ['x' => 1]\n];\n"
                  nil (expand-file-name "app/etc/env.php" root))
    (cl-letf (((symbol-function 'transient-scope) (lambda () (list :root root))))
      (should (equal '("config" "my_custom")
                     (magento-cache-clean--cache-types))))))

(ert-deftest magento-cache-clean-test-cache-types-old-array-syntax ()
  "Cache types are read from an array() style env.php too."
  (magento-cache-clean-test--with-root root
    (write-region "<?php\nreturn array(\n  \"cache_types\" => array (\n    \"layout\" => 1,\n  ),\n);\n"
                  nil (expand-file-name "app/etc/env.php" root))
    (should (equal '("layout") (magento-cache-clean--env-cache-types root)))))

(ert-deftest magento-cache-clean-test-cache-types-skip-comments ()
  "Commented out cache types and brackets in comments are skipped."
  (magento-cache-clean-test--with-root root
    (write-region "<?php
return [
    // 'cache_types' => ['old' => 1],
    # 'cache_types' => ['older' => 1],
    /* 'cache_types' => [
        'oldest' => 1,
    ], */
    'cache_types' => [
        'config' => 1, // keeps ] out
        # 'disabled' => 0,
        /* 'gone' => 1, */
        'layout' => 1
    ]
];
"
                  nil (expand-file-name "app/etc/env.php" root))
    (should (equal '("config" "layout")
                   (magento-cache-clean--env-cache-types root)))))

(ert-deftest magento-cache-clean-test-cache-types-unclosed ()
  "An unclosed cache types list gives none, not the keys after it."
  (magento-cache-clean-test--with-root root
    (write-region "<?php\nreturn [\n    'cache_types' => [\n        'config' => 1,\n    'db' => 'x'\n"
                  nil (expand-file-name "app/etc/env.php" root))
    (should-not (magento-cache-clean--env-cache-types root))))

(ert-deftest magento-cache-clean-test-cache-types-fallback ()
  "Without cache types in env.php, the core types are completed."
  (magento-cache-clean-test--with-root root
    (cl-letf (((symbol-function 'transient-scope) (lambda () (list :root root))))
      (should (equal magento-cache-clean--core-cache-types
                     (magento-cache-clean--cache-types)))))
  (cl-letf (((symbol-function 'transient-scope)
             (lambda () '(:root "/no/such/root/"))))
    (should (equal magento-cache-clean--core-cache-types
                   (magento-cache-clean--cache-types)))))

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
  (ert-with-temp-directory temp-dir
    :prefix "not-magento"
    (let ((default-directory temp-dir)
          (magento-cache-clean-directory nil))
      (cl-letf (((symbol-function 'read-directory-name)
                 (lambda (&rest _) (error "Unexpected prompt"))))
        (should-not (magento-cache-clean--root))))))

(ert-deftest magento-cache-clean-test-directory-uses-menu-root ()
  "Commands run from the menu use the root it was opened with."
  (let ((transient-current-command 'magento-cache-clean-menu)
        (magento-cache-clean-directory "/elsewhere/"))
    (cl-letf (((symbol-function 'transient-scope) (lambda () '(:root "/menu/root/"))))
      (should (equal "/menu/root/" (magento-cache-clean--directory))))))

(ert-deftest magento-cache-clean-test-directory-uses-custom ()
  "Use custom directory when set."
  (ert-with-temp-directory temp-dir
    :prefix "magento-test"
    (let ((magento-cache-clean-directory (directory-file-name temp-dir)))
      (should (equal temp-dir (magento-cache-clean--directory))))))

(ert-deftest magento-cache-clean-test-directory-custom-missing ()
  "A configured root that isn't a directory is a user error."
  (let ((magento-cache-clean-directory "/no/such/magento/root/"))
    (should-error (magento-cache-clean--directory) :type 'user-error)))

(ert-deftest magento-cache-clean-test-directory-of-output-buffer ()
  "An output buffer's root is used, even without env.php or when another
root is configured."
  (ert-with-temp-directory dir
    :prefix "not-magento"
    (let ((magento-cache-clean-directory "/elsewhere/"))
      (cl-letf (((symbol-function 'read-directory-name)
                 (lambda (&rest _) (error "Unexpected prompt"))))
        (dolist (mode '(magento-cache-clean-watch-mode
                        magento-cache-clean-compilation-mode))
          (with-temp-buffer
            (setq default-directory dir)
            (let ((enable-local-variables nil))
              (funcall mode))
            (should (equal dir (magento-cache-clean--directory)))))))))

(ert-deftest magento-cache-clean-test-directory-prompt-is-expanded ()
  "A prompted directory is expanded, since the watch process gets no shell."
  (ert-with-temp-directory temp-dir
    :prefix "not-magento"
    (let ((default-directory temp-dir)
          (magento-cache-clean-directory nil))
      (cl-letf (((symbol-function 'read-directory-name)
                 (lambda (&rest _) "~/magento"))
                ((symbol-function 'yes-or-no-p) #'always))
        (should (equal (file-name-as-directory (expand-file-name "~/magento"))
                       (magento-cache-clean--directory)))))))

(ert-deftest magento-cache-clean-test-read-root-confirms-non-magento ()
  "A prompted directory without env.php is only used after confirmation."
  (ert-with-temp-directory dir
    :prefix "not-magento"
    (let ((asked nil))
      (cl-letf (((symbol-function 'read-directory-name)
                 (lambda (&rest _) dir)))
        (cl-letf (((symbol-function 'yes-or-no-p)
                   (lambda (&rest _) (setq asked t) nil)))
          (should-error (magento-cache-clean--read-root) :type 'user-error))
        (should asked)
        (cl-letf (((symbol-function 'yes-or-no-p) #'always))
          (should (equal dir (magento-cache-clean--read-root)))))))
  (magento-cache-clean-test--with-root root
    (cl-letf (((symbol-function 'read-directory-name) (lambda (&rest _) root))
              ((symbol-function 'yes-or-no-p)
               (lambda (&rest _) (error "Unexpected prompt"))))
      (should (equal root (magento-cache-clean--read-root))))))

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
    (should (eq 'magento-cache-clean-watch-restart (key-binding (kbd "C-c C-r"))))
    (should (eq 'find-file (key-binding (kbd "C-x C-f"))))))

(ert-deftest magento-cache-clean-test-watch-ret-sends-only-return ()
  "RET on an old output line doesn't send that line as hotkeys."
  (with-temp-buffer
    (magento-cache-clean-watch-mode)
    (should (eq 'magento-cache-clean-watch-send-key (key-binding (kbd "RET"))))
    (let ((sent nil))
      (cl-letf (((symbol-function 'get-buffer-process) (lambda (_) 'proc))
                ((symbol-function 'process-live-p) (lambda (_) t))
                ((symbol-function 'process-send-string)
                 (lambda (_ string) (setq sent string))))
        (let ((inhibit-read-only t))
          (insert "Cleaned layout cache\n"))
        (goto-char (point-min))
        (let ((last-command-event ?\r))
          (call-interactively (key-binding (kbd "RET")))))
      (should (equal "\r" sent)))))

(ert-deftest magento-cache-clean-test-watch-send-key-repeats ()
  "A prefix argument sends the key that many times, as when typing."
  (with-temp-buffer
    (magento-cache-clean-watch-mode)
    (let ((sent nil))
      (cl-letf (((symbol-function 'get-buffer-process) (lambda (_) 'proc))
                ((symbol-function 'process-live-p) (lambda (_) t))
                ((symbol-function 'process-send-string)
                 (lambda (_ string) (setq sent string))))
        (let ((last-command-event ?c)
              (current-prefix-arg 3))
          (call-interactively #'magento-cache-clean-watch-send-key))
        (should (equal "ccc" sent))
        (let ((last-command-event ?c))
          (magento-cache-clean-watch-send-key))
        (should (equal "c" sent))))))

(ert-deftest magento-cache-clean-test-watch-send-key-exited-process ()
  "A key sent to a watcher that has exited is a user error."
  (with-temp-buffer
    (magento-cache-clean-watch-mode)
    (let ((delete-exited-processes nil)
          (proc (make-process :name "exited-watcher" :buffer (current-buffer)
                              :command '("true"))))
      (unwind-protect
          (progn
            (magento-cache-clean-test--wait-until
             (lambda () (not (process-live-p proc))))
            (should (eq proc (get-buffer-process (current-buffer))))
            (let ((last-command-event ?c))
              (should-error (magento-cache-clean-watch-send-key)
                            :type 'user-error)))
        (delete-process proc)))))

(ert-deftest magento-cache-clean-test-watch-send-key-only-in-watch-mode ()
  "M-x offers the key forwarding command only in watch buffers."
  (should (equal '(magento-cache-clean-watch-mode)
                 (command-modes 'magento-cache-clean-watch-send-key))))

(ert-deftest magento-cache-clean-test-watch-send-key-non-positive ()
  "A zero prefix argument sends nothing and a negative one is refused."
  (with-temp-buffer
    (magento-cache-clean-watch-mode)
    (let ((sent nil))
      (cl-letf (((symbol-function 'get-buffer-process) (lambda (_) 'proc))
                ((symbol-function 'process-live-p) (lambda (_) t))
                ((symbol-function 'process-send-string)
                 (lambda (_ string) (setq sent string))))
        (let ((last-command-event ?c))
          (magento-cache-clean-watch-send-key 0)
          (should (equal "" sent))
          (setq sent nil)
          (should-error (magento-cache-clean-watch-send-key -1)
                        :type 'user-error)
          (should-not sent))))))

(ert-deftest magento-cache-clean-test-watch-send-key-non-character ()
  "An event that isn't a character, as from M-x, is a user error."
  (with-temp-buffer
    (magento-cache-clean-watch-mode)
    (let ((sent nil))
      (cl-letf (((symbol-function 'get-buffer-process) (lambda (_) 'proc))
                ((symbol-function 'process-live-p) (lambda (_) t))
                ((symbol-function 'process-send-string)
                 (lambda (_ string) (setq sent string))))
        (let ((last-command-event 'f5))
          (should-error (magento-cache-clean-watch-send-key)
                        :type 'user-error))
        (should-not sent)))))

(ert-deftest magento-cache-clean-test-watch-truncates-buffer ()
  "Watch buffers are truncated, as a watcher can run for days."
  (with-temp-buffer
    (magento-cache-clean-watch-mode)
    (should (memq #'comint-truncate-buffer comint-output-filter-functions))))

(ert-deftest magento-cache-clean-test-watch-output-read-only ()
  "Keys that aren't sent to the watcher can't edit its output."
  (magento-cache-clean-test--with-root root
    (let* ((magento-cache-clean-executable
            (magento-cache-clean-test--fake-executable
             root "echo ready; exec sleep 30"))
           (default-directory root)
           (magento-cache-clean-directory nil)
           (buf (magento-cache-clean-watch)))
      (unwind-protect
          (with-current-buffer buf
            (magento-cache-clean-test--wait-for-output buf "ready")
            (goto-char (point-max))
            (should-error (delete-char -1) :type 'buffer-read-only)
            (should (string-match-p "ready" (buffer-string)))
            (magento-cache-clean-watch-stop buf)
            (magento-cache-clean-test--wait buf)
            (should (string-match-p "^Process .* interrupt" (buffer-string))))
        (when (buffer-live-p buf) (kill-buffer buf))))))

(ert-deftest magento-cache-clean-test-watch-ignores-password-prompts ()
  "Output looking like a password prompt doesn't read a password."
  (with-temp-buffer
    (magento-cache-clean-watch-mode)
    (let ((read nil))
      (cl-letf (((symbol-function 'run-at-time)
                 (lambda (&rest _) (setq read t)))
                ((symbol-function 'send-invisible)
                 (lambda (&rest _) (setq read t))))
        (comint-watch-for-password-prompt "Password: ")
        (should-not read)))))

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
              (should (derived-mode-p 'magento-cache-clean-compilation-mode))
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

(ert-deftest magento-cache-clean-test-watch-global-env-stays-global ()
  "Watch doesn't freeze a copy of the global environment.
Without buffer-local values in the caller, the watch buffer has none
either, so later changes to the global values reach restarts."
  (magento-cache-clean-test--with-root root
    (let ((magento-cache-clean-executable
           (magento-cache-clean-test--fake-executable root "exec sleep 30"))
          (magento-cache-clean-directory nil)
          (buf nil))
      (unwind-protect
          (with-temp-buffer
            (setq default-directory root)
            (setq buf (magento-cache-clean-watch))
            (with-current-buffer buf
              (should-not (local-variable-p 'process-environment))
              (should-not (local-variable-p 'exec-path))
              (magento-cache-clean-watch-restart)
              (should-not (local-variable-p 'process-environment))
              (should-not (local-variable-p 'exec-path))))
        (when (buffer-live-p buf) (kill-buffer buf))))))

(ert-deftest magento-cache-clean-test-watch-inherits-each-local-separately ()
  "A local exec-path is inherited without a local environment."
  (magento-cache-clean-test--with-root root
    (let ((magento-cache-clean-executable
           (magento-cache-clean-test--fake-executable root "exec sleep 30"))
          (magento-cache-clean-directory nil)
          (buf nil))
      (unwind-protect
          (with-temp-buffer
            (setq default-directory root)
            (setq-local exec-path (cons "/caller/bin/" exec-path))
            (setq buf (magento-cache-clean-watch))
            (with-current-buffer buf
              (should-not (local-variable-p 'process-environment))
              (should (equal "/caller/bin/" (car exec-path)))))
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
          ;; Don't let Tramp connect to check the root.
          (cl-letf (((symbol-function 'file-directory-p) #'always))
            (magento-cache-clean nil))
          (should-not local-env))
        (let ((magento-cache-clean-directory temporary-file-directory))
          (magento-cache-clean nil)
          (should local-env))))))

(ert-deftest magento-cache-clean-test-one-shot-remote-quotes-posix ()
  "A remote one-shot command is quoted for a POSIX shell."
  (let ((magento-cache-clean-executable "cache-clean")
        (posix-args nil)
        (quote-argument (symbol-function 'shell-quote-argument)))
    (cl-letf (((symbol-function 'magento-cache-clean--buffer-name)
               (lambda (&rest _) "*magento-cache-clean-test*"))
              ((symbol-function 'compilation-start) #'ignore)
              ((symbol-function 'shell-quote-argument)
               (lambda (arg &optional posix)
                 (push posix posix-args)
                 (funcall quote-argument arg posix)))
              ;; Don't let Tramp connect to check the root.
              ((symbol-function 'file-directory-p) #'always))
      (let ((default-directory temporary-file-directory)
            (magento-cache-clean-directory "/ssh:host:/var/www/"))
        (magento-cache-clean nil)
        (should posix-args)
        (should (cl-every #'identity posix-args)))
      (setq posix-args nil)
      (let ((default-directory temporary-file-directory)
            (magento-cache-clean-directory temporary-file-directory))
        (magento-cache-clean nil)
        (should posix-args)
        (should-not (cl-some #'identity posix-args))))))

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

(ert-deftest magento-cache-clean-test-watch-shows-shell-quoted-command ()
  "The watch buffer's command line can be pasted into a shell."
  (magento-cache-clean-test--with-root root
    (let* ((magento-cache-clean-executable
            (magento-cache-clean-test--fake-executable root "exec sleep 30"))
           (default-directory root)
           (magento-cache-clean-directory nil)
           (buf (magento-cache-clean-watch '("$HOME's"))))
      (unwind-protect
          (with-current-buffer buf
            (goto-char (point-min))
            (should (looking-at-p
                     (regexp-quote
                      (format "$ %s -d %s --watch %s\n"
                              (shell-quote-argument magento-cache-clean-executable)
                              (shell-quote-argument root)
                              (shell-quote-argument "$HOME's"))))))
        (when (buffer-live-p buf) (kill-buffer buf))))))

(ert-deftest magento-cache-clean-test-watch-query-on-exit ()
  "Exiting Emacs asks about watchers only when the option says so."
  (magento-cache-clean-test--with-root root
    (let ((magento-cache-clean-executable
           (magento-cache-clean-test--fake-executable root "exec sleep 30"))
          (default-directory root)
          (magento-cache-clean-directory nil)
          (buf nil))
      (unwind-protect
          (progn
            (setq buf (magento-cache-clean-watch))
            (should-not (process-query-on-exit-flag (get-buffer-process buf)))
            (let ((magento-cache-clean-watch-query-on-exit t))
              (with-current-buffer buf (magento-cache-clean-watch-restart)))
            (should (process-query-on-exit-flag (get-buffer-process buf))))
        (when (buffer-live-p buf)
          (when-let* ((proc (get-buffer-process buf)))
            (set-process-query-on-exit-flag proc nil))
          (kill-buffer buf))))))

(ert-deftest magento-cache-clean-test-watch-reaped-at-start ()
  "A watcher already gone when it was started leaves its buffer.
Tramp may reap a remote command that exits at once before it returns."
  (magento-cache-clean-test--with-root root
    (let* ((magento-cache-clean-executable
            (magento-cache-clean-test--fake-executable root "exec sleep 30"))
           (default-directory root)
           (magento-cache-clean-directory nil)
           (orig (symbol-function 'comint-exec))
           (buf nil))
      (unwind-protect
          (cl-letf (((symbol-function 'comint-exec)
                     (lambda (buffer &rest args)
                       (apply orig buffer args)
                       (delete-process (get-buffer-process buffer))
                       buffer)))
            (setq buf (magento-cache-clean-watch))
            (should (buffer-live-p buf))
            (should-not (get-buffer-process buf)))
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
            (cl-letf (((symbol-function 'y-or-n-p) #'always))
              (magento-cache-clean-watch))
            (let ((second (get-buffer-process buf)))
              (should second)
              (should-not (eq first second))
              (magento-cache-clean-test--wait-until
               (lambda () (not (process-live-p first))))
              (should (process-live-p second))
              (should-not (process-live-p first))))
        (when (buffer-live-p buf) (kill-buffer buf))))))

(defmacro magento-cache-clean-test--recording-interrupts (var &rest body)
  "Run BODY, pushing each process `interrupt-process' gets onto VAR."
  (declare (indent 1))
  (let ((orig (make-symbol "orig")))
    `(let ((,orig (symbol-function 'interrupt-process)))
       (cl-letf (((symbol-function 'interrupt-process)
                  (lambda (proc &rest args)
                    (push proc ,var)
                    (apply ,orig proc args))))
         ,@body))))

(ert-deftest magento-cache-clean-test-watch-restart-interrupts-old-process ()
  "Restarting interrupts the old watcher rather than only deleting it.
Deleting a remote process leaves the watcher running on its host."
  (magento-cache-clean-test--with-root root
    (let* ((magento-cache-clean-executable
            (magento-cache-clean-test--fake-executable root "exec sleep 30"))
           (default-directory root)
           (magento-cache-clean-directory nil)
           (buf (magento-cache-clean-watch))
           (first (get-buffer-process buf))
           (interrupted nil))
      (unwind-protect
          (magento-cache-clean-test--recording-interrupts interrupted
            (with-current-buffer buf
              (magento-cache-clean-watch-restart))
            (should (equal (list first) interrupted)))
        (let ((kill-buffer-query-functions nil))
          (kill-buffer buf))))))

(ert-deftest magento-cache-clean-test-watch-kill-buffer-interrupts-process ()
  "Killing a watch buffer interrupts its watcher."
  (magento-cache-clean-test--with-root root
    (let* ((magento-cache-clean-executable
            (magento-cache-clean-test--fake-executable root "exec sleep 30"))
           (default-directory root)
           (magento-cache-clean-directory nil)
           (buf (magento-cache-clean-watch))
           (proc (get-buffer-process buf))
           (interrupted nil))
      (magento-cache-clean-test--recording-interrupts interrupted
        (let ((kill-buffer-query-functions nil))
          (kill-buffer buf)))
      (should (equal (list proc) interrupted)))))

(ert-deftest magento-cache-clean-test-watch-kill-buffer-despite-interrupt-error ()
  "A failing interrupt, as on a lost connection, still kills the buffer."
  (magento-cache-clean-test--with-root root
    (let* ((magento-cache-clean-executable
            (magento-cache-clean-test--fake-executable root "exec sleep 30"))
           (default-directory root)
           (magento-cache-clean-directory nil)
           (buf (magento-cache-clean-watch)))
      (unwind-protect
          (cl-letf (((symbol-function 'interrupt-process)
                     (lambda (&rest _) (error "Connection lost"))))
            (let ((kill-buffer-query-functions nil)
                  (inhibit-message t))
              (kill-buffer buf))
            (should-not (buffer-live-p buf)))
        (when (buffer-live-p buf)
          (let ((kill-buffer-query-functions nil)
                (kill-buffer-hook nil))
            (kill-buffer buf)))))))

(ert-deftest magento-cache-clean-test-kill-emacs-interrupts-watchers ()
  "Exiting Emacs interrupts running watchers, despite errors."
  (magento-cache-clean-test--with-root root
    (let* ((magento-cache-clean-executable
            (magento-cache-clean-test--fake-executable root "trap '' INT; exec sleep 30"))
           (default-directory root)
           (magento-cache-clean-directory nil)
           (kill-emacs-hook nil)
           (buf (magento-cache-clean-watch))
           (proc (get-buffer-process buf))
           (interrupted nil))
      (unwind-protect
          (progn
            (should (memq #'magento-cache-clean--interrupt-watchers
                          kill-emacs-hook))
            (magento-cache-clean-test--recording-interrupts interrupted
              (run-hooks 'kill-emacs-hook))
            (should (equal (list proc) interrupted))
            (cl-letf (((symbol-function 'interrupt-process)
                       (lambda (&rest _) (error "Connection lost"))))
              (let ((inhibit-message t))
                (run-hooks 'kill-emacs-hook))))
        (let ((kill-buffer-query-functions nil))
          (kill-buffer buf))))))

(ert-deftest magento-cache-clean-test-watch-declined-restart-keeps-process ()
  "Declining the restart prompt keeps and shows the running watcher."
  (magento-cache-clean-test--with-root root
    (let* ((magento-cache-clean-executable
            (magento-cache-clean-test--fake-executable root "exec sleep 30"))
           (default-directory root)
           (magento-cache-clean-directory nil)
           (buf (magento-cache-clean-watch)))
      (unwind-protect
          (let ((proc (get-buffer-process buf))
                (shown nil))
            (cl-letf (((symbol-function 'y-or-n-p) #'ignore)
                      ((symbol-function 'display-buffer)
                       (lambda (b &rest _) (setq shown b))))
              (should (eq buf (magento-cache-clean-watch))))
            (should (eq buf shown))
            (should (eq proc (get-buffer-process buf)))
            (should (process-live-p proc)))
        (when (buffer-live-p buf) (kill-buffer buf))))))

(ert-deftest magento-cache-clean-test-watch-not-found-leaves-no-buffer ()
  "A watcher that can't start because nothing is found leaves no buffer."
  (magento-cache-clean-test--with-root root
    (magento-cache-clean-test--with-programs root nil nil
      (let ((default-directory root)
            (magento-cache-clean-directory nil))
        (should-error (magento-cache-clean-watch) :type 'user-error)
        (should-not (get-buffer (magento-cache-clean--buffer-name root t)))))))

(ert-deftest magento-cache-clean-test-watch-missing-executable-leaves-no-buffer ()
  "A configured executable that doesn't exist leaves no buffer."
  (magento-cache-clean-test--with-root root
    (let ((magento-cache-clean-executable "magento-cache-clean-test-missing")
          (default-directory root)
          (magento-cache-clean-directory nil))
      (should-error (magento-cache-clean-watch) :type 'file-missing)
      (should-not (magento-cache-clean--root-watch-buffer root)))))

(ert-deftest magento-cache-clean-test-watch-missing-executable-on-restart ()
  "A restart whose executable is gone says why in the watch buffer."
  (magento-cache-clean-test--with-root root
    (let* ((magento-cache-clean-executable
            (magento-cache-clean-test--fake-executable root "exec sleep 30"))
           (default-directory root)
           (magento-cache-clean-directory nil)
           (buf (magento-cache-clean-watch)))
      (unwind-protect
          (cl-letf ((magento-cache-clean-executable
                     "magento-cache-clean-test-missing")
                    ((symbol-function 'y-or-n-p) (lambda (_) t)))
            (should-error (magento-cache-clean-watch) :type 'file-missing)
            (should (eq buf (magento-cache-clean--root-watch-buffer root)))
            (with-current-buffer buf
              (should-not (process-live-p (get-buffer-process buf)))
              (should (string-match-p "magento-cache-clean-test-missing\n\\'"
                                      (buffer-string)))))
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
              (let ((inhibit-read-only t))
                (erase-buffer))
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
           (when-let* ((buf (magento-cache-clean--root-watch-buffer ,(car roots))))
             (kill-buffer buf)))))))

(defun magento-cache-clean-test--watch-buffer (root)
  "Return the watch buffer of ROOT."
  (magento-cache-clean--root-watch-buffer root))

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

(ert-deftest magento-cache-clean-test-watch-buffer-defaults ()
  "The watch buffer is the menu root's, else the current one, else the root's."
  (magento-cache-clean-test--with-watchers (a b)
    (let ((buf-a (magento-cache-clean-test--watch-buffer a))
          (buf-b (magento-cache-clean-test--watch-buffer b)))
      (with-current-buffer buf-a
        (let ((magento-cache-clean-directory b))
          (should (eq buf-a (magento-cache-clean--watch-buffer)))
          (let ((transient-current-command 'magento-cache-clean-menu))
            (cl-letf (((symbol-function 'transient-scope)
                       (lambda () (list :root b))))
              (should (eq buf-b (magento-cache-clean--watch-buffer)))))))
      (let ((default-directory b))
        (should (eq buf-b (magento-cache-clean--watch-buffer)))
        (should (eq buf-a (magento-cache-clean--watch-buffer a)))))
    (let ((default-directory temporary-file-directory))
      (should-not (magento-cache-clean--watch-buffer)))))

(ert-deftest magento-cache-clean-test-watch-buffer-renamed ()
  "A renamed watch buffer is still its root's, and a restart reuses it."
  (magento-cache-clean-test--with-watchers (a)
    (let ((buf (magento-cache-clean-test--watch-buffer a)))
      (with-current-buffer buf
        (rename-buffer "my watcher"))
      (should (eq (get-buffer-process buf)
                  (magento-cache-clean--watch-process a)))
      (let ((default-directory a))
        (cl-letf (((symbol-function 'y-or-n-p) (lambda (&rest _) t)))
          (should (eq buf (magento-cache-clean-watch)))))
      (should (process-live-p (get-buffer-process buf)))
      (should-not (get-buffer (magento-cache-clean--buffer-name a t))))))

(ert-deftest magento-cache-clean-test-stop-by-buffer-name ()
  "Stop takes the watch buffer's name too."
  (magento-cache-clean-test--with-watchers (a)
    (let ((buf (magento-cache-clean-test--watch-buffer a)))
      (magento-cache-clean-watch-stop (buffer-name buf))
      (magento-cache-clean-test--wait buf)
      (should-not (get-buffer-process buf)))))

(ert-deftest magento-cache-clean-test-stop-uses-current-root ()
  "Inside a watched root, stop acts on its watcher without asking."
  (magento-cache-clean-test--with-watchers (a b)
    (let ((default-directory a))
      (cl-letf (((symbol-function 'read-buffer)
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
      (cl-letf (((symbol-function 'read-buffer)
                 (lambda (_prompt _default _require-match predicate)
                   (setq offered predicate)
                   (buffer-name (magento-cache-clean-test--watch-buffer b)))))
        (call-interactively #'magento-cache-clean-watch-stop))
      (dolist (root (list a b))
        (let ((name (buffer-name (magento-cache-clean-test--watch-buffer root))))
          (should (funcall offered name))
          (should (funcall offered (cons name nil)))))
      (should-not (funcall offered (buffer-name (current-buffer)))))
    (magento-cache-clean-test--wait (magento-cache-clean-test--watch-buffer b))
    (should-not (get-buffer-process (magento-cache-clean-test--watch-buffer b)))
    (should (get-buffer-process (magento-cache-clean-test--watch-buffer a)))))

(ert-deftest magento-cache-clean-test-stop-with-bad-root-reads-watcher ()
  "A configured root that isn't a directory still lets stop read a watcher."
  (magento-cache-clean-test--with-watchers (a)
    (let ((default-directory temporary-file-directory)
          (magento-cache-clean-directory "/no/such/magento/"))
      (cl-letf (((symbol-function 'read-buffer)
                 (lambda (&rest _)
                   (buffer-name (magento-cache-clean-test--watch-buffer a)))))
        (call-interactively #'magento-cache-clean-watch-stop)))
    (magento-cache-clean-test--wait (magento-cache-clean-test--watch-buffer a))
    (should-not (get-buffer-process (magento-cache-clean-test--watch-buffer a)))))

(ert-deftest magento-cache-clean-test-stop-from-menu-uses-menu-root ()
  "From the menu, stop acts on the menu's root, even in another watch buffer."
  (magento-cache-clean-test--with-watchers (a b)
    (with-current-buffer (magento-cache-clean-test--watch-buffer a)
      (let ((transient-current-command 'magento-cache-clean-menu))
        (cl-letf (((symbol-function 'transient-scope)
                   (lambda () (list :root b))))
          (call-interactively #'magento-cache-clean-watch-stop))))
    (magento-cache-clean-test--wait (magento-cache-clean-test--watch-buffer b))
    (should-not (get-buffer-process (magento-cache-clean-test--watch-buffer b)))
    (should (get-buffer-process (magento-cache-clean-test--watch-buffer a)))))

(ert-deftest magento-cache-clean-test-menu-restarts-without-prompt ()
  "From the menu, where it reads \"Restart watch\", restart doesn't ask."
  (magento-cache-clean-test--with-watchers (a)
    (let ((transient-current-command 'magento-cache-clean-menu)
          (first (get-buffer-process (magento-cache-clean-test--watch-buffer a))))
      (cl-letf (((symbol-function 'transient-scope) (lambda () (list :root a)))
                ((symbol-function 'y-or-n-p)
                 (lambda (&rest _) (error "Unexpected prompt"))))
        (magento-cache-clean-watch))
      (should-not (eq first (get-buffer-process
                             (magento-cache-clean-test--watch-buffer a)))))))

(ert-deftest magento-cache-clean-test-stop-nothing-running ()
  "Stopping with no watcher running is a user error."
  (let ((default-directory temporary-file-directory))
    (cl-letf (((symbol-function 'magento-cache-clean--watch-buffers) #'ignore))
      (should-error (call-interactively #'magento-cache-clean-watch-stop)
                    :type 'user-error))))

(ert-deftest magento-cache-clean-test-stop-without-live-process ()
  "Stopping a buffer without a live watcher is a user error."
  (with-temp-buffer
    (magento-cache-clean-watch-mode)
    (should-error (magento-cache-clean-watch-stop (current-buffer))
                  :type 'user-error)))

(ert-deftest magento-cache-clean-test-stop-only-watch-buffers ()
  "Stop refuses a missing buffer or another buffer's process."
  (should-error (magento-cache-clean-watch-stop "*no such watch buffer*")
                :type 'user-error)
  (with-temp-buffer
    (let ((proc (make-process :name "not-a-watcher" :buffer (current-buffer)
                              :command '("sleep" "30"))))
      (unwind-protect
          (progn
            (should-error (magento-cache-clean-watch-stop (current-buffer))
                          :type 'user-error)
            (should (process-live-p proc)))
        (delete-process proc)))))

(ert-deftest magento-cache-clean-test-stop-all ()
  "Stop all interrupts every running watcher."
  (magento-cache-clean-test--with-watchers (a b)
    (should (equal "Interrupted 2 magento-cache-clean watchers"
                   (magento-cache-clean-watch-stop-all)))
    (dolist (root (list a b))
      (magento-cache-clean-test--wait (magento-cache-clean-test--watch-buffer root)))
    (should-not (magento-cache-clean--watch-buffers))))

(ert-deftest magento-cache-clean-test-stop-all-despite-interrupt-error ()
  "A watcher that fails to stop doesn't keep the others running."
  (magento-cache-clean-test--with-watchers (a b)
    (let* ((buffers (magento-cache-clean--watch-buffers))
           (failing (get-buffer-process (car buffers)))
           (orig (symbol-function 'interrupt-process))
           (interrupted nil))
      (cl-letf (((symbol-function 'interrupt-process)
                 (lambda (proc &rest args)
                   (if (eq proc failing)
                       (error "Connection lost")
                     (push proc interrupted)
                     (apply orig proc args))))
                (inhibit-message t))
        (should (equal "Interrupted 1 magento-cache-clean watcher; 1 failed, see *Messages*"
                       (magento-cache-clean-watch-stop-all))))
      (should (equal (list (get-buffer-process (cadr buffers))) interrupted)))))

(ert-deftest magento-cache-clean-test-watch-interrupt-result ()
  "Interrupting a watcher says whether it worked and logs which one failed."
  (magento-cache-clean-test--with-watchers (a)
    (with-current-buffer (magento-cache-clean-test--watch-buffer a)
      (let ((logged nil))
        (cl-letf (((symbol-function 'interrupt-process)
                   (lambda (&rest _) (error "Connection lost")))
                  ((symbol-function 'message)
                   (lambda (format &rest args)
                     (setq logged (apply #'format-message format args)))))
          (should-not (magento-cache-clean--watch-interrupt)))
        (should (string-search (buffer-name) logged))
        (should (string-search "Connection lost" logged)))
      (should (magento-cache-clean--watch-interrupt)))))

(ert-deftest magento-cache-clean-test-watch-interrupt-times-out ()
  "An interrupt that hangs, as on a lost connection, gives up in time."
  (magento-cache-clean-test--with-watchers (a)
    (with-current-buffer (magento-cache-clean-test--watch-buffer a)
      (let ((magento-cache-clean-interrupt-timeout 0.1)
            (logged nil)
            (start (float-time)))
        (cl-letf (((symbol-function 'interrupt-process)
                   (lambda (&rest _) (sleep-for 5)))
                  ((symbol-function 'message)
                   (lambda (format &rest args)
                     (setq logged (apply #'format-message format args)))))
          (should-not (magento-cache-clean--watch-interrupt)))
        (should (< (- (float-time) start) 2))
        (should (string-search "Timed out" logged))
        (should (string-search (buffer-name) logged))))))

(ert-deftest magento-cache-clean-test-stop-interrupt-failure ()
  "Stopping a watcher that can't be interrupted is a user error."
  (magento-cache-clean-test--with-watchers (a)
    (let ((buf (magento-cache-clean-test--watch-buffer a))
          (inhibit-message t))
      (cl-letf (((symbol-function 'interrupt-process)
                 (lambda (&rest _) (error "Connection lost"))))
        (should-error (magento-cache-clean-watch-stop buf) :type 'user-error))
      (should (process-live-p (get-buffer-process buf))))))

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
  "One-shot buffers take their error regexps from the user option."
  (should (custom-variable-p 'magento-cache-clean-error-regexp-alist))
  (let ((magento-cache-clean-error-regexp-alist '(("^ERR \\(.+\\)$" 1))))
    (with-temp-buffer
      (magento-cache-clean-compilation-mode)
      (should (equal magento-cache-clean-error-regexp-alist
                     compilation-error-regexp-alist)))))

;;; Safe local variables

(ert-deftest magento-cache-clean-test-menu-old-name ()
  "The menu's old name still opens it, as an obsolete alias."
  (should (eq (indirect-function 'magento-cache-clean-transient)
              (indirect-function 'magento-cache-clean-menu)))
  (should (get 'magento-cache-clean-transient 'byte-obsolete-info)))

(ert-deftest magento-cache-clean-test-compilation-mode-old-name ()
  "The one-shot mode's old name and hook are obsolete aliases."
  (should (eq (indirect-function 'magento-cache-clean-mode)
              (indirect-function 'magento-cache-clean-compilation-mode)))
  (should (get 'magento-cache-clean-mode 'byte-obsolete-info))
  (should (eq 'magento-cache-clean-compilation-mode-hook
              (indirect-variable 'magento-cache-clean-mode-hook))))

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

(ert-deftest magento-cache-clean-test-directory-safe-local-same-host ()
  "Directory is only safe as a dir-local on the buffer's own host."
  (let ((default-directory "/"))
    (should-not (safe-local-variable-p 'magento-cache-clean-directory
                                       "/sudo::/path/to/magento/"))
    (should-not (safe-local-variable-p 'magento-cache-clean-directory
                                       "/ssh:other:/srv/magento/")))
  ;; A remote .dir-locals.el may name a root on its own host.
  (let ((default-directory "/ssh:host:/srv/magento/app/"))
    (should (safe-local-variable-p 'magento-cache-clean-directory
                                   "/ssh:host:/srv/magento/"))
    (should-not (safe-local-variable-p 'magento-cache-clean-directory
                                       "/srv/magento/"))
    (should-not (safe-local-variable-p 'magento-cache-clean-directory
                                       "/ssh:other:/srv/magento/"))))

(provide 'magento-cache-clean-test)
;;; magento-cache-clean-test.el ends here
