;;; magento-cache-clean-test.el --- Tests for magento-cache-clean -*- lexical-binding: t; -*-

;;; Commentary:

;; ERT tests for magento-cache-clean.

;;; Code:

(require 'ert)
(require 'magento-cache-clean)

;;; Build args tests

(ert-deftest magento-cache-clean-test-build-args-empty ()
  "Build args with no options produces empty list."
  (should (equal '() (magento-cache-clean--build-args nil nil nil nil nil))))

(ert-deftest magento-cache-clean-test-build-args-verbose ()
  "Build args with verbose verbosity."
  (should (equal '("--verbose")
                 (magento-cache-clean--build-args "verbose" nil nil nil nil))))

(ert-deftest magento-cache-clean-test-build-args-debug ()
  "Build args with debug verbosity."
  (should (equal '("--debug")
                 (magento-cache-clean--build-args "debug" nil nil nil nil))))

(ert-deftest magento-cache-clean-test-build-args-silent ()
  "Build args with silent verbosity."
  (should (equal '("--silent")
                 (magento-cache-clean--build-args "silent" nil nil nil nil))))

(ert-deftest magento-cache-clean-test-build-args-keep-generated ()
  "Build args with keep-generated flag."
  (should (equal '("--keep-generated")
                 (magento-cache-clean--build-args nil t nil nil nil))))

(ert-deftest magento-cache-clean-test-build-args-no-flood-guard ()
  "Build args with no-flood-guard flag."
  (should (equal '("--no-flood-guard")
                 (magento-cache-clean--build-args nil nil t nil nil))))

(ert-deftest magento-cache-clean-test-build-args-watch ()
  "Build args with watch flag."
  (should (equal '("--watch")
                 (magento-cache-clean--build-args nil nil nil t nil))))

(ert-deftest magento-cache-clean-test-build-args-cache-types ()
  "Build args with cache types appended at the end."
  (should (equal '("config" "layout")
                 (magento-cache-clean--build-args nil nil nil nil '("config" "layout")))))

(ert-deftest magento-cache-clean-test-build-args-all-options ()
  "Build args with all options combined."
  (should (equal '("--verbose" "--keep-generated" "--no-flood-guard" "--watch"
                   "config" "full_page")
                 (magento-cache-clean--build-args "verbose" t t t '("config" "full_page")))))

(ert-deftest magento-cache-clean-test-build-args-flags-before-types ()
  "Flags appear before cache types."
  (let ((args (magento-cache-clean--build-args "debug" t nil nil '("layout"))))
    (should (equal '("--debug" "--keep-generated" "layout") args))))

;;; Directory detection tests

(ert-deftest magento-cache-clean-test-detect-directory-found ()
  "Detect directory when app/etc/env.php exists."
  (let ((temp-dir (make-temp-file "magento-test" t)))
    (unwind-protect
        (progn
          (make-directory (expand-file-name "app/etc" temp-dir) t)
          (write-region "" nil (expand-file-name "app/etc/env.php" temp-dir))
          (let ((default-directory (file-name-as-directory temp-dir)))
            (should (equal (file-name-as-directory temp-dir)
                           (magento-cache-clean--detect-directory)))))
      (delete-directory temp-dir t))))

(ert-deftest magento-cache-clean-test-detect-directory-from-subdirectory ()
  "Detect directory from a subdirectory of the Magento root."
  (let ((temp-dir (make-temp-file "magento-test" t)))
    (unwind-protect
        (progn
          (make-directory (expand-file-name "app/etc" temp-dir) t)
          (write-region "" nil (expand-file-name "app/etc/env.php" temp-dir))
          (make-directory (expand-file-name "app/code/Vendor/Module" temp-dir) t)
          (let ((default-directory
                 (file-name-as-directory
                  (expand-file-name "app/code/Vendor/Module" temp-dir))))
            (should (equal (file-name-as-directory temp-dir)
                           (magento-cache-clean--detect-directory)))))
      (delete-directory temp-dir t))))

(ert-deftest magento-cache-clean-test-detect-directory-not-found ()
  "Return nil when not in a Magento project."
  (let ((temp-dir (make-temp-file "not-magento" t)))
    (unwind-protect
        (let ((default-directory (file-name-as-directory temp-dir)))
          (should-not (magento-cache-clean--detect-directory)))
      (delete-directory temp-dir t))))

(ert-deftest magento-cache-clean-test-directory-uses-custom ()
  "Use custom directory when set."
  (let ((temp-dir (make-temp-file "magento-test" t)))
    (unwind-protect
        (let ((magento-cache-clean-directory temp-dir))
          (should (equal (file-name-as-directory (expand-file-name temp-dir))
                         (magento-cache-clean--directory))))
      (delete-directory temp-dir t))))

;;; Transient arg parsing tests

(ert-deftest magento-cache-clean-test-parse-args-empty ()
  "Parse empty args."
  (let ((parsed (magento-cache-clean--transient-parse-args nil)))
    (should-not (plist-get parsed :verbosity))
    (should-not (plist-get parsed :keep-generated))
    (should-not (plist-get parsed :no-flood-guard))
    (should-not (plist-get parsed :cache-types))))

(ert-deftest magento-cache-clean-test-parse-args-verbosity ()
  "Parse verbosity arg."
  (let ((parsed (magento-cache-clean--transient-parse-args '("--verbose"))))
    (should (equal "verbose" (plist-get parsed :verbosity)))))

(ert-deftest magento-cache-clean-test-parse-args-flags ()
  "Parse flag args."
  (let ((parsed (magento-cache-clean--transient-parse-args
                 '("--keep-generated" "--no-flood-guard"))))
    (should (plist-get parsed :keep-generated))
    (should (plist-get parsed :no-flood-guard))))

(ert-deftest magento-cache-clean-test-parse-args-cache-types ()
  "Parse cache types arg."
  (let ((parsed (magento-cache-clean--transient-parse-args '("--types=config,layout"))))
    (should (equal '("config" "layout") (plist-get parsed :cache-types)))))

(ert-deftest magento-cache-clean-test-parse-args-combined ()
  "Parse combined args."
  (let ((parsed (magento-cache-clean--transient-parse-args
                 '("--debug" "--keep-generated" "--types=full_page"))))
    (should (equal "debug" (plist-get parsed :verbosity)))
    (should (plist-get parsed :keep-generated))
    (should-not (plist-get parsed :no-flood-guard))
    (should (equal '("full_page") (plist-get parsed :cache-types)))))

;;; Buffer naming tests

(ert-deftest magento-cache-clean-test-buffer-name-no-project ()
  "Buffer name without a project."
  (let ((default-directory (make-temp-file "no-project" t)))
    (unwind-protect
        (progn
          (should (equal "*magento-cache-clean*"
                         (magento-cache-clean--buffer-name)))
          (should (equal "*magento-cache-clean-watch*"
                         (magento-cache-clean--watch-buffer-name))))
      (delete-directory default-directory t))))

(ert-deftest magento-cache-clean-test-buffer-name-with-project ()
  "Buffer name includes project name."
  (let ((temp-dir (make-temp-file "my-project" t)))
    (unwind-protect
        (progn
          (make-directory (expand-file-name ".git" temp-dir))
          (let ((default-directory (file-name-as-directory temp-dir)))
            (should (string-match-p "\\*magento-cache-clean\\[.*\\]\\*"
                                    (magento-cache-clean--buffer-name)))
            (should (string-match-p "\\*magento-cache-clean-watch\\[.*\\]\\*"
                                    (magento-cache-clean--watch-buffer-name)))))
      (delete-directory temp-dir t))))

(ert-deftest magento-cache-clean-test-watch-not-running ()
  "Watch not running when no buffer exists."
  (let ((buf-name (magento-cache-clean--watch-buffer-name)))
    (when (get-buffer buf-name)
      (kill-buffer buf-name))
    (should-not (magento-cache-clean--watch-running-p))))

;;; Cache types constant tests

(ert-deftest magento-cache-clean-test-cache-types-non-empty ()
  "Cache types list is non-empty."
  (should (consp magento-cache-clean-types)))

(ert-deftest magento-cache-clean-test-cache-types-all-strings ()
  "All cache types are strings."
  (dolist (ct magento-cache-clean-types)
    (should (stringp ct))))

(ert-deftest magento-cache-clean-test-cache-types-no-duplicates ()
  "No duplicate cache types."
  (should (equal (length magento-cache-clean-types)
                 (length (delete-dups (copy-sequence magento-cache-clean-types))))))

;;; Safe local variable tests

(ert-deftest magento-cache-clean-test-executable-safe-local ()
  "Executable is safe as a dir-local when string."
  (should (safe-local-variable-p 'magento-cache-clean-executable "cache-clean"))
  (should (safe-local-variable-p 'magento-cache-clean-executable "/usr/bin/cache-clean"))
  (should-not (safe-local-variable-p 'magento-cache-clean-executable 42)))

(ert-deftest magento-cache-clean-test-directory-safe-local ()
  "Directory is safe as a dir-local when string or nil."
  (should (safe-local-variable-p 'magento-cache-clean-directory nil))
  (should (safe-local-variable-p 'magento-cache-clean-directory "/path/to/magento"))
  (should-not (safe-local-variable-p 'magento-cache-clean-directory 42)))

(provide 'magento-cache-clean-test)
;;; magento-cache-clean-test.el ends here
