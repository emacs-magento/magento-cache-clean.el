;;; magento-cache-clean-docker-test.el --- Remote tests for magento-cache-clean -*- lexical-binding: t; -*-

;;; Commentary:

;; ERT tests running the real cache-clean in a Docker container,
;; reached through Tramp's docker method as a remote Magento root.
;; The image is built from test/docker on first use.
;;
;; The tests are tagged `docker' and skipped when Docker isn't
;; available, unless MAGENTO_CACHE_CLEAN_REQUIRE_DOCKER is set, as in
;; CI, where they fail instead.

;;; Code:

(require 'ert)
(require 'tramp)
(require 'magento-cache-clean)

(defconst magento-cache-clean-docker-test--dir
  (expand-file-name "docker/" (file-name-directory
                               (or load-file-name buffer-file-name)))
  "Directory with the test image's Dockerfile.")

(defvar magento-cache-clean-docker-test--available 'unknown
  "Whether Docker can run containers, once checked.")

(defun magento-cache-clean-docker-test--docker (&rest args)
  "Run docker with ARGS and return its output.
Signal an error with the output when it fails."
  (with-temp-buffer
    (let ((status (apply #'call-process "docker" nil t nil args)))
      (unless (eql status 0)
        (error "docker %s failed: %s" (string-join args " ") (buffer-string)))
      (string-trim (buffer-string)))))

(defun magento-cache-clean-docker-test--require ()
  "Skip the current test unless Docker can run containers.
When MAGENTO_CACHE_CLEAN_REQUIRE_DOCKER is set, fail instead."
  (when (eq magento-cache-clean-docker-test--available 'unknown)
    (setq magento-cache-clean-docker-test--available
          (and (executable-find "docker")
               (eql 0 (call-process "docker" nil nil nil "info")))))
  (unless magento-cache-clean-docker-test--available
    (if (getenv "MAGENTO_CACHE_CLEAN_REQUIRE_DOCKER")
        (error "Docker is required but not available")
      (ert-skip "Docker is not available"))))

(defun magento-cache-clean-docker-test--image ()
  "Return the test image, building it when it doesn't exist yet.
The tag is a hash of the image's files, so changing them rebuilds it."
  (let* ((dir magento-cache-clean-docker-test--dir)
         (image (format "magento-cache-clean-test:%s"
                        (substring
                         (secure-hash
                          'sha1
                          (mapconcat (lambda (file)
                                       (with-temp-buffer
                                         (insert-file-contents
                                          (expand-file-name file dir))
                                         (buffer-string)))
                                     '("Dockerfile" "env.php")))
                         0 12))))
    (unless (eql 0 (call-process "docker" nil nil nil "image" "inspect" image))
      (magento-cache-clean-docker-test--docker "build" "-q" "-t" image dir))
    image))

(defmacro magento-cache-clean-docker-test--with-root (var &rest body)
  "Run BODY with VAR bound to the Magento root of a new container.
VAR is a Tramp file name.  The options are reset to their defaults,
and the container and its watch buffers are removed afterwards."
  (declare (indent 1))
  `(progn
     (magento-cache-clean-docker-test--require)
     (let* ((name (format "magento-cache-clean-test-%s-%d"
                          (emacs-pid) (random 1000000)))
            (,var (format "/docker:%s:/srv/magento/" name))
            (tramp-verbose 0)
            (magento-cache-clean-executable 'auto)
            (magento-cache-clean-directory nil)
            (magento-cache-clean-watch-query-on-exit nil)
            (kill-emacs-hook nil))
       (magento-cache-clean-docker-test--docker
        "run" "-d" "--name" name (magento-cache-clean-docker-test--image))
       (unwind-protect
           (progn ,@body)
         ;; Before Tramp's connection buffers, which interrupting needs.
         (dolist (buf (buffer-list))
           (when (and (string-prefix-p "*magento-cache-clean" (buffer-name buf))
                      (string-search name (buffer-name buf)))
             (let ((kill-buffer-query-functions nil))
               (kill-buffer buf))))
         (tramp-cleanup-connection (tramp-dissect-file-name ,var) nil t)
         (call-process "docker" nil nil nil "rm" "-f" name)))))

(defun magento-cache-clean-docker-test--wait-until (predicate)
  "Wait up to 60 seconds for PREDICATE to return non-nil, and return it."
  (let ((deadline (+ (float-time) 60))
        value)
    (while (and (not (setq value (funcall predicate)))
                (< (float-time) deadline))
      (accept-process-output nil 0.1))
    value))

(defun magento-cache-clean-docker-test--count (buffer regexp)
  "Return how many times REGEXP matches in BUFFER."
  (with-current-buffer buffer
    (count-matches regexp (point-min) (point-max))))

(defun magento-cache-clean-docker-test--wait-for (buffer regexp &optional n)
  "Wait for REGEXP to appear N times, default once, in BUFFER.
Fail with BUFFER's content when it doesn't."
  (unless (magento-cache-clean-docker-test--wait-until
           (lambda ()
             (>= (magento-cache-clean-docker-test--count buffer regexp)
                 (or n 1))))
    (ert-fail (list "Output not found" regexp
                    (with-current-buffer buffer (buffer-string))))))

(defun magento-cache-clean-docker-test--watchers (root)
  "Return the number of cache-clean watchers running in ROOT's container."
  (let ((container (tramp-file-name-host (tramp-dissect-file-name root))))
    (with-temp-buffer
      (call-process "docker" nil t nil "exec" container
                    "pgrep" "-fc" "cache-clean.js -d .* --watch")
      (string-to-number (buffer-string)))))

(defun magento-cache-clean-docker-test--watch (root)
  "Start a watcher for ROOT from a buffer in it, and return its buffer.
Wait until it watches."
  (let ((buf (with-temp-buffer
               (setq default-directory root)
               (magento-cache-clean-watch))))
    (magento-cache-clean-docker-test--wait-for buf "Watcher initialized")
    buf))

;;; Tests

(ert-deftest magento-cache-clean-docker-test-auto-detect ()
  "Auto-detection finds the vendor script and node on the remote host."
  :tags '(docker)
  (magento-cache-clean-docker-test--with-root root
    (should (equal "vendor/bin/cache-clean.js"
                   (magento-cache-clean--executable root)))))

(ert-deftest magento-cache-clean-docker-test-cache-types ()
  "Cache types are completed from the remote env.php."
  :tags '(docker)
  (magento-cache-clean-docker-test--with-root root
    (should (equal '("config" "layout" "full_page" "my_custom")
                   (magento-cache-clean--env-cache-types root)))))

(ert-deftest magento-cache-clean-docker-test-one-shot ()
  "One-shot cleaning runs cache-clean on the remote host."
  :tags '(docker)
  (magento-cache-clean-docker-test--with-root root
    (let ((buf (with-temp-buffer
                 (setq default-directory root)
                 (magento-cache-clean '("config" "my_custom")))))
      (magento-cache-clean-docker-test--wait-for
       buf "Cleaning cache type(s) config")
      (magento-cache-clean-docker-test--wait-for
       buf "^MagentoCacheClean finished"))))

(ert-deftest magento-cache-clean-docker-test-one-shot-file-list ()
  "A remote --file-list is passed as the file's local name."
  :tags '(docker)
  (magento-cache-clean-docker-test--with-root root
    (let ((buf (with-temp-buffer
                 (setq default-directory root)
                 (magento-cache-clean
                  (magento-cache-clean--transient-to-cli
                   (list (concat "--file-list=" root "app/etc/env.php")))))))
      (magento-cache-clean-docker-test--wait-for
       buf "--file-list /srv/magento/app/etc/env.php")
      (magento-cache-clean-docker-test--wait-for
       buf "^MagentoCacheClean finished"))))

(ert-deftest magento-cache-clean-docker-test-watch-hotkey ()
  "A key typed in the watch buffer reaches the remote watcher."
  :tags '(docker)
  (magento-cache-clean-docker-test--with-root root
    (let ((buf (magento-cache-clean-docker-test--watch root)))
      (with-current-buffer buf
        (let ((last-command-event ?c))
          (call-interactively #'magento-cache-clean-watch-send-key)))
      (magento-cache-clean-docker-test--wait-for
       buf "Cleaning cache type(s) config"))))

(ert-deftest magento-cache-clean-docker-test-watch-stop ()
  "Stopping a watcher ends it on the remote host."
  :tags '(docker)
  (magento-cache-clean-docker-test--with-root root
    (let ((buf (magento-cache-clean-docker-test--watch root)))
      (should (= 1 (magento-cache-clean-docker-test--watchers root)))
      (magento-cache-clean-watch-stop buf)
      (should (magento-cache-clean-docker-test--wait-until
               (lambda ()
                 (zerop (magento-cache-clean-docker-test--watchers root))))))))

(ert-deftest magento-cache-clean-docker-test-watch-restart ()
  "Restarting leaves a single watcher on the remote host."
  :tags '(docker)
  (magento-cache-clean-docker-test--with-root root
    (let ((buf (magento-cache-clean-docker-test--watch root)))
      (with-current-buffer buf
        (magento-cache-clean-watch-restart))
      (magento-cache-clean-docker-test--wait-for buf "Watcher initialized" 2)
      (should (= 1 (magento-cache-clean-docker-test--watchers root))))))

(ert-deftest magento-cache-clean-docker-test-watch-kill-buffer ()
  "Killing the watch buffer ends the watcher on the remote host."
  :tags '(docker)
  (magento-cache-clean-docker-test--with-root root
    (let ((buf (magento-cache-clean-docker-test--watch root)))
      (let ((kill-buffer-query-functions nil))
        (kill-buffer buf))
      (should (magento-cache-clean-docker-test--wait-until
               (lambda ()
                 (zerop (magento-cache-clean-docker-test--watchers root))))))))

(ert-deftest magento-cache-clean-docker-test-kill-emacs ()
  "Exiting Emacs ends the watchers on the remote host."
  :tags '(docker)
  (magento-cache-clean-docker-test--with-root root
    (magento-cache-clean-docker-test--watch root)
    (run-hooks 'kill-emacs-hook)
    (should (magento-cache-clean-docker-test--wait-until
             (lambda ()
               (zerop (magento-cache-clean-docker-test--watchers root)))))))

(ert-deftest magento-cache-clean-docker-test-dir-locals ()
  "A remote root's directory local executable survives a restart."
  :tags '(docker)
  (magento-cache-clean-docker-test--with-root root
    (write-region "((nil . ((magento-cache-clean-executable . \"cache-clean\"))))\n"
                  nil (expand-file-name ".dir-locals.el" root))
    (let* ((enable-local-variables :safe)
           (enable-remote-dir-locals t)
           (file (find-file-noselect (expand-file-name "app/etc/env.php" root)))
           (buf (with-current-buffer file
                  (magento-cache-clean-watch))))
      (unwind-protect
          (progn
            (magento-cache-clean-docker-test--wait-for buf "cache-clean on PATH")
            (with-current-buffer buf
              (magento-cache-clean-watch-restart))
            (magento-cache-clean-docker-test--wait-for
             buf "cache-clean on PATH" 2))
        (kill-buffer file)))))

(provide 'magento-cache-clean-docker-test)
;;; magento-cache-clean-docker-test.el ends here
