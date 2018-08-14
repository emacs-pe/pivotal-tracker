;; -*- lexical-binding: t -*-

(defvar pivotal-projects-test-data)

(pivotal-json-api (pivotal-v5-url "projects" "?fields=id,name,current_velocity,current_volatility") "GET"
                  (lambda (status)
                    (let ((json (progn
                                  (goto-char url-http-end-of-headers)
                                  (json-read))))
                      (setq pivotal-projects-test-data json)
                      json)))

(with-current-buffer (get-buffer-create "*pivotal-projects*")
  (let ((inhibit-read-only t))
    (pivotal-project-mode)
    (erase-buffer)
    ;; (switch-to-buffer (current-buffer))
    (pivotal-insert-projects pivotal-projects-test-data)))

