;; -*- lexical-binding: t -*-

(defvar pivotal-projects-test-data)

(pivotal-json-api (pivotal-v5-url "projects" "?fields=id,name,current_velocity,current_volatility,current_iteration_number") "GET"
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
    (pivotal-insert-projects pivotal-projects-test-data)
    (goto-char (point-min))
    (pivotal-forward-project-entry)))


(defvar pivotal-iterations-test-data)
(pivotal-json-api (pivotal-v5-url "projects"
                                  (number-to-string
                                   (cdr (assoc 'id
                                               (aref pivotal-projects-test-data 0))))
                                  "iterations" "?scope=current")
                  "GET"
                  (lambda (status)
                    (message "Status: %s" status)
                    (let ((json (progn
                                  (goto-char url-http-end-of-headers)
                                  (json-read))))
                      (setq pivotal-iteration-test-data
                            json)
                      json)))

(with-current-buffer (get-buffer-create "*pivotal-iteration*")
  (let ((inhibit-read-only t))
    (pivotal-mode)
    (erase-buffer)
    ;; (switch-to-buffer (current-buffer))
    (pivotal-insert-iteration pivotal-iterations-test-data)))

(let ((iteration (aref pivotal-iterations-test-data 0)))
  (values (assoc 'start iteration)
          (assoc 'finish iteration)))

(let ((stories (cdr (assoc 'stories (aref pivotal-iterations-test-data 0)))))
  (assoc 'current_state (aref stories 0)))
(length pivotal-iterations-test-data)
;; Test for non-existent iterations
