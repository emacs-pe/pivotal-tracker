;;; pivotal-tracker.el --- Interact with Pivotal Tracker through its API -*- lexical-binding: t -*-

;; Author: John Andrews
;; URL: http://github.com/jxa/pivotal-tracker
;; Created: 2010.11.14
;; Version: 1.3.1

;; This file is not part of GNU Emacs.

;;; License

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation version 2, or any later version.

;; This program is distributed in the hope that it will be useful, but
;; WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;; General Public License for more details.

;; For a copy of the GNU General Public License, search the Internet, or
;; write to the Free Software Foundation, Inc., 59 Temple Place, Suite
;; 330, Boston, MA 02111-1307 USA

;;; Commentary:

;; Pivotal Tracker Mode provides a mode and functions for interacting
;; with Pivotal Tracker from Emacs.  It is designed to give most of the
;; functionality that is important to a developer.

;; Before using pivotal-tracker.el you must save your pivotal API key
;; under ~/.authinfo.gpg. See the README for further instructions.

;;; Code:

(eval-when-compile
  (require 'subr-x))
(require 'xml)
(require 'url)
(require 'json)
(require 'auth-source)
(require 'magit-popup)


(defgroup pivotal nil
  "Pivotal Tracker"
  :group 'external)

(defvar pivotal-api-token nil
  "The cache of the pivotal API key. The API key found on the
  /profile page of pivotal tracker")

(defconst pivotal-base-url "https://www.pivotaltracker.com/services/v3"
  "Format string to use when creating endpoint urls.")

(defconst pivotal-states '("unstarted" "started" "finished" "delivered" "accepted" "rejected")
  "Story status will be one of these values.")

(defconst pivotal-current-iteration-number -1)

(defvar *pivotal-current-project*)
(defvar *pivotal-iteration* pivotal-current-iteration-number)

(defun pivotal-api-token ()
  "Return the pivotal API key from the user."
  (or pivotal-api-token
      (getenv "PIVOTAL_API_TOKEN")
      (let* ((pivotal-auth-info
              (car (auth-source-search :max 1
                                       :host "api.pivotaltracker.com"
                                       :require '(:host))))
             (pivotal-api-token-fn (getf pivotal-auth-info :secret)))
        (setq pivotal-api-token
              (funcall pivotal-api-token-fn))
        pivotal-api-token)
      (error "You need to generate a personal access token.")))

(defface pivotal-title-face
  '((t :height 1.2 :underline t))
  "Face for iteration heading"
  :group 'pivotal)

(defface pivotal-section-face
  '((t :underline t))
  "Face for iteration heading"
  :group 'pivotal)

(defvar project-entry-button-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "i") 'pivotal-list-project-iterations)
    map))

(define-button-type 'project-entry
  'action 'pivotal-open-project
  'follow-link nil ; What is this?
  'project-id nil
  'keymap project-entry-button-map
  'help-echo "View the project's current iteration.")


;;; Utils

(defsubst assocdr (key list &optional testfn)
  (cdr (assoc key list testfn)))


;;;; INTERACTIVE USER FUNCTIONS

;;;###autoload
(defun pivotal ()
  "Launch pivotal-projects window, or just switch to it."
  (interactive)
  (if-let ((buffer (get-buffer "*pivotal-projects*")))
      (switch-to-buffer buffer)
    (pivotal-get-projects)))

;;;###autoload
(defun pivotal-get-projects ()
  "Show a buffer of all projects you have access to."
  (interactive)
  (pivotal-json-api (pivotal-v5-url "projects" "?fields=id,name,current_velocity,current_volatility,current_iteration_number")
                    "GET"
                    'pivotal-projects-callback))

(defun pivotal-forward-project-entry ()
  (interactive)
  (forward-button 1 t))

(defun pivotal-backward-project-entry ()
  (interactive)
  (backward-button 1 t))

(defun pivotal-insert-iterations (iterations)
  ;; What do we have to show?
  ;; Iteration number (property)
  ;; Project-name for header
  ;; Project-id (proprety)
  ;; Start period
  ;; End Period
  ;; Points?
  ;; # Stories?
  ;; Team strength (it is a floating point, present it as a %)
  (let ((header "From Until Points Strength # of Stories"))
    (insert header)
    (insert
     (mapconcat 'pivotal-insert-iteration-entry
                iterations
                "\n")))
  (message iterations)
    )

(defun pivotal-list-project-iterations-callback (status)
  (let ((json (progn
                (goto-char url-http-end-of-headers)
                (json-read))))
    ;; TODO: Include the project name in the buffer name.
    (with-current-buffer (get-buffer-create "pivotal-iterations*")
      (pivotal-iteration-list-mode) ;; Check major-mode first?
      (let ((inhibit-read-only t))
        (erase-buffer)
        (pivotal-insert-iterations (reverse json)))
      (goto-char (point-min))
      (forward-line 1)
      (switch-to-buffer (current-buffer)))))

(defun pivotal-list-project-iterations ()
  (interactive)
  (let* ((button (button-at (point)))
         (project-id (button-get button :project-id)))
    ;; Iterations are returned oldest first. So when before inserting we need
    ;; to do from the end of the vector to the first.
    (pivotal-json-api (pivotal-v5-url "projects" project-id "iterations" "?fields=number,project_id,team_strength,start,finish,points,kind")
                      "GET"
                      'pivotal-list-project-iterations-callback)))

(defun pivotal-get-current ()
  "Show a buffer of all stories in the currently selected iteration."
  (interactive)
  (pivotal-get-iteration *pivotal-iteration*))

(defun pivotal-get-iteration (iteration)
  "Get the current project ITERATION."
  (let ((query-string (if (= pivotal-current-iteration-number iteration)
                          "?scope=current"
                        (format "?offset=%s&limit=1" iteration))))
    (pivotal-json-api (pivotal-url "projects" *pivotal-current-project* "iterations" query-string)
                 "GET"
                 'pivotal-iteration-callback)))

(defun pivotal-next-iteration ()
  "Replace iteration view with the next upcoming iteration."
  (interactive)
  (setq *pivotal-iteration* (1+ *pivotal-iteration*))
  (pivotal-get-iteration *pivotal-iteration*))

(defun pivotal-previous-iteration ()
  "Replace iteration view with previous iteration.
If you try to go before 0 it just reloads current."
  (interactive)
  (setq *pivotal-iteration*
        (if (= pivotal-current-iteration-number *pivotal-iteration*)
            pivotal-current-iteration-number
          (1- *pivotal-iteration*)))
  (pivotal-get-iteration *pivotal-iteration*))

(defun pivotal-open-project (button)
  "Set the current project, and open the current iteration for
that project."
  (interactive)
  (setq *pivotal-current-project* (button-get button :project-id))
  (setq *pivotal-iteration* pivotal-current-iteration-number)
  (pivotal-get-current))

(defun pivotal-get-story (id)
  "Open a single story (ID) for view / edit."
  (interactive)
  (pivotal-api (pivotal-url "projects" *pivotal-current-project* "stories" id)
               "GET"
               'pivotal-story-callback))

(defun pivotal-toggle-visibility ()
  "Show/Hide story detail."
  (interactive)
  (progn
    (if-let ((cur-invisible (member (pivotal-story-at-point)
                                    buffer-invisibility-spec)))
        (pivotal-show)
      (pivotal-hide))
    (force-window-update (current-buffer))))

(defun pivotal-estimate-story (estimate)
  "Assign an ESTIMATE to the story on the current line."
  (interactive "NEstimate: ")
  (message "going to set estimate to %s" estimate)
  (pivotal-api (pivotal-url "projects" *pivotal-current-project* "stories" (pivotal-story-id-at-point))
             "PUT"
             'pivotal-update-current-story
             (format "<story><estimate>%s</estimate></story>" estimate)))

(defun pivotal-set-status ()
  "Transition status according to the current status.  Assigns
the story to user."
  (interactive)
  (let ((new-state (completing-read "Status: " pivotal-states nil t)))
    (pivotal-api (pivotal-url "projects" *pivotal-current-project* "stories" (pivotal-story-id-at-point))
                 "PUT"
                 'pivotal-update-current-story
                 (format "<story><current_state>%s</current_state></story>" new-state))))

(defun pivotal-set-owner (new-owner-id)
  "Set owner (NEW-OWNER-ID) for the current story."
  (interactive
   (let ((member-name-id-alist (pivotal-project->member-name-id-alist *pivotal-current-project*)))
     (list (assocdr (completing-read "New owner: "
                                     member-name-id-alist
                                     nil
                                     t
                                     nil
                                     'pivotal-story-owner-history)
                    member-name-id-alist))))
  (pivotal-api (pivotal-url "projects" *pivotal-current-project* "stories" (pivotal-story-id-at-point))
         "PUT"
         'pivotal-update-current-story
         (format "<story><owned_by_id>%s</owned_by_id></story>" new-owner-id)))

(defun pivotal-add-comment (comment)
  "Prompt user for COMMENT and add it to the current story."
  (interactive "sAdd Comment: ")
  (pivotal-api (pivotal-url "projects" *pivotal-current-project* "stories" (pivotal-story-id-at-point) "notes")
               "POST"
               'pivotal-add-comment-callback
               (format "<note><text>%s</text></note>" (xml-escape-string comment))))

(defun pivotal-add-task (task)
  "Prompt user for a TASK and add it to the current story."
  (interactive "sAdd Task: ")
  (pivotal-api (pivotal-url "projects" *pivotal-current-project* "stories" (pivotal-story-id-at-point) "tasks")
               "POST"
               'pivotal-add-task-callback
               (format "<task><description>%s</description></task>" (xml-escape-string task))))

(defun pivotal-check-task ()
  "Mark current task as done."
  (interactive)
  (pivotal-api (pivotal-url "projects" *pivotal-current-project* "stories" (pivotal-story-id-at-point) "tasks" (pivotal-task-id-at-point))
               "PUT"
               'pivotal-check-task-callback
               (format "<task><complete>true</complete></task>")))

(defun pivotal-kill-ring-save-story-url ()
  "Save the external story URL as if killed, but don't kill anything."
  (interactive)
  (let ((story-url (pivotal-story-url-at-point)))
    (kill-new story-url)
    (message (concat "copied story URL to kill ring: " story-url))))

(defun pivotal-open-story-in-browser ()
  "Asks a WWW browser to load the story."
  (interactive)
  (browse-url (pivotal-story-url-at-point)))

(defun pivotal-open-current-project-in-browser ()
  "Asks a WWW browser to load the current project."
  (interactive)
  (browse-url (pivotal-get-project-url *pivotal-current-project*)))

(defun pivotal-open-project-at-point-in-browser ()
  "Asks a WWW browser to open the project at point."
  (interactive)
  (browse-url (pivotal-get-project-url (pivotal-project-id-at-point))))


;;;; CALLBACKS

(defun pivotal-iteration-callback (status)
  "Pivotal iteration callback handler (accept STATUS from response)."
  (let ((xml (pivotal-get-xml-from-current-buffer)))
    (with-current-buffer (get-buffer-create "*pivotal-iteration*")
      (let ((inhibit-read-only t))
        (pivotal-mode)
        (erase-buffer)
        (switch-to-buffer (current-buffer))

        ;; for some reason trying to load an iteration that doesn't
        ;; exist returns the following xml
        ;; ((nil-classes ((type . array)))
        ;;  - Peer has closed the GnuTLS connection
        ;;  )
        (if (eq 'nil-classes (first (first xml)))
            (insert "No stories in this iteration yet")
          (pivotal-insert-iteration xml))))))

(defun pivotal-projects-callback (status)
  "Pivotal projects callback handler (accept STATUS from response)."
  (let ((json (progn
                (goto-char url-http-end-of-headers)
                (json-read))))
    (with-current-buffer (get-buffer-create "*pivotal-projects*")
      (pivotal-project-mode)
      (let ((inhibit-read-only t))
        (erase-buffer)
        (pivotal-insert-projects json))
      (goto-char (point-min))
      (pivotal-forward-project-entry)
      (switch-to-buffer (current-buffer)))))

(defun pivotal-story-callback (status)
  "Pivotal story callback handler (accept STATUS from response)."
  (let ((xml (pivotal-get-xml-from-current-buffer)))
    (erase-buffer)
    (insert (pivotal-format-story xml)) (rename-buffer (concat "*pivotal-" (pivotal-story-attribute xml 'id) "*"))
    (switch-to-buffer (current-buffer))))

(defun pivotal-update-current-story (status)
  "Pivotal current story update callback handler (accept STATUS from response)."
  (let ((xml (pivotal-get-xml-from-current-buffer)))
    (if (eq :error (car status))
        (message "Error: %s" (pivotal-parse-errors xml))
      (with-current-buffer (get-buffer-create "*pivotal-iteration*")
        (pivotal-remove-story-at-point)
        (pivotal-insert-story xml)))))

(defun pivotal-add-comment-callback (status)
  "Pivotal add comment callback handler (accept STATUS from response)."
  (let* ((xml (pivotal-get-xml-from-current-buffer))
         (comment (pivotal-format-comment (car xml))))
    (with-current-buffer (get-buffer-create "*pivotal-iteration*")
      (pivotal-append-to-current-story comment))))

(defun pivotal-add-task-callback (status)
  "Pivotal add task callback handler (accept STATUS from response)."
  (let* ((xml (pivotal-get-xml-from-current-buffer))
         (task (pivotal-format-task (car xml))))
    (with-current-buffer (get-buffer-create "*pivotal-iteration*")
      (pivotal-append-task-to-current-story task))))

(defun pivotal-check-task-callback (status)
  "Pivotal check task callback handler (accept STATUS from response)."
  (let ((xml (pivotal-get-xml-from-current-buffer)))
    (if (eq :error (car status))
        (message "Error: %s" (pivotal-parse-errors xml))
      (with-current-buffer (get-buffer-create "*pivotal-iteration*")
        (let* ((task (car xml))
                (task-id (pivotal-element-value task 'id)))
          (save-excursion
            (goto-char (point-min))
            (re-search-forward (concat "ID:#" task-id))
            (beginning-of-line)
            ;; Looking at [ ]
            (forward-char 1)
            (delete-char 1)
            (insert "X")))))))

(defun pivotal-parse-errors (xml)
  "Parse Pivotal API errors from XML."
  (mapconcat (lambda (error)
               (car (last error)))
             (xml-get-children (car xml) 'error)
             " "))

;;;; MODE DEFINITIONS

(defconst pivotal-font-lock-keywords
  `(("^\\(\\[.*?\\]\\)+" 0 font-lock-doc-face)
    ("^-.*-$" . 'pivotal-title-face)
    ("^--- [a-zA-Z]+$" . 'pivotal-section-face)))


(magit-define-popup pivotal-link-popup
  "Popup for opening stories in a web browser"
  :actions '((?o "Current story" pivotal-open-story-in-browser)
             (?p "Current project" pivotal-open-current-project-in-browser)
             (?l "Copy current story URL" pivotal-kill-ring-save-story-url)))

(magit-define-popup pivotal-story-popup
  "Popup for interacting with stories"
  :actions '((?e "Estimate" pivotal-estimate-story)
             (?c "Comment" pivotal-add-comment)
             (?s "Set status" pivotal-set-status)
             (?o "Set owner" pivotal-set-owner)
             (?t "Add task" pivotal-add-task)
             (?v "Check task" pivotal-check-task)))

(magit-define-popup pivotal-dispatch-popup
  "Popup console for dispatching other popups"
  :actions '("Popup commands"
             (?o "Openening in a browser" pivotal-link-popup)
             (?s "Modifying stories" pivotal-story-popup)
             "\
g      refresh current buffer
TAB    toggle story details
+      add new story
N      next iteration
P      previous iteration
^      list all pivotal projects

C-h m  show all keybindings"))

(defvar pivotal-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "j") 'next-line)
    (define-key map (kbd "k") 'previous-line)
    (define-key map (kbd "?") 'pivotal-dispatch-popup)

    (define-key map (kbd "<tab>") 'pivotal-toggle-visibility)
    (define-key map (kbd "g") 'pivotal-get-current)
    ;; FIXME: Decide between u or l to return the the project list
    (define-key map (kbd "^") 'pivotal)
    (define-key map (kbd "+") 'pivotal-add-story)
    (define-key map (kbd "N") 'pivotal-next-iteration)
    (define-key map (kbd "P") 'pivotal-previous-iteration)

    ;; SubMenus
    (define-key map (kbd "o") 'pivotal-link-popup)
    (define-key map (kbd "s") 'pivotal-story-popup)
    map))

(define-derived-mode pivotal-mode special-mode "Pivotal"
  "Major mode to display the information of a specific
project. By default it shows the current iteration."
  (setq font-lock-defaults '(pivotal-font-lock-keywords))
  (font-lock-mode))

(defvar pivotal-project-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "g") 'pivotal-get-projects)
    (define-key map (kbd "C-n") 'pivotal-forward-project-entry)
    (define-key map (kbd "C-p") 'pivotal-backward-project-entry)
    (define-key map (kbd "j") 'pivotal-forward-project-entry)
    (define-key map (kbd "k") 'pivotal-backward-project-entry)

    (define-key map (kbd "o") 'pivotal-open-project-at-point-in-browser)
    map))

(define-derived-mode pivotal-project-mode special-mode "Pivotal Project List"
  "Major mode to display the list of pivotal projects.")

(define-derived-mode pivotal-iteration-list-mode special-mode "Pivotal Iteration List"
  "Display all the iterations associated with the current Project")


;;; SUPPORTING FUNS

(defun pivotal-url (&rest parts-of-url)
  "Build a Pivotal API URL from PARTS-OF-URL."
  (apply 'concat
         pivotal-base-url
         (mapcar (lambda (part) (concat "/" part)) parts-of-url)))

(defun pivotal-v5-url (&rest parts-of-url)
  "Build a Pivotal API (v5) URL from PARTS-OF-URL."
  (let ((v3-url (apply 'pivotal-url parts-of-url)))
   (replace-regexp-in-string "/v3/" "/v5/" v3-url)))

(defun pivotal-api (url method callback &optional xml-data)
  "Access wrapper for the the Pivotal API.

URL of the API endpoint
HTTP METHOD to use
CALLBACK func to handle request complete/fail

Optionally provide XML-DATA to send to the API endpoint."
  (let ((url-request-method method)
        (url-request-data xml-data)
        (url-request-extra-headers `(("X-TrackerToken" . ,(pivotal-api-token))
                                     ("Content-Type" . "application/xml"))))
    (url-retrieve url callback)))

(defun pivotal-json-api (url method &optional callback json-data)
  "Access wrapper for the Pivotal (v5) JSON API.

URL of the API endpoint
METHOD to use

Optional parameters:
provide JSON-DATA to send to the API endpoint.
CALLBACK func to handle request complete/fail"
  (let ((url-request-method method)
        (url-request-data json-data)
        (url-request-extra-headers `(("X-TrackerToken" . ,(pivotal-api-token))
                                     ("Content-Type" . "application/json"))))
    (if callback
        (url-retrieve url callback)
      (url-retrieve-synchronously url))))

(defun pivotal-get-project-members (project-id)
  "Get the project members (by PROJECT-ID)."
  (let ((response (pivotal-json-api (pivotal-v5-url "projects" project-id
                                                    "memberships" "?fields=person")
                                    "GET")))
    (with-current-buffer response
      ;; TODO: Error reporting.
      (goto-char url-http-end-of-headers)
      (mapcar (lambda (project-membership)
                (assocdr 'person project-membership))
              (json-read)))))

(defun pivotal-get-project (project-id)
  "Get the project (by PROJECT-ID)."
  (with-current-buffer (pivotal-json-api (pivotal-v5-url "projects" project-id)
                                         "GET")
    (let ((project (progn
                     (goto-char url-http-end-of-headers)
                     (json-read))))
      (if (eq :reissue project)
          (pivotal-get-project project-id)
        project))))

(defun pivotal-get-project-url (project-id)
  "Get the project URL (by PROJECT-ID)."
  (replace-regexp-in-string "/services/v3/" "/n/"
                            (pivotal-url
                             "projects" project-id)))

(defun pivotal-get-estimate-scale (project-id)
  "Get the project estimation scale (by PROJECT-ID)."
  (let* ((project             (pivotal-get-project project-id))
         (point-scale-str     (assocdr 'point_scale project))
         (estimate-scale-strs (split-string point-scale-str ",")))
    estimate-scale-strs))

(defvar pivotal-story-name-history ())

(defvar pivotal-story-description-history ())

(defvar pivotal-story-owner-history ())

(defvar pivotal-story-requester-history ())

(defvar pivotal-story-estimate-history ())

(defun pivotal-project-id-at-point ()
  "Find the Pivotal Tracker project at/after point."
  (number-to-string (get-text-property (point) 'project-id)))

(defun pivotal-project->member-name-id-alist (project-id)
  "Get the project member names for PROJECT-ID."
  (let ((project-members (pivotal-get-project-members project-id)))
    (mapcar (lambda (member)
              (let-alist member
                (cons .name .id)))
            project-members)))

(defun pivotal-add-story (name description owner-id requester-id estimate)
  "Add a story to the current project.

NAME of the story;
DESCRIPTION of the story (can be markdown formatted text);
OWNER-ID the Pivotal Tracker user id of the story owner (working on the story);
REQUESTER-ID the Pivotal Tracker user id of the story requester;
ESTIMATE the story points estimation."
  (interactive
   (let ((member-name-id-alist (pivotal-project->member-name-id-alist *pivotal-current-project*))
         (estimate-scale       (pivotal-get-estimate-scale *pivotal-current-project*)))
     (list (read-string "Name: " nil 'pivotal-story-name-history)
           (read-string "Description: " nil 'pivotal-story-description-history)
           (assocdr (completing-read "Owner: "
                                     member-name-id-alist
                                     nil
                                     t
                                     nil
                                     'pivotal-story-owner-history)
                    member-name-id-alist)
           (assocdr (completing-read "Requester: "
                                     member-name-id-alist
                                     nil
                                     t
                                     nil
                                     'pivotal-story-requester-history)
                    member-name-id-alist)
           (string-to-number (completing-read "Estimate: "
                                              estimate-scale
                                              nil
                                              t
                                              nil
                                              'pivotal-story-estimate-history)))))
  (kill-buffer (pivotal-json-api (pivotal-v5-url "projects" *pivotal-current-project* "stories")
                                 "POST"
                                 nil
                                 (json-encode (list :name            name
                                                    :description     description
                                                    :owned_by_id     owner-id
                                                    :requested_by_id requester-id
                                                    :estimate        estimate))))
  (message "Story added!"))

(defun pivotal-get-xml-from-current-buffer ()
  "Get Pivotal API XML from the current buffer."
  (let ((nb (get-buffer-create (make-temp-name "scratch")))
	(cb (current-buffer)))
    (decode-coding-region (point-min) (point-max) 'utf-8 nb)
    (with-current-buffer nb
      (kill-buffer cb)
      (let ((xml (if (functionp 'xml-parse-fragment)
		     (cdr (xml-parse-fragment))
		   (xml-parse-region))))
	(kill-buffer)
	xml))))

(defun pivotal-insert-projects (project-list-json)
  "Render projects one per line in their own buffer, from source
PROJECT-LIST-JSON."
  (let ((header (format "%-50s Velocity Volatility" "Project name")))
    (insert header) ; TODO: Figure out how to make text unreachable
    (mapcar (lambda (project)
            (let-alist project
              (let ((project-name (format "%-50s" .name))
                    (rest (format "%9s%10s%%"
                                  .current_velocity .current_volatility)))
                (insert "\n")
                (insert-text-button project-name :type 'project-entry
                                                 :project-id (number-to-string .id)
                                                 :iteration (number-to-string .current_iteration_number))
                (insert rest))))
            project-list-json)))

(defun pivotal-insert-iteration (iteration-xml)
  "Extract story information from the ITERATION-XML and insert it into current buffer."
  (insert (if (= pivotal-current-iteration-number *pivotal-iteration*)
              (format "- Current Iteration - Ending %s -\n"
                      (pivotal-iteration-date iteration-xml 'finish))
            (format "- Iteration Starting %s -\n"
                    (pivotal-iteration-date iteration-xml 'start))))
  (mapc 'pivotal-insert-story
        (pivotal-extract-stories-from-iteration-xml iteration-xml)))

(defun pivotal-insert-story (story)
  "Insert single STORY into current buffer."
  (let* ((start-point (point))
         (_ (insert (pivotal-format-story-oneline story)))
         (end-of-oneline (point))
         (_ (insert (pivotal-format-story story)))
         (end-of-detail (point)))
    (pivotal-mark-story start-point end-of-detail (pivotal-story-attribute story 'id))
    (pivotal-mark-invisibility end-of-oneline end-of-detail)
    (pivotal-hide end-of-oneline)))

(defun pivotal-append-to-current-story (text)
  "Append TEXT to the current story."
  (progn
    (pivotal-show)
    (let* ((story-id (pivotal-story-id-at-point (point)))
           (bounds (pivotal-story-boundaries (point)))
           (story-end (second bounds))
           (_ (goto-char story-end))
           (_ (insert text))
           (new-end (point)))
      (pivotal-mark-story story-end new-end story-id)
      (pivotal-mark-invisibility story-end new-end))))

(defun pivotal-append-task-to-current-story (task)
  "Append TASK to the current story."
  (progn
    (pivotal-show)
    (let* ((story-id (pivotal-story-id-at-point (point)))
            (bounds (pivotal-story-boundaries (point)))
            (story-beginning (first bounds)))
      (goto-char story-beginning)
      (re-search-forward "--- Comments")
      (forward-line -1)
      (let ((begin-of-task (point)))
        (insert task)
        ;; Mark this new line has belonging to the story
        (pivotal-mark-story begin-of-task (point) story-id)))))

(defun pivotal-invisibility-id (story-id)
  "Generate/Retrieve the symbol for the STORY-ID."
  (intern (concat "pivotal-" story-id)))

(defun pivotal-mark-story (min max story-id)
  "For region MIN - MAX, set the STORY-ID as a text property."
  (put-text-property min max 'pivotal-story-id story-id))

(defun pivotal-mark-invisibility (min max)
  "For region MIN - MAX, add an invisible overlay."
  (let ((overlay (make-overlay min max)))
    (overlay-put overlay 'invisible (pivotal-story-at-point min))))

(defun pivotal-hide (&optional position)
  "Hide the story at POSITION."
  (add-to-invisibility-spec (pivotal-story-at-point position)))

(defun pivotal-show (&optional position)
  "Show the story at POSITION."
  (remove-from-invisibility-spec (pivotal-story-at-point position)))

(defun pivotal-story-at-point (&optional position)
  "Get the story at POSITION, return the invisibility id."
  (let* ((buf-point (if position position (point)))
         (story-id (get-text-property buf-point 'pivotal-story-id))
         (invis-id (pivotal-invisibility-id story-id)))
    invis-id))

(defun pivotal-story-id-at-point (&optional position)
  "Get the story ID at POSITION."
  (let* ((story-sym (pivotal-story-at-point position))
         (story-str (symbol-name story-sym)))
    (string-match "pivotal-\\([0-9]+\\)" story-str)
    (match-string 1 story-str)))

(defun pivotal-story-url-at-point (&optional position)
  "Get the story URL at POSITION."
  (replace-regexp-in-string "/services/v3/" "/n/"
                            (pivotal-url
                             "projects" *pivotal-current-project*
                             "stories" (pivotal-story-id-at-point position))))

(defun pivotal-task-id-at-point (&optional position)
  "Get the task ID at POSITION."
  (save-excursion
    (beginning-of-line)
    (forward-char 4)
    (cond ((looking-at "Task")
            (re-search-forward "ID:#\\([0-9]\\)")
            (forward-char 3)
            (number-to-string (number-at-point)))
          (t (beep)
             (message "%s" "Could not find task at point")))))

(defun pivotal-format-story (story)
  "Format the STORY."
  (format "%s #%s
Status:       %s
Requested By: %s
Owned By:     %s
Labels:       %s

[Description]
%s

[Tasks]
%s

[Comments]
%s
"
          (pivotal-story-attribute story 'story_type)
          (pivotal-story-attribute story 'id)
          (pivotal-story-attribute story 'current_state)
          (pivotal-story-attribute story 'requested_by)
          (pivotal-story-attribute story 'owned_by)
          (pivotal-story-attribute story 'labels)
          (pivotal-story-attribute story 'description)
          (pivotal-tasks story)
          (pivotal-comments story)))

(defun pivotal-format-story-oneline (story)
  "Format STORY as one line."
  (let ((owner (pivotal-story-attribute story 'owned_by))
        (estimate (pivotal-story-attribute story 'estimate))
        (story-name (pivotal-story-attribute story 'name))
        (status (pivotal-story-attribute story 'current_state)))
    (format "[%4.4s][%1.1s][%9.9s] %.80s\n" owner estimate status story-name)))

(defun pivotal-remove-story-at-point ()
  "Delete all characters that belong to the current story.
Put point at the first char of the next story."
  (interactive)
  (let* ((bounds (pivotal-story-boundaries (point)))
         (first-point (first bounds))
         (last-point (second bounds)))
    (delete-region first-point last-point)
    (if (< (point) (point-max))
        (forward-char))))

(defun pivotal-story-boundaries (point)
  "Get char boundaries (min max) for the story at POINT."
  (let ((story-id (get-text-property (point) 'pivotal-story-id))
        (first-point (point))
        (last-point (point)))
    (while (pivotal-point-has-story-id (1- first-point) story-id)
      (setq first-point (1- first-point)))
    (while (pivotal-point-has-story-id (1+ last-point) story-id)
      (setq last-point (1+ last-point)))
    (list first-point last-point)))

(defun pivotal-point-has-story-id (point story-id)
  "Does the story at POINT have STORY-ID."
  (when (and (<= point (point-max)) (>= point (point-min)))
      (string-equal (get-text-property point 'pivotal-story-id) story-id)))

(defun pivotal-extract-stories-from-iteration-xml (iteration-xml)
  "Extract the story data from ITERATION-XML."
  (pivotal-xml-collection (car iteration-xml) `(iteration stories story)))

(defun pivotal-story-attribute (xml attribute)
  "Search given XML for the story ATTRIBUTE."
  (let*
      ((story (if (eq 'story (car xml))
                  xml
                (car xml)))
       (value (pivotal-element-value story attribute)))
    (if (symbolp value)
        (symbol-name value)
      value)))

(defun pivotal-element-value (xml element)
  "Search given XML for the ELEMENT value."
  (let ((node (xml-get-children xml element)))
    (caddar node)))

(defun pivotal-xml-collection (xml structure)
  "Return a collection of XML nodes found within STRUCTURE."
  (let ((results nil)
        (node xml))
    (mapc (lambda (element)
            (progn
              (setq results (xml-get-children node element))
              (setq node (first results))))
          structure)
    results))

(defun pivotal-iteration-date (xml attr)
  "Get the iteration date from XML ATTR."
  (first (split-string
          (third (first (pivotal-xml-collection (car xml) `(iteration ,attr))))
          " ")))

(defun pivotal-comments (story)
  "Get comments for the STORY."
  (let ((notes (pivotal-xml-collection story '(notes note))))
    (mapconcat #'pivotal-format-comment notes "")))

(defun pivotal-format-comment (note)
  "Format the NOTE as a comment."
  (let ((text (pivotal-element-value note 'text))
        (author (pivotal-element-value note 'author))
        (created-at (pivotal-element-value note 'noted_at)))
    (if created-at
        (setq created-at (substring created-at 5 10)))
    (format "%s  --  %s on %s\n" text author created-at)))

(defun pivotal-tasks (story)
  "Get the tasks for STORY."
  (let ((tasks (pivotal-xml-collection story `(tasks task))))
    (mapconcat 'pivotal-format-task tasks "")))

(defun pivotal-format-task (task)
  "Format TASK."
  (format "[%s] Task %s (ID:#%s) -- %s\n"
          (if (string= (pivotal-element-value task 'complete) "true")
              "X"
            " ")
          (pivotal-element-value task 'position)
          (pivotal-element-value task 'id)
          (pivotal-element-value task 'description)))

(provide 'pivotal-tracker)
;;; pivotal-tracker.el ends here
