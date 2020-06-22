;;; org

  (setq org-replace-disputed-keys t)
  (setq org-return-follows-link t)
  (setq org-catch-invisible-edits 'show)


  (add-hook
   'org-mode-hook
   (lambda ()
     (visual-line-mode 1)))

  (setf org-blank-before-new-entry '((heading . nil) (plain-list-item . nil)))
  (setq org-ellipsis "⤵")
  (setq org-use-speed-commands t)

;;;; org-todo-keywords
(setq org-todo-keywords
	    (quote ((sequence "TODO(t)" "NEXT(n)" "IN-PROGRESS(i)" "|" "DONE(d)")
		          (sequence "WAITING(w)" "HOLD(h)" "DELEGATE(g)" "|" "CANCELLED(c)" "SCHEDULED(s)"))))

(setq org-treat-S-cursor-todo-selection-as-state-change nil)

(custom-declare-face '+org-todo-active '((t (:inherit (bold font-lock-constant-face org-todo)))) "")
(custom-declare-face '+org-todo-project '((t (:inherit (bold font-lock-doc-face org-todo)))) "")
(custom-declare-face '+org-todo-onhold '((t (:inherit (bold warning org-todo)))) "")

(setq org-todo-keyword-faces
      (quote (("TODO" :foreground "red" :weight bold)
	            ("NEXT" :foreground "#007cee" :weight bold)
              ("IN-PROGRESS" :foreground "yellow" :weight bold)
	            ("DONE" :foreground "forest green" :weight bold)
	            ("WAITING" :foreground "orange" :weight bold)
              ("HOLD" :foreground "magenta" :weight bold)
	            ("DELEGATE" :foreground "purple" :weight bold)
	            ("CANCELLED" :foreground "red" :weight bold)
	            ("SCHEDULED" :foreground "forest green" :weight bold)

              (quote (("[-]"  . +org-todo-active)
                      ("STRT" . +org-todo-active)
                      ("[?]"  . +org-todo-onhold)
                      ("WAIT" . +org-todo-onhold))))))
;;;; org-todo-state-tags-triggers
(setq org-todo-state-tags-triggers
	    (quote (("CANCELLED" ("CANCELLED" . t))
		          ("WAITING" ("WAITING" . t))
		          ("HOLD" ("WAITING") ("HOLD" . t))
		          (done ("WAITING") ("HOLD"))
		          ("TODO" ("WAITING") ("CANCELLED") ("HOLD"))
		          ("NEXT" ("WAITING") ("CANCELLED") ("HOLD"))
		          ("DONE" ("WAITING") ("CANCELLED") ("HOLD")))))

(setq org-use-fast-todo-selection t)

;;;; org-refile
(setq org-id-locations-file "/home/alexander/.doom.d/.orgids")

;(setq org-refile-use-outline-path 'file)
;(setq org-outline-path-complete-in-steps nil)

(setq org-refile-allow-creating-parent-nodes (quote confirm))

;;;; org-babel
(defadvice org-babel-execute-src-block (around load-language nil activate)
  "Load language if needed"
  (let ((language (org-element-property :language (org-element-at-point))))
    (unless (cdr (assoc (intern language) org-babel-load-languages))
	    (add-to-list 'org-babel-load-languages (cons (intern language) t))
	    (org-babel-do-load-languages 'org-babel-load-languages org-babel-load-languages))
    ad-do-it))

(setq org-confirm-babel-evaluate nil)

(setq org-src-fontify-natively t
	    org-src-tab-acts-natively t
	    org-src-preserve-indentation t
	    org-src-window-setup 'current-window)

(setq org-confirm-babel-evaluate nil)
(setq org-babel-results-keyword "results")

(defun as/display-inline-images ()
  (condition-case nil
	    (org-display-inline-images)
    (error nil)))

(add-hook 'org-babel-after-execute-hook 'as/display-inline-images 'append)

;;;; org-tag-alist
(setq org-tag-alist (quote
                     (
                      ("NOTE" . ?n)
                      ("resilient" . ?r)
                      ("learning" . ?l)
                      ("sancocho" . ?s)
                      ("personal" . ?p)
                      ("crypt" . ?c)
                      )))

(setq org-fast-tag-selection-include-todo t)
(setq org-fast-tag-selection-single-key (quote expert))
;;;; org-column
(setq org-columns-default-format
      "%1PRIORITY(P) %50ITEM(Task) %12ALLTAGS(Area) %50OUTCOME(Outcome) %6Effort(Effort){:} %6ENERGY(Energy) %5CLOCKSUM(Clock) %10DEADLINE(Deadline)")

;;;; global-properties
(setq org-global-properties (quote (("Effort_ALL" . "0:05 0:10 0:15 0:25 0:45 1:00 1:30 2:00 3:00 4:00 5:00 6:00 0:00")
				                            ("STYLE_ALL" . "habit"))))
;;;; org-capture templates
(require 'org-protocol)

(defun org-journal-find-location ()
  (interactive)
  (journal-file-today)
  (goto-char (point-min)))


(add-hook 'org-capture-mode-hook (lambda () (call-interactively 'org-store-link)))

(setq org-inbox-file "~/1-Agenda/Inbox.org")

(setq org-capture-templates
	    (quote
       (("t" "Task" entry (file org-inbox-file) (function as/quick-capture))
        ("p" "Project" entry (file org-inbox-file) (file "~/.doom.d/templates/new-project.org"))
        ("e" "Event" entry (file org-inbox-file) "* %^{Event} %^g \n\n%^{When?}t\n")
        ("l" "Link" entry (file org-inbox-file) "* %(org-cliplink-capture) \n %(org-insert-time-stamp nil t nil nil nil nil) \n\n %?" :immediate-finish t)

        ("n" "Note")
		    ("nn" "Note" entry (file org-inbox-file) "* %^{Note} \n %(org-insert-time-stamp nil t nil nil nil nil) \n\n %?")
        ("np" "Plain Note" entry (file org-inbox-file) "* %^{Note} \n %T \n %?")
        ("ns" "Selection" entry (file org-inbox-file) "* %^{Title}  \nSource: %u, [[%F][%f]]\n\n %(org-insert-time-stamp nil t nil nil nil nil)\n\n #+BEGIN_QUOTE\n%i\n#+END_QUOTE\n\n %?")

        ("c" "Cooking")
        ("cc" "Cookbook" entry (file org-inbox-file) "%(org-chef-get-recipe-from-url)" :empty-lines 1)
        ("cm" "Manual Cookbook" entry (file org-inbox-file) "* %^{Recipe title: }\n  :PROPERTIES:\n  :source-url:\n  :servings:\n  :prep-time:\n  :cook-time:\n  :ready-in:\n  :END:\n** Ingredients\n   %?\n** Directions\n\n")



        ("j" "Journal")
		    ("jj" "Journal" entry (file org-inbox-file) "* %^{Title} %^g \n %(org-insert-time-stamp nil t nil nil nil nil) \n\n %?")
		    ("jp" "Problem" entry (file org-inbox-file) "* Problem - %^{Domain} %^g \n %(org-insert-time-stamp nil t nil nil nil nil) \n\n *Problem:* %?\n\n *Insight:*\n\n *Tomorrow:*\n\n")
		    ("jc" "Code" entry    (file org-inbox-file) "* Code - %^{Title} %^g \n %(org-insert-time-stamp nil t nil nil nil nil) \n\n#+BEGIN_SRC\n%i\n#+END_SRC\n\n%?")
        ("jf" "Focus Block" entry (file org-inbox-file) "* Focus - %^{Focus:} %^g \n %(org-insert-time-stamp nil t nil nil nil nil) \n\n %?")
		    ("jr" "Recovery Block" entry (file org-inbox-file) "* Recovery - %^{Recovery:} %^g \n %(org-insert-time-stamp nil t nil nil nil nil) \n\n %?")
		    ("ja" "Admin Block" entry (file org-inbox-file) "* Admin - %^{Admin:} %^g \n %(org-insert-time-stamp nil t nil nil nil nil) \n\n %?")
        ("ji" "Interruption" entry (file org-inbox-file) "* Interrupt - %? :interrupt:  \n %(org-insert-time-stamp nil t nil nil nil nil) \n\n")
        ("jg" "Github Issue" entry (file org-inbox-file) "* %? \n- Bug Description\n\n- Steps to Reproduce\n\n- Actual Result\n\n- Expected Result\n\n- Build Version  \n\n")

        ("r" "Routines")
        ("rr" "Morning Routine" entry (file org-inbox-file) (file "~/.doom.d/templates/morning-routine.org"))
        ("re" "Evening Routine" entry (file org-inbox-file) (file "~/.doom.d/templates/evening-routine.org"))
        ("rb" "Bedtime Routine" entry (file org-inbox-file) (file "~/.doom.d/templates/night-routine.org"))
        ("r!" "Meta Review" entry (file+olp+datetree "/tmp/reviews.org") (file "~/.doom.d/templates/meta.org"))

        ("w" "Review")
        ("wd" "Daily Review" entry (file org-inbox-file) (file "~/.doom.d/templates/daily-review.org"))
        ("ww" "Weekly Review" entry (file org-inbox-file) (file "~/.doom.d/templates/weekly-review.org"))
        ("wm" "Monthly Review" entry (file org-inbox-file) (file "~/.doom.d/templates/monthly-review.org"))
        ("wy" "Yearly Review" entry (file org-inbox-file) (file "~/.doom.d/templates/annual-review.org"))


        )))


(defun as/quick-capture-status ()
  "Create and return a TODO heading template"
  (interactive)
  (let* ((title (read-from-minibuffer "Capture: ")))
    (with-temp-buffer
      (org-mode)
      (org-insert-heading)
      (insert (concat title " "))
      (org-todo)
      (buffer-string))))

(defun as/quick-capture ()
  (mapconcat #'identity
             `(
               ,(as/quick-capture-status)
               "%(org-insert-time-stamp nil t nil nil nil nil)\n\n%?")
             "\n"))

(setq org-agenda-time-grid '((daily today require-timed)
                             (600 1000 1100 1200 1300 1400 1500 1600 1700 1800 1900 2000 2100 2200)
                             "......" "----------------"))

(setq org-capture-use-agenda-date t)


;;;; org-agenda
(after! org-agenda
  (require 'ox-org)

  (setq org-agenda-prefix-format '((agenda . " %i %?-12t% s")
 (todo . " ")
 (tags . " ")
 (search . " ")))


    (setq org-agenda-files (apply 'append
                                (mapcar
                                 (lambda (directory)
                                   (directory-files-recursively
                                    directory org-agenda-file-regexp))
                                 '("~/1-Agenda" "~/0-Inbox")
			                           )))

  (setq org-agenda-hide-tags-regexp "noexport\\|ATTACH")

  ;; (setq org-agenda-text-search-extra-files (quote (
  ;;                                                  )))


  (setq org-archive-location "/home/alexander/2-Linked/2-Time/Archive/%s_archive::")

  (setq org-time-stamp-rounding-minutes (quote (1 1)))

  (setq org-default-priority ?D)
  (setq org-agenda-follow-indirect t)

  (setq org-agenda-window-setup 'current-window)
  (setq org-agenda-dim-blocked-tasks nil)
  (setq org-agenda-use-tag-inheritance t)
  (setq org-agenda-compact-blocks t)

  (setq org-agenda-restriction-lock-highlight-subtree nil)

  (add-hook 'org-agenda-mode-hook
	          '(lambda () (hl-line-mode 1))
	          'append)

  (setq org-agenda-tags-todo-honor-ignore-options t)
  (setq org-agenda-inhibit-startup t)

  (setq org-agenda-log-mode-items (quote (closed state clock)))
  (setq org-agenda-show-all-dates t)
  (setq org-agenda-start-on-weekday nil)

  (setq org-agenda-tags-column -100
	      org-agenda-start-with-log-mode nil)

  (setq org-agenda-entry-text-maxlines 20)
  (setq org-agenda-entry-text-leaders " ")
  (setq org-agenda-timegrid-use-ampm t)
  (setq org-agenda-include-diary nil)
  (setq org-agenda-insert-diary-extract-time t)
  (setq org-log-done t)
  (setq org-agenda-span 'day)
  (setq org-agenda-start-day nil)

  (defun my-org-agenda-recent-open-loops ()
    (interactive)
    (let ((org-agenda-start-with-log-mode t)
          (org-agenda-use-time-grid nil))
      (org-agenda-list nil (org-read-date nil nil "-2d") 4)))

  (defun my-org-agenda-longer-open-loops ()
    (interactive)
    (let ((org-agenda-start-with-log-mode t)
          (org-agenda-use-time-grid nil))
      (org-agenda-list 'file (org-read-date nil nil "-14d") 28)))


  (setq organization-task-id "0f1cf581-cd20-4acd-bf70-e2174635579c")


  (defun as/punch-in (arg)
    (interactive "p")
    (setq as/keep-clock-running t)
    (if (equal major-mode 'org-agenda-mode)
        (let* ((marker (org-get-at-bol 'org-hd-marker))
               (tags (org-with-point-at marker (org-get-tags-at))))
          (if (and (eq arg 4) tags)
              (org-agenda-clock-in '(16))
            (as/clock-in-organization-task-as-default)))
      (save-restriction
        (widen)
        (if (and (equal major-mode 'org-mode) (not (org-before-first-heading-p)) (eq arg 4))
            (org-clock-in '(16))
          (as/clock-in-organization-task-as-default)))))

  (defun as/clock-in-organization-task-as-default ()
    (interactive)
    (org-with-point-at (org-id-find organization-task-id 'marker)
      (org-clock-in '(16))))

  (defun as/punch-out ()
    (interactive)
    (setq as/keep-clock-running nil)
    (when (org-clock-is-active)
      (org-clock-out))
    (org-agenda-remove-restriction-lock))


;;;; org-super-agenda
  (org-super-agenda-mode t)
  (setq org-agenda-skip-scheduled-if-done t
        org-agenda-skip-deadline-if-done t
        org-agenda-include-deadlines t
        org-agenda-include-diary t
        org-agenda-block-separator t
        org-agenda-compact-blocks t
        org-agenda-start-with-log-mode nil)


  (setq org-agenda-custom-commands
	      '(
          ("o" "Overview"
           ((agenda "" ((org-agenda-span 1)
			                  (org-super-agenda-groups
			                   '((:name "Habits"
				                    :habit t
                            :order 2)

			                     (:name "Schedule"
				                    :time-grid t
				                    :scheduled t
				                    :order 1)
			                     (:discard (:anything t))))))


            (alltodo "" ((org-agenda-overriding-header "Stuck Project")
                         (org-super-agenda-groups
                          '( (:name none
                              :discard (:children "NEXT")
                              :discard (:children "IN-PROGRESS")
                              :order 4)

                             (:name none
                              :discard (:children nil)
                              :order 4)
                             (:name none
                              :children todo)))))


            (alltodo "" ((org-agenda-overriding-header "Active Project")
                         (org-super-agenda-groups
                          '((:name none
                             :children "NEXT"
                             :children "IN-PROGRESS"
                             :order 1)
                            (:discard (:anything t))))))

            (alltodo "" ((org-agenda-overriding-header "In-Progress")
                         (org-super-agenda-groups
                          '((:name none
                             :discard (:not (:todo ("IN-PROGRESS")))
                             :discard (:habit)
                             :order 6)
                            (:name none
                             :todo t
                             :face (:underline t))
                            ))))

            (alltodo "" ((org-agenda-overriding-header "Next Task")
                         (org-super-agenda-groups
                          '((:name none
                             :discard (:not (:todo "NEXT"))
                             :discard (:tag "HOLD")
                             :discard (:habit)
                             :order 5)
                            (:name none
                             :todo "NEXT"
                             :face (:background "" :underline t))))))

            (alltodo "" ((org-agenda-overriding-header "Project Task")
                         (org-agenda-skip-function 'bh/skip-non-project-tasks)
                         (org-super-agenda-groups
                          '((:name none
                             :discard (:tag "HOLD")
                             :todo t
                             :order 5)))))

            (alltodo "" ((org-agenda-overriding-header "Standalone Task")
                         (org-agenda-skip-function 'bh/skip-project-tasks)
                         (org-super-agenda-groups
                          '((:name none
                             :discard (:tag "REFILE")
                             :todo ("TODO")
                             :order 7)
                            (:discard (:anything t))))))

            (alltodo "" ((org-agenda-overriding-header "Waiting for, Delegate, Hold")
                         (org-agenda-skip-function 'bh/skip-project-tasks)
                         (org-super-agenda-groups
                          '((:name none
                             :discard (:tag "REFILE")
                             :tag ("HOLD")
                             :todo ("WAITING" "DELEGATE")
                             :order 7)
                            (:discard (:anything t))))))


            ))
          ))

        (defun bh/is-project-p ()
          "Any task with a todo keyword subtask"
          (save-restriction
            (widen)
            (let ((has-subtask)
                  (subtree-end (save-excursion (org-end-of-subtree t)))
                  (is-a-task (member (nth 2 (org-heading-components)) org-todo-keywords-1)))
              (save-excursion
                (forward-line 1)
                (while (and (not has-subtask)
                            (< (point) subtree-end)
                            (re-search-forward "^\*+ " subtree-end t))
                  (when (member (org-get-todo-state) org-todo-keywords-1)
                    (setq has-subtask t))))
              (and is-a-task has-subtask))))

        (defun bh/find-project-task ()
          "Move point to the parent (project) task if any"
          (save-restriction
            (widen)
            (let ((parent-task (save-excursion (org-back-to-heading 'invisible-ok) (point))))
              (while (org-up-heading-safe)
                (when (member (nth 2 (org-heading-components)) org-todo-keywords-1)
                  (setq parent-task (point))))
              (goto-char parent-task)
              parent-task)))

        (defun bh/skip-non-tasks ()
          "Show non-project tasks.
Skip project and sub-project tasks, habits, and project related tasks."
          (save-restriction
            (widen)
            (let ((next-headline (save-excursion (or (outline-next-heading) (point-max)))))
              (cond
               ((bh/is-task-p)
                nil)
               (t
                next-headline)))))

        (defun bh/skip-project-tasks ()
          "Show non-project tasks.
Skip project and sub-project tasks, habits, and project related tasks."
          (save-restriction
            (widen)
            (let* ((subtree-end (save-excursion (org-end-of-subtree t))))
              (cond
               ((bh/is-project-p)
                subtree-end)
               ((org-is-habit-p)
                subtree-end)
               ((bh/is-project-subtree-p)
                subtree-end)
               (t
                nil)))))

        (defun bh/is-task-p ()
          "Any task with a todo keyword and no subtask"
          (save-restriction
            (widen)
            (let ((has-subtask)
                  (subtree-end (save-excursion (org-end-of-subtree t)))
                  (is-a-task (member (nth 2 (org-heading-components)) org-todo-keywords-1)))
              (save-excursion
                (forward-line 1)
                (while (and (not has-subtask)
                            (< (point) subtree-end)
                            (re-search-forward "^\*+ " subtree-end t))
                  (when (member (org-get-todo-state) org-todo-keywords-1)
                    (setq has-subtask t))))
              (and is-a-task (not has-subtask)))))

        (defun bh/is-project-subtree-p ()
          "Any task with a todo keyword that is in a project subtree.
Callers of this function already widen the buffer view."
          (let ((task (save-excursion (org-back-to-heading 'invisible-ok)
                                      (point))))
            (save-excursion
              (bh/find-project-task)
              (if (equal (point) task)
                  nil
                t))))


        (defun bh/skip-non-project-tasks ()
          "Show project tasks.
Skip project and sub-project tasks, habits, and loose non-project tasks."
          (save-restriction
            (widen)
            (let* ((subtree-end (save-excursion (org-end-of-subtree t)))
                   (next-headline (save-excursion (or (outline-next-heading) (point-max)))))
              (cond
               ((bh/is-project-p)
                next-headline)
               ((org-is-habit-p)
                subtree-end)
               ((and (bh/is-project-subtree-p)
                     (member (org-get-todo-state) (list "NEXT" "IN-PROGRESS")))
                subtree-end)
               ((not (bh/is-project-subtree-p))
                subtree-end)
               (t
                nil)))))
        )
;;;; org-attach
  (setq org-attach-id-dir  "~/2-Linked/2-Codex")
  (setq org-attach-method 'mv)

  (defun as/goto-inbox ()
    (interactive)
    (find-file "~/1-Agenda/Inbox.org")
    (dired-other-window "~/0-Inbox")
    (dired-hide-details-mode)
    )

  (defun as/add-to-inbox ()
    (interactive)
    (dired-mark nil)
    (dired-copy-filename-as-kill '(4))
    (other-window 1 nil)
    (goto-char (point-max))
    (newline-and-indent)
    (end-of-line)
    (call-interactively 'org-insert-heading-respect-content nil)
    (yank)
    (org-id-get-create)
    (other-window 1 nil)
    (call-interactively 'org-attach-dired-to-subtree)
    )
;;;; org-gcal
;(load-file "~/.doom.d/gcal.el")
