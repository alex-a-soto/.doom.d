(defvar bh/organization-task-id "13b0b560-b5c7-4314-8438-54c684bfd08c")

(setq org-inbox-file "~/1-Agenda/Inbox.org")
(setq anki-flashcards "~/2-Linked/3-Persinter/1-persinter/flashcards.org")
(setq rc-journal "~/1-Agenda/4-Time/2-Not-me/0-Resilient Coders/journal.org")


(if (boundp 'org-user-agenda-files)
    (setq org-agenda-files org-user-agenda-files)
      (setq org-agenda-files (apply 'append
                                (mapcar
                                 (lambda (directory)
                                   (directory-files-recursively
                                    directory org-agenda-file-regexp))
                                 '("~/1-Agenda")
			                           ))))

;; Capture templates for: TODO tasks, Notes, appointments, phone calls, meetings, and org-protocol
(setq org-capture-templates
	    (quote
       (("t" "Task" entry (file org-inbox-file) "* TODO %^{Task} \n %U \n\n %?" :clock-in t :clock-resume t)
        ("p" "Project" entry (file org-inbox-file) (file "~/.doom.d/templates/new-project.org") :clock-in t :clock-resume t)
        ("m" "Meeting" entry (file org-inbox-file) "* %^{Meeting} :MEETING: \n\n%^{When?}t\n" :clock-in t :clock-resume t)
        ("c" "Phone call" entry (file org-inbox-file) "* PHONE %? :PHONE:\n%U" :clock-in t :clock-resume t)
        ("n" "Note" entry (file org-inbox-file) "* %^{Note} :NOTE: \n %U \n\n %?" :clock-in t :clock-resume t)
        ("l" "Link" entry (file org-inbox-file) "* %(org-cliplink-capture) \n %U \n\n %?" :immediate-finish t)
        ("h" "Habit" entry (file org-inbox-file) "* TODO %?\n SCHEDULED: %(format-time-string \"%<<%Y-%m-%d %a .+1d/3d>>\")\n:PROPERTIES:\n:STYLE: habit\n:REPEAT_TO_STATE: NEXT\n:END:\n")
        ("f" "Flashcard" entry (file anki-flashcards) "* %^{Title} \n:PROPERTIES: \n :ANKI_DECK: %^{Deck} \n :ANKI_NOTE_TYPE: Cloze \n :END: \n\n** Text \n#+BEGIN_QUOTE\n%i\n#+END_QUOTE\n\n %?" :clock-in t :clock-resume t)

        ("j" "Journal")
        ("jj" "Resilient" entry (file+function rc-journal org-reverse-datetree-goto-date-in-file) "* %^{Title} :resilient: \n %(org-insert-time-stamp nil t nil nil nil nil) \n\n %?" :clock-in t :clock-resume t)
        ("jp" "Personal" entry (file org-inbox-file) "* %^{Title} %^g \n %(org-insert-time-stamp nil t nil nil nil nil) \n\n %?" :clock-in t :clock-resume t)

        ("r" "Routines")
        ("rm" "Morning" entry (file org-inbox-file) (file "~/.doom.d/templates/morning-routine.org") :clock-in t :clock-resume t)
        ("re" "Evening" entry (file org-inbox-file) (file "~/.doom.d/templates/evening-routine.org") :clock-in t :clock-resume t)
        ("rb" "Bedtime" entry (file org-inbox-file) (file "~/.doom.d/templates/night-routine.org") :clock-in t :clock-resume t)
        ("rp" "Processing" entry (file+olp+datetree "/tmp/reviews.org") (file "~/.doom.d/templates/meta.org") :clock-in t :clock-resume t)

        ("w" "Revie(w)")
        ("wd" "Daily" entry (file org-inbox-file) (file "~/.doom.d/templates/daily-review.org") :clock-in t :clock-resume t)
        ("ww" "Weekly" entry (file org-inbox-file) (file "~/.doom.d/templates/weekly-review.org") :clock-in t :clock-resume t)
        ("wm" "Monthly" entry (file org-inbox-file) (file "~/.doom.d/templates/monthly-review.org") :clock-in t :clock-resume t)
        ("wy" "Yearly" entry (file org-inbox-file) (file "~/.doom.d/templates/annual-review.org") :clock-in t :clock-resume t))))



; Tags with fast selection keys
(setq org-tag-alist (quote ((:startgroup)
                      ("resilient" . ?r)
                      ("learning" . ?l)
                      ("sancocho" . ?s)
                      ("personal" . ?p)
                      (:endgroup)
                      ("NOTE" . ?n)
                      ("crypt" . ?E)
                      ("CANCELLED" . ?c)
                      ("FLAGGED" . ??)
                      ("WAITING" . ?w)
                      ("HOLD" . ?h)
                      ("LOG" . ?L)
                      )))

; Set default column view headings: Task Effort Clock_Summary
 (setq org-columns-default-format
      "%1PRIORITY(P) %50ITEM(Task) %12ALLTAGS(Area) %50OUTCOME(Outcome) %6Effort(Effort){:} %6ENERGY(Energy) %5CLOCKSUM(Clock) %10DEADLINE(Deadline)")

  (setq org-attach-id-dir  "~/2-Linked/2-Codex")
  (setq org-attach-method 'mv)

(defadvice org-babel-execute-src-block (around load-language nil activate)
  "Load language if needed"
  (let ((language (org-element-property :language (org-element-at-point))))
    (unless (cdr (assoc (intern language) org-babel-load-languages))
	    (add-to-list 'org-babel-load-languages (cons (intern language) t))
	    (org-babel-do-load-languages 'org-babel-load-languages org-babel-load-languages))
    ad-do-it))


(setq org-agenda-prefix-format '((agenda . " %i %?-12t% s")
                                 (todo . " ")
                                 (tags . " ")
                                 (search . " ")))

(setq org-archive-location "/home/alexander/2-Linked/4-Time/1-Me/%s_archive::")

(setq org-lowest-priority 68)
(setq org-agenda-follow-indirect t)

(setq org-agenda-use-tag-inheritance t)

(setq org-ellipsis "⤵")

(setq org-agenda-timegrid-use-ampm t)


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

(setq org-agenda-entry-text-leaders " ")
(setq org-agenda-entry-text-maxlines 20)

(setq org-agenda-follow-indirect t)

(setq org-time-stamp-rounding-minutes (quote (1 1)))

(setq org-capture-use-agenda-date t)

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
               "%U")
             "\n"))

(setq org-src-fontify-natively t)

(setq org-refile-allow-creating-parent-nodes (quote confirm))

(add-hook 'org-capture-mode-hook (lambda () (call-interactively 'org-id-get-create)))

(setq org-habit-show-all-today t)
(setq org-habit-show-habits-only-for-today t)
(setq org-agenda-show-future-repeats 'next)

(setq org-agenda-start-day (org-today))

;(setq org-agenda-hide-tags-regexp "noexport\\|ATTACH\\|")
;(add-hook 'org-capture-mode-hook (lambda () (call-interactively 'org-store-link)))
;(defun org-journal-find-location ()
;  (interactive)
;  (journal-file-today)
;  (goto-char (point-min)))


(load-file "~/.doom.d/modules/lang/my-org/org-mode.el")
