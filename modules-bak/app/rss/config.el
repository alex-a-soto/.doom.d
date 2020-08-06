;;; elfeed
(setf url-queue-timeout 30)

(elfeed-org)

(setq rmh-elfeed-org-files (list "~/2-Linked/2-Codex/08/6e648e-2baf-4775-a0bc-8153ae99e505/rss.org"))

(setq-default elfeed-search-filter "@4-hours-ago +unread")

(defun as/elfeed-load-db-and-open ()
  "Wrapper to load the elfeed db from disk before opening"
  (interactive)
  (elfeed-db-load)
  (elfeed)
  (elfeed-update)
  (setq elfeed-search-filter "@1-hours-ago +unread")
  (elfeed-search-update--force))

;;write to disk when quiting
(defun as/elfeed-save-db-and-bury ()
  "Wrapper to save the elfeed db to disk before burying buffer"
  (interactive)
  (elfeed-db-save)
  (quit-window))

(define-key elfeed-search-mode-map "q" 'as/elfeed-save-db-and-bury)
(define-key elfeed-search-mode-map "h"
  (lambda ()
    (interactive)
    (elfeed-search-set-filter (default-value 'elfeed-search-filter))))

(define-key elfeed-search-mode-map (kbd "j") #'next-line)
(define-key elfeed-search-mode-map (kbd "k") #'previous-line)

(defun as/elfeed-yank-capture ()
  (interactive)
  (elfeed-search-yank)
  (org-capture nil "l")
  (message "Entry saved to Inbox"))

(define-key elfeed-search-mode-map "y" #'as/elfeed-yank-capture)

(use-package! elfeed-org
  :when (featurep! +org)
  :after elfeed
  :config
  (and (let ((default-directory org-directory))
         (setq rmh-elfeed-org-files
               (cl-remove-if-not
                #'file-exists-p (mapcar #'expand-file-name rmh-elfeed-org-files))))
       (elfeed-org)))
