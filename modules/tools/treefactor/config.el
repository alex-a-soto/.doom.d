;;;; treefactor

(setq treefactor-org-agenda-dir "~/1-Agenda")

(defhydra as/treefactor-mode-hydra ()
  ("1" as/treefactor-agenda "Agenda")
  ("2" as/treefactor-linked "Linked")
  ("o" other-window "Other Window")
  ("r" org-rename-heading "Rename heading")
  ("t" treefactor-throw "Throw")
  ("u" treefactor-up "Throw Up")
  ("s" treefactor-org-store-link-fold-drawer "Store")
  ("i" org-insert-link "Insert")
  ("k" org-cut-subtree "Cut Subtree")
  ("q" nil "cancel" :exit t))

(defun as/copy-to-journal ()
  (interactive)
  (let ((filename (buffer-file-name))
        (buffer (current-buffer))
        (name (buffer-name)))
    (if (not (and filename (file-exists-p filename)))
        (error "'%s' is not a file buffer" name)
      (copy-file  filename (format-time-string (concat user-home-directory "2-Linked/4-Time/1-Me/Journal/Review/Source/%Y/%m/")) t )
      (message "Copied '%s' to journal." name))))

  (defun org-rename-heading (label)
    "Rename the current section's header to LABEL, and moves the
          point to the end of the line."
    (interactive (list
                  (read-string "Heading: "
                               (substring-no-properties (org-get-heading t t t t)))))
    (org-back-to-heading)
    (replace-string (org-get-heading t t t t) label))


  (defun as/treefactor-hydra ()
    (interactive)
    (find-file "~/1-Agenda/Inbox.org")
    (delete-other-windows)
    (goto-char (point-min))
    (dired-other-window "~/1-Agenda")
    (other-window -1)
    (org-next-visible-heading 1)
    (mark-whole-buffer)
    (org-sort-entries nil ?o)
    (insert-mode-modalka)
    (as/treefactor-mode-hydra/body))

  (defun as/treefactor-agenda ()
    (interactive)
    (delete-other-windows)
    (goto-char (point-min))
    (dired-other-window "~/1-Agenda")
    (other-window -1))

  (defun as/treefactor-linked ()
    (interactive)
    (delete-other-windows)
    (goto-char (point-min))
    (dired-other-window "~/2-Linked")
    (other-window -1))
