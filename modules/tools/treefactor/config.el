;;;; treefactor

(setq treefactor-org-agenda-dir "~/1-Agenda")

(defhydra as/treefactor-mode-hydra ()
  ("1" as/treefactor-agenda "Agenda")
  ("2" as/treefactor-linked "Linked")
  ("o" other-window "Other Window")
  ("r" org-rename-heading "Rename heading")
  ("t" org-todo "TODO Status")
  ("SPC" treefactor-throw "Throw")
  ("RET" dired-find-file "Find File")
  ("<up>" dired-previous-line "Previous Line")
  ("<down>" dired-next-line "Next Line")
  ("<backspace>" dired-up-directory "Up dir")
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
    (delete-other-windows)
    (if (file-equal-p (substring (directory-file-name buffer-file-truename) 0 11 ) "~/1-Agenda/")
        (dired-other-window "~/1-Agenda/")
    
      (if (file-equal-p (substring (directory-file-name buffer-file-truename) 0 11 ) "~/2-Linked/")
        (dired-other-window "~/2-Linked/")))

    (other-window -1)
    (mark-whole-buffer)
    (goto-char (point-min))
;    (org-sort-entries nil ?o)
    (org-next-visible-heading 1)
    (insert-mode-modalka)
    (as/treefactor-mode-hydra/body))

  (defun as/treefactor-agenda ()
    (interactive)
    (delete-other-windows)
    (goto-char (point-min))
    (org-next-visible-heading 1)
    (dired-other-window "~/1-Agenda")
    (other-window -1))

  (defun as/treefactor-linked ()
    (interactive)
    (delete-other-windows)
    (goto-char (point-min))
    (org-next-visible-heading 1)
    (dired-other-window "~/2-Linked")
    (other-window -1))
