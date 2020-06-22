;;;; dired
(after! dired
(add-hook 'dired-mode-hook
          (lambda ()
            (define-key dired-mode-map
              (kbd "C-c C-x a")
              #'org-attach-dired-to-subtree)))

(add-hook 'dired-mode-hook
          (lambda ()
            (define-key dired-mode-map (kbd "C-c C-x c")
              (lambda ()
                (interactive)
                (let ((org-attach-method 'cp))
                  (call-interactively #'org-attach-dired-to-subtree))))))

(setq dired-recursive-copies (quote always))
(setq dired-recursive-deletes (quote top))


;; always execute dired-k when dired buffer is opened
(add-hook 'dired-initial-position-hook 'dired-k)
(add-hook 'dired-after-readin-hook #'dired-k-no-revert)


;;; dired-hide-dotfiles-mode
(define-key dired-mode-map "." #'dired-hide-dotfiles-mode)
(add-hook! 'dired-mode-hook 'as/dired-mode-hook)
(defun as/dired-mode-hook ()
  (dired-hide-dotfiles-mode +1))
)
