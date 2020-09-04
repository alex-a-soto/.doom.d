;;; editor/scratch/config.el -*- lexical-binding: t; -*-

(defun prot/scratch-buffer-setup ()
    "Add contents to `scratch' buffer and name it accordingly."
    (let* ((mode (format "%s" major-mode))
           (string (concat "Scratch buffer for: " mode "\n\n")))
      (when scratch-buffer
        (save-excursion
          (insert string)
          (goto-char (point-min))
          (comment-region (point-at-bol) (point-at-eol)))
        (forward-line 2))
      (rename-buffer (concat "*Scratch for " mode "*") t)))


(add-hook 'scratch-create-buffer-hook #'prot/scratch-buffer-setup)
