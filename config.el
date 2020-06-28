(cond
 ((string-equal system-name "exomind")
  (load-file (concat "~/.doom.d/config/" system-name ".el"))))

(cond
 ((string-equal system-name "SurfaceGo")
  (load-file (concat "~/.doom.d/config/" system-name ".el"))))

(load-file "~/.doom.d/config/defaults.el")
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(bmkp-last-as-first-bookmark-file "~/.emacs.d/.local/etc/bookmarks")
 '(modalka-excluded-modes
   (quote
    (ediff-mode helpful-mode dired-mode magit-mode magit-popup-mode debugger-mode ediff-mode help-mode git-rebase-mode help-mode org-agenda-mode org-capture-mode emms-playlist-mode pdf-tools-modes undo-tree-visualizer-mode)))
 '(mouse-1-click-follows-link nil)
 '(my-font-size 14)
 '(which-key-prefix-prefix "+")
 '(which-key-separator " "))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(default ((t (:family "DejaVu Sans Mono" :foundry "PfEd" :slant normal :weight normal :height 90 :width normal))))
 '(aw-leading-char-face ((t (:inherit ace-jump-face-foreground :height 3.0)))))
