(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(ansi-color-faces-vector
   [default default default italic underline success warning error])
 '(ansi-color-names-vector
   ["#282c34" "#ff6c6b" "#98be65" "#ECBE7B" "#51afef" "#c678dd" "#46D9FF" "#bbc2cf"])
 '(bmkp-last-as-first-bookmark-file "~/.emacs.d/.local/etc/bookmarks")
 '(doom-modeline-mode t)
 '(global-display-line-numbers-mode t)
 '(jdee-db-active-breakpoint-face-colors (cons "#1B2229" "#51afef"))
 '(jdee-db-requested-breakpoint-face-colors (cons "#1B2229" "#98be65"))
 '(jdee-db-spec-breakpoint-face-colors (cons "#1B2229" "#3f444a"))
 '(modalka-excluded-modes
   '(ediff-mode helpful-mode dired-mode magit-mode magit-popup-mode debugger-mode ediff-mode help-mode git-rebase-mode help-mode org-agenda-mode org-capture-mode emms-playlist-mode pdf-tools-modes undo-tree-visualizer-mode))
 '(mouse-1-click-follows-link nil)
 '(my-font-size 15)
 '(objed-cursor-color "#ff6c6b")
 '(pdf-view-midnight-colors (cons "#bbc2cf" "#282c34"))
 '(rustic-ansi-faces
   ["#282c34" "#ff6c6b" "#98be65" "#ECBE7B" "#51afef" "#c678dd" "#46D9FF" "#bbc2cf"])
 '(safe-local-variable-values
   '((eval progn
           (set
            (make-local-variable 'org-time-clocksum-format)
            '(:hours "%d" :require-hours t :minutes ":%02d" :require-minutes t))
           (setq org-latex-tables-centered nil org-latex-default-table-environment "longtable")
           (local-set-key
            (kbd "<f5>")
            (lambda nil
              (interactive)
              (beginning-of-buffer)
              (re-search-forward "Invoice number: \\([0-9]+\\)")
              (let
                  ((n
                    (string-to-number
                     (match-string 1))))
                (kill-region
                 (match-beginning 1)
                 (match-end 1))
                (insert
                 (format "%d"
                         (1+ n))))
              (beginning-of-buffer)
              (re-search-forward "Invoice date: *")
              (kill-region
               (point)
               (save-excursion
                 (end-of-line)
                 (point)))
              (org-insert-time-stamp
               (current-time)
               nil t)
              (beginning-of-buffer)
              (search-forward "#+BEGIN: clocktable")
              (unwind-protect
                  (progn
                    (defadvice org-table-goto-column
                        (before always-make-new-columns
                                (n &optional on-delim force)
                                activate)
                      "always adds new columns when we move to them"
                      (setq force t))
                    (org-clocktable-shift 'right 1))
                (ad-deactivate 'org-table-goto-column))
              (beginning-of-buffer)
              (search-forward "| totaltarget")
              (org-table-recalculate t))))
     (org-file-tags
      '("business-money"))
     (org-file-tags "business-money")))
 '(vc-annotate-background "#282c34")
 '(vc-annotate-color-map
   (list
    (cons 20 "#98be65")
    (cons 40 "#b4be6c")
    (cons 60 "#d0be73")
    (cons 80 "#ECBE7B")
    (cons 100 "#e6ab6a")
    (cons 120 "#e09859")
    (cons 140 "#da8548")
    (cons 160 "#d38079")
    (cons 180 "#cc7cab")
    (cons 200 "#c678dd")
    (cons 220 "#d974b7")
    (cons 240 "#ec7091")
    (cons 260 "#ff6c6b")
    (cons 280 "#cf6162")
    (cons 300 "#9f585a")
    (cons 320 "#6f4e52")
    (cons 340 "#5B6268")
    (cons 360 "#5B6268")))
 '(vc-annotate-very-old-color nil)
 '(which-key-prefix-prefix "+")
 '(which-key-separator " "))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(default ((t (:family "DejaVu Sans Mono" :foundry "PfEd" :slant normal :weight normal :height 90 :width normal))))
 '(aw-leading-char-face ((t (:inherit ace-jump-face-foreground :height 3.0))))
 '(org-mode-line-clock ((t (:foreground "red" :box (:line-width -1 :style released-button))))))
