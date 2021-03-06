;;; Hyperbole
(require 'hyperbole)
(use-package! hyperbole
  :config
  (define-key org-mode-map (kbd "<M-return>") nil)
 ; (add-hook 'hyperbole-init-hook 'hmouse-add-unshifted-smart-keys)
  (add-to-list 'hyperbole-web-search-alist '("DuckDuckGo" . "https://duckduckgo.com/?q=%s"))

  (setq hbmap:dir-user (concat "~/.doom.d/hyperb/"))
  (setq hbmap:dir-filename (concat "~/.doom.d/hyperb/HBMAP"))
  (setq hattr:filename "hypb")
  (setq gbut:file (concat "~/.doom.d/hyperb/HYPB"))


  (setq browse-url-firefox-program "firefox")
  (setq browse-url-browser-function #'browse-url-firefox)
  (setq hyperbole-web-search-browser-function #'browse-url-firefox)


(defun as/find-overview ()
  (interactive)
  (find-file "~/.doom.d/hyperb/pim/agenda.org"))


;; To add Org files directly to HyRolo so they're searchable, customize this
;; variable:.
;(setq hyrolo-file-list '("~/2-Linked/rolo.org"))

;; You'll probably need to set the face for HyRolo's word highlighting. I chose
;; purple with an underline:
(setq hyrolo-highlight-face '(:background "SystemWindowText" :foreground "purple1" :underline t))

;; The below prevents HyRolo from seeing Org's *bold* markup as a heading when
;; it happens at the start of a line
;(setq hyrolo-entry-regexp "^\\*+ ")

;; To stop HyRolo inserting the date at the end of your Org entries when you
;; visit them from HyRolo to make edits, add this after you load Hyperbole
;; in your init:
(remove-hook 'hyrolo-edit-hook #'hyrolo-set-date)

;; To stop HyRolo adding the date at the end of your Org entries when you
;; Add a record, add this after you load Hypervole in your init:
(remove-hook 'hyrolo-add-hook #'hyrolo-set-date)

;; Both of the above remove dates across the board when adding and editing
;; though, which may not be what you want. I generally don't want edit
;; dates, so I'm fine turning them off. YMMV.
;;

)

(hyperbole-toggle-messaging 1)
(hpath:find-file-urls-mode 1)
(hkey-ace-window-setup)
