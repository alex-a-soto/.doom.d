;;; Hyperbole
(use-package! hyperbole
  :config
  (define-key org-mode-map (kbd "<M-return>") nil)
  (add-hook 'hyperbole-init-hook 'hmouse-add-unshifted-smart-keys)
  (add-to-list 'hyperbole-web-search-alist '("DuckDuckGo" . "https://duckduckgo.com/?q=%s"))

  (setq hbmap:dir-user (concat "~/.doom.d/hyperb/"))
  (setq hbmap:dir-filename (concat "~/.doom.d/hyperb/HBMAP"))
  (setq hattr:filename "hypb")
  (setq gbut:file (concat "~/.doom.d/hyperb/HYPB"))

  (setq browse-url-firefox-program "firefox-esr")
  (setq browse-url-browser-function #'browse-url-firefox)
  (setq hyperbole-web-search-browser-function #'browse-url-firefox)

(defun as/find-overview ()
  (interactive)
  (find-file "~/.doom.d/hyperb/HYPB"))

)
