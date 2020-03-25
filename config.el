;;; $DOOMDIR/config.el -*- lexical-binding: t; -*-

;; Place your private configuration here! Remember, you do not need to run 'doom
;; sync' after modifying this file!
;;
;; Here are some additional functions/macros that could help you configure Doom:
;;
;; - `load!' for loading external *.el files relative to this one
;; - `use-package' for configuring packages
;; - `after!' for running code after a package has loaded
;; - `add-load-path!' for adding directories to the `load-path', relative to
;;   this file. Emacs searches the `load-path' when you load packages with
;;   `require' or `use-package'.
;; - `map!' for binding new keys
;;
;; To get information about any of these functions/macros, move the cursor over
;; the highlighted symbol at press 'K' (non-evil users must press 'C-c g k').
;; This will open documentation for it, including demos of how they are used.
;;
;; You can also try 'gd' (or 'C-c g d') to jump to their definition and see how
;; they are implemented.


;;; Personal Information
(setq user-full-name "Alexander Soto"
      user-mail-address "alexander.soto@gmail.com")
;;; Fonts
;; Doom exposes five (optional) variables for controlling fonts in Doom. Here
;; are the three important ones:
;;
;; + `doom-font'
;; + `doom-variable-pitch-font'
;; + `doom-big-font' -- used for `doom-big-font-mode'; use this for
;;   presentations or streaming.
;;
;; They all accept either a font-spec, font string ("Input Mono-12"), or xlfd
;; font string. You generally only need these two:
(setq doom-font (font-spec :family "monospace" :size 14))

;;; Default Theme
;; There are two ways to load a theme. Both assume the theme is installed and
;; available. You can either set `doom-theme' or manually load a theme with the
;; `load-theme' function. This is the default:

(setq doom-theme 'doom-one)

;;; Configuration
;;;; Constants
;; If you use `org' and don't want your org files in the default location below,
;; change `org-directory'. It must be set before org loads!

(setq org-directory "~/Sync/org/")

;; This determines the style of line numbers in effect. If set to `nil', line
;; numbers are disabled. For relative line numbers, set this to `relative'.

;;;; Display Luine Numbers
(setq display-line-numbers-type t)
;;;; Display time and battery
  (display-time-mode 1)
  (display-battery-mode 1)

;;;; eww
(add-hook 'eww-after-render-hook 'eww-readable)
(add-hook 'eww-after-render-hook 'writeroom-mode)
(add-hook 'eww-after-render-hook 'visual-line-mode)

;;; Packages
;;;; Which Key
(use-package! which-key
  :diminish
  :config
  (setq which-key-sort-order #'which-key-prefix-then-key-order
	      which-key-sort-uppercase-first nil
	      which-key-add-column-padding 1
	      which-key-max-display-columns nil
	      which-key-idle-delay 0.0
	      which-key-special-keys nil
	      which-key-min-display-lines 7)
  (set-face-attribute 'which-key-local-map-description-face nil :weight 'bold)
  (which-key-setup-side-window-bottom)
  :custom
  (which-key-separator " ")
  (which-key-prefix-prefix "+"))

;;;; Outshine
(use-package! outshine
  :commands
  (outshine-mode))

;;;; Company
(after! company
  (setq company-echo-delay 0)
  (setq company-idle-delay 0.1)
  (setq company-minimum-prefix-length 2)
  (setq company-tooltip-limit 20))

   (custom-set-faces
    '(aw-leading-char-face
      ((t (:inherit ace-jump-face-foreground :height 3.0)))))


;;;; Scratch
(use-package! scratch)

;;;; org-now
(setq org-now-location (quote ("~/Sync/org/now/now.org")))


(setf org-agenda-bulk-custom-functions
      '((?n org-now-agenda-refile-to-now)
        (?P org-now-agenda-refile-to-previous-location)))
