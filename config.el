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


;;; Configuration
;;;; Personal Information
(setq user-full-name "Alexander Soto"
      user-mail-address "alexander.soto@gmail.com")
;;;; Fonts
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

;;;; Default Theme
;; There are two ways to load a theme. Both assume the theme is installed and
;; available. You can either set `doom-theme' or manually load a theme with the
;; `load-theme' function. This is the default:

(setq doom-theme 'doom-one)

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

;;;; Delete by moving to trash
 (setq delete-by-moving-to-trash  t)

;;;; eww
(add-hook 'eww-after-render-hook 'eww-readable)
(add-hook 'eww-after-render-hook 'writeroom-mode)
(add-hook 'eww-after-render-hook 'visual-line-mode)

(setf org-agenda-bulk-custom-functions
      '((?n org-now-agenda-refile-to-now)
        (?P org-now-agenda-refile-to-previous-location)))

   (defvar as/toggle-one-window--buffer-name nil
     "Variable to store the name of the buffer for which the `as/toggle-one-window'
   function is called.")
   (defvar as/toggle-one-window--window-configuration nil
     "Variable to store the window configuration before `as/toggle-one-window'
   function was called.")
   (defun as/toggle-one-window (&optional force-one-window)
     "Toggles the frame state between deleting all windows other than
   the current window and the windows state prior to that."
     (interactive "P")
     (if (or (null (one-window-p))
	     force-one-window)
	 (progn
	   (setq as/toggle-one-window--buffer-name (buffer-name))
	   (setq as/toggle-one-window--window-configuration (current-window-configuration))
	   (delete-other-windows))
       (progn
	 (when as/toggle-one-window--buffer-name
	   (set-window-configuration as/toggle-one-window--window-configuration)
	   (switch-to-buffer as/toggle-one-window--buffer-name)))))


(map! :leader
      :prefix "w"
      :map winner-mode-map
      :desc "Toggle Window" "SPC" #'as/toggle-one-window)

;;;; Fold
(map! :leader
      :desc "Fold" "TAB" #'+fold/toggle)


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


;;;; org-now
(after! org-now
(setq org-now-location (quote ("~/Sync/org/now/now.org"))))

;;;; org-roam
(use-package! org-roam
  :commands (org-roam-insert org-roam-find-file org-roam-switch-to-buffer org-roam)
  :hook
  (after-init . org-roam-mode)
  :custom-face
  (org-roam-link ((t (:inherit org-link :foreground "#005200"))))
  :init
  (map! :leader
        :prefix "n"
        :desc "org-roam" "l" #'org-roam
        :desc "org-roam-insert" "i" #'org-roam-insert
        :desc "org-roam-switch-to-buffer" "b" #'org-roam-switch-to-buffer
        :desc "org-roam-find-file" "f" #'org-roam-find-file
        :desc "org-roam-show-graph" "g" #'org-roam-show-graph
        :desc "org-roam-insert" "i" #'org-roam-insert)
  (setq org-roam-directory "/home/alexander/Sync/org/notes/"
        org-roam-db-location "/home/alexander/Sync/org/notes/org-roam.db")
  :config
  (require 'org-roam-protocol)
  (setq org-roam-capture-templates
        '(("d" "default" plain (function org-roam--capture-get-point)
           "%?"
           :file-name "${slug}"
           :head "#+SETUPFILE:./hugo_setup.org
#+HUGO_SECTION: zettels
#+HUGO_SLUG: ${slug}
#+TITLE: ${title}\n"
           :unnarrowed t)
          ("p" "private" plain (function org-roam--capture-get-point)
           "%?"
           :file-name "private-${slug}"
           :head "#+TITLE: ${title}\n"
           :unnarrowed t)))
  (setq org-roam-ref-capture-templates
        '(("r" "ref" plain (function org-roam--capture-get-point)
           "%?"
           :file-name "websites/${slug}"
           :head "#+SETUPFILE:./hugo_setup.org
#+ROAM_KEY: ${ref}
#+HUGO_SLUG: ${slug}
#+TITLE: ${title}
- source :: ${ref}"
           :unnarrowed t))))
;;;; company-org-roam
(use-package company-org-roam
  :when (featurep! :completion company)
  :after org-roam
  :config
  (set-company-backend! 'org-mode '(company-org-roam company-yasnippet company-dabbrev)))

(after! (org org-roam)
    (defun my/org-roam--backlinks-list (file)
      (if (org-roam--org-roam-file-p file)
          (--reduce-from
           (concat acc (format "- [[file:%s][%s]]\n"
                               (file-relative-name (car it) org-roam-directory)
                               (org-roam--get-title-or-slug (car it))))
           "" (org-roam-sql [:select [file-from]
                             :from file-link                             :where (= file-to $s1)
                             :and file-from :not :like $s2] file "%private%"))
        ""))
    (defun my/org-export-preprocessor (_backend)
      (let ((links (my/org-roam--backlinks-list (buffer-file-name))))
        (unless (string= links "")
          (save-excursion
            (goto-char (point-max))
            (insert (concat "\n* Backlinks\n" links))))))
    (add-hook 'org-export-before-processing-hook 'my/org-export-preprocessor))

;;;; Hyperbole
(eval-after-load "hyperbole"
  '(progn
     (define-key org-mode-map (kbd "<M-return>") nil)))


;;;; notdeft
(after! notdeft
  (load "notdeft-example")
  (setq notdeft-xapian-program "/home/alexander/.emacs.d/.local/straight/repos/notdeft/xapian/notdeft-xapian")
  (setq notdeft-directories '("/home/alexander/Sync/org/notes"))


  (setq notdeft-template
        "#+TITLE:
:HIDDEN:
,#+SETUPFILE: ./hugo_setup.org
,#+DATE:
,#+HUGO_TAGS:
:END:
# <glink: notes-menu>




,* Related
,* External Links
"))
