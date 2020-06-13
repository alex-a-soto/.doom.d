;;; $DOOMDIR/config.el -*- lexical-binding: t; -*-
;;
;;
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
;; To get information about any of these functions/macros, move the cursor over
;; the highlighted symbol at press 'K' (non-evil users must press 'C-c g k').
;;
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
;;
(setq user-home-directory (expand-file-name "~/"))


;; This determines the style of line numbers in effect. If set to `nil', line
;; numbers are disabled. For relative line numbers, set this to `relative'.

;;;; Display Line Numbers
(setq display-line-numbers-type t)
(setq global-display-line-numbers-mode nil)


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

(use-package! org-now
  :general (:keymaps 'org-mode-map
            :prefix "M-SPC"
            "rl" #'org-now-link
            "rn" #'org-now-refile-to-now
            "rp" #'org-now-refile-to-previous-location))



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

(defun as/create-workspace ()
  (interactive)
  (let* ((name (read-from-minibuffer "Name:")))
    (+workspace-new name)
    (+workspace-switch name))
  (+workspace/display))


(map! :leader
      :prefix "w"
      :map winner-mode-map
      :desc "Toggle Window" "TAB" #'as/toggle-one-window)

(map! :leader
      :prefix "w"
      :map winner-mode-map
      :desc "Create Workspace" "c" #'as/create-workspace)

;;;; Fold
(map! :leader
      :desc "Fold" "TAB" #'+fold/toggle)


;;;; Add some hooks for saving buffers
(add-hook 'focus-out-hook (lambda () (interactive)(save-some-buffers t)))
;; save when frame is closed
(add-hook 'delete-frame-functions (lambda () (interactive)(save-some-buffers t)))

;;;; Enable narrow functions
(put 'narrow-to-page 'disabled nil)
(put 'narrow-to-region 'disabled nil)
;;;; dired
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

;;; dired-hide-dotfiles
(use-package! dired-hide-dotfiles
  :config
  (define-key dired-mode-map "." #'dired-hide-dotfiles-mode)
  (add-hook! 'dired-mode-hook 'as/dired-mode-hook)
  (defun as/dired-mode-hook ()
    (dired-hide-dotfiles-mode +1)))



;;;; Workspaces
(after! persp-mode
  (setq persp-emacsclient-init-frame-behaviour-override "scratch"))

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
;;;; Company
(after! company
  (setq company-echo-delay 0)
  (setq company-idle-delay 0.2)
  (setq company-minimum-prefix-length 2)
  (setq company-tooltip-limit 20))

(custom-set-faces
 '(aw-leading-char-face
   ((t (:inherit ace-jump-face-foreground :height 3.0)))))

;;;; org-roam
(use-package! org-roam
  ;; :hook
  ;; (after-init . org-roam-mode)
  ;; :custom-face
  ;; (org-roam-link ((t (:inherit org-link :foreground "#005200"))))
  :init
  ;; (map! :leader
  ;;       :prefix "n"
  ;;       :desc "org-roam" "l" #'org-roam
  ;;       :desc "org-roam-insert" "i" #'org-roam-insert
  ;;       :desc "org-roam-switch-to-buffer" "b" #'org-roam-switch-to-buffer
  ;;       :desc "org-roam-find-file" "f" #'org-roam-find-file
  ;;       :desc "org-roam-show-graph" "g" #'org-roam-show-graph
  ;;       :desc "org-roam-insert" "i" #'org-roam-insert)

  (setq org-roam-directory "/home/alexander/2-Reference/5-Location/LearnInPublic/"
        org-roam-db-location "/home/alexander/.doom.d/org-roam.db"
                                        ;        org-roam-graph-exclude-matcher "private"
        ))

;;;; company-org-roam
(use-package company-org-roam
  :when (featurep! :completion company)
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
;;; def
(setq deft-directory "/home/alexander/0-Inbox")

;;;; Notdeft
(map! :leader
      :prefix "n"
      :desc "Open NotDeft" "d" #'notdeft
      :desc "Create Note" "n" #'as/notdeft-new-file-named
      :desc "#LearningInPublic" "p" #'as/LearnInPublic?)


(defun notdeft-file-format (title)
  "Convert TITLE to a filename-suitable slug."
  (cl-flet* ((nonspacing-mark-p (char)
                                (eq 'Mn (get-char-code-property char 'general-category)))
             (strip-nonspacing-marks (s)
                                     (apply #'string (seq-remove #'nonspacing-mark-p
                                                                 (ucs-normalize-NFD-string s))))
             (cl-replace (title pair)
                         (replace-regexp-in-string (car pair) (cdr pair) title)))
    (let* ((pairs `(("[^[:alnum:][:digit:]]" . "_")  ;; convert anything not alphanumeric
                    ("__*" . "_")  ;; remove sequential underscores
                    ("^_" . "")  ;; remove starting underscore
                    ("_$" . "")))  ;; remove ending underscore
           (slug (-reduce-from #'cl-replace (strip-nonspacing-marks title) pairs)))
      (downcase slug)
      (concat (format-time-string "%Y-%m-%dT%H.%M.%S") "_" slug))))





(setq notdeft-notename-function 'notdeft-file-format)

(defun as/notdeft-new-file-named ()
  (interactive)
  (setq notdeft-directory "/home/alexander/2-Reference/5-Location/LearnInPublic")
  (let ((title (read-string "New note: ")))
    (notdeft-new-file-named nil title notdeft-template)
    (goto-char (point-min))
    (re-search-forward "^#\\+TITLE:.*$" nil t)
    (insert " " title)
    (re-search-forward "^#\\+DATE:.*$" nil t)
    (insert (format-time-string " [%Y-%m-%d-%H%M]"))
    (goto-char (point-min))
    (goto-line 5)
    ))


(setq org-roam-capture-templates '(("d" "default" plain #'org-roam-capture--get-point "%?" :file-name "%<%Y%m%d%H%M%S>-${slug}" :head "#+TITLE: ${title}\n#+KEYWORDS:\n\n\n\n\n* Related\n* External Links" :unnarrowed t)))

(setq notdeft-template
      "#+TITLE:
#+DATE:
#+KEYWORDS:



* Related
* External Links
")

(after! notdeft
  (load "notdeft-example")
  (setq notdeft-xapian-program "/home/alexander/.bin/Notdeft/notdeft-xapian")
  (setq notdeft-directories '("/home/alexander/0-Inbox"))
  (setq notdeft-sparse-directories
        `(("~" . ,(mapcar
                   (lambda (file) (file-relative-name file "~"))
                   (file-expand-wildcards "~/2-Reference/5-Location/LearnInPublic/*.org")))))

  (setq notdeft-time-format " %Y-%m-%d-%H%M")

  (defun notdeft-new-file (pfx)
    "Create a new file quickly.
Create it with an automatically generated name, one based
on the `notdeft-filter-string' filter string if it is non-nil.
With a prefix argument PFX, offer a choice of NotDeft
directories, when there is more than one of them.
With two prefix arguments, also offer a choice of filename
extensions when `notdeft-secondary-extensions' is non-empty.
Return the filename of the created file."
    (interactive "P")
    (let ((data (and notdeft-filter-string))
          (notename
           (and notdeft-filter-string
                (notdeft-title-to-notename notdeft-filter-string))))
      (setq notdeft-template (concat
                              "#+TITLE: " data
                              "\n#+DATE:"
                              "\n#+KEYWORDS:\n\n\n\n"


                              "* Related\n"
                              "* External Links"
                              ))
      (notdeft-sub-new-file notdeft-template notename pfx)
      (goto-char (point-min))
      (re-search-forward "^#\\+TITLE:.*$" nil t)
      (re-search-forward "^#\\+DATE:.*$" nil t)
      (insert (format-time-string " [%Y-%m-%d-%H%M]"))
      (goto-char (point-min))
      (goto-line 5)

      )))


;;;; pdf-tools
;;;; org
(use-package! org
  :config
  (setq org-replace-disputed-keys t)
  (setq org-return-follows-link t)
  (setq org-catch-invisible-edits 'show)


  (add-hook
   'org-mode-hook
   (lambda ()
     (visual-line-mode 1)))

  (setf org-blank-before-new-entry '((heading . nil) (plain-list-item . nil)))
  (setq org-ellipsis "⤵")
  (setq org-use-speed-commands t))

;;;;; org-todo-keywords
(setq org-todo-keywords
	    (quote ((sequence "TODO(t)" "NEXT(n)" "IN-PROGRESS(i)" "|" "DONE(d)")
		          (sequence "WAITING(w)" "HOLD(h)" "DELEGATE(g)" "|" "CANCELLED(c)" "SCHEDULED(s)"))))

(setq org-treat-S-cursor-todo-selection-as-state-change nil)

(custom-declare-face '+org-todo-active '((t (:inherit (bold font-lock-constant-face org-todo)))) "")
(custom-declare-face '+org-todo-project '((t (:inherit (bold font-lock-doc-face org-todo)))) "")
(custom-declare-face '+org-todo-onhold '((t (:inherit (bold warning org-todo)))) "")

;;;;; org-todo-keyword-faces
(setq org-todo-keyword-faces
      (quote (("TODO" :foreground "red" :weight bold)
	            ("NEXT" :foreground "#007cee" :weight bold)
              ("IN-PROGRESS" :foreground "yellow" :weight bold)
	            ("DONE" :foreground "forest green" :weight bold)
	            ("WAITING" :foreground "orange" :weight bold)
              ("HOLD" :foreground "magenta" :weight bold)
	            ("DELEGATE" :foreground "purple" :weight bold)
	            ("CANCELLED" :foreground "red" :weight bold)
	            ("SCHEDULED" :foreground "forest green" :weight bold)

              (quote (("[-]"  . +org-todo-active)
                      ("STRT" . +org-todo-active)
                      ("[?]"  . +org-todo-onhold)
                      ("WAIT" . +org-todo-onhold))))))
;;;;; org-todo-state-tags-triggers
(setq org-todo-state-tags-triggers
	    (quote (("CANCELLED" ("CANCELLED" . t))
		          ("WAITING" ("WAITING" . t))
		          ("HOLD" ("WAITING") ("HOLD" . t))
		          (done ("WAITING") ("HOLD"))
		          ("TODO" ("WAITING") ("CANCELLED") ("HOLD"))
		          ("NEXT" ("WAITING") ("CANCELLED") ("HOLD"))
		          ("DONE" ("WAITING") ("CANCELLED") ("HOLD")))))

(setq org-use-fast-todo-selection t)

;;;;; org-refile
(setq org-id-locations-file "/home/alexander/.doom.d/.orgids")

;(setq org-refile-use-outline-path 'file)
;(setq org-outline-path-complete-in-steps nil)

(setq org-refile-allow-creating-parent-nodes (quote confirm))

;;;;; org-babel
(defadvice org-babel-execute-src-block (around load-language nil activate)
  "Load language if needed"
  (let ((language (org-element-property :language (org-element-at-point))))
    (unless (cdr (assoc (intern language) org-babel-load-languages))
	    (add-to-list 'org-babel-load-languages (cons (intern language) t))
	    (org-babel-do-load-languages 'org-babel-load-languages org-babel-load-languages))
    ad-do-it))

(setq org-confirm-babel-evaluate nil)

(setq org-src-fontify-natively t
	    org-src-tab-acts-natively t
	    org-src-preserve-indentation t
	    org-src-window-setup 'current-window)

(setq org-confirm-babel-evaluate nil)
(setq org-babel-results-keyword "results")

(defun as/display-inline-images ()
  (condition-case nil
	    (org-display-inline-images)
    (error nil)))

(add-hook 'org-babel-after-execute-hook 'as/display-inline-images 'append)

;;;;; org-tag-alist
(setq org-tag-alist (quote
                     (
                      ("NOTE" . ?n)
                      ("resilient" . ?r)
                      ("learning" . ?l)
                      ("sancocho" . ?s)
                      ("personal" . ?p)
                      ("crypt" . ?c)
                      )))

(setq org-fast-tag-selection-include-todo t)
(setq org-fast-tag-selection-single-key (quote expert))
;;;;; org-column
(setq org-columns-default-format
      "%1PRIORITY(P) %50ITEM(Task) %12ALLTAGS(Area) %50OUTCOME(Outcome) %6Effort(Effort){:} %6ENERGY(Energy) %5CLOCKSUM(Clock) %10DEADLINE(Deadline)")

;;;;; global-properties
(setq org-global-properties (quote (("Effort_ALL" . "0:05 0:10 0:15 0:25 0:45 1:00 1:30 2:00 3:00 4:00 5:00 6:00 0:00")
				                            ("STYLE_ALL" . "habit"))))
;;;;; org-capture templates
(require 'org-protocol)
(require 'org-expiry)

(defun org-journal-find-location ()
  (interactive)
  (journal-file-today)
  (goto-char (point-min)))


(add-hook 'org-capture-mode-hook (lambda () (call-interactively 'org-store-link)))

(setq org-inbox-file "~/1-Agenda/Inbox.org")

(setq org-capture-templates
	    (quote
       (("t" "Task" entry (file org-inbox-file) (function as/quick-capture))
        ("p" "Project" entry (file org-inbox-file) (file "~/.doom.d/templates/new-project.org"))
        ("e" "Event" entry (file org-inbox-file) "* %^{Event} %^g \n\n%^{When?}t\n")
        ("l" "Link" entry (file org-inbox-file) "* %(org-cliplink-capture) \n %(org-insert-time-stamp nil t nil nil nil nil) \n\n %?" :immediate-finish t)

        ("n" "Note")
		    ("nn" "Note" entry (file org-inbox-file) "* %^{Note} \n %(org-insert-time-stamp nil t nil nil nil nil) \n\n %?")
        ("np" "Plain Note" entry (file org-inbox-file) "* %^{Note} \n %T \n %?")
        ("ns" "Selection" entry (file org-inbox-file) "* %^{Title}  \nSource: %u, [[%F][%f]]\n\n %(org-insert-time-stamp nil t nil nil nil nil)\n\n #+BEGIN_QUOTE\n%i\n#+END_QUOTE\n\n %?")

        ("c" "Cooking")
        ("cc" "Cookbook" entry (file org-inbox-file) "%(org-chef-get-recipe-from-url)" :empty-lines 1)
        ("cm" "Manual Cookbook" entry (file org-inbox-file) "* %^{Recipe title: }\n  :PROPERTIES:\n  :source-url:\n  :servings:\n  :prep-time:\n  :cook-time:\n  :ready-in:\n  :END:\n** Ingredients\n   %?\n** Directions\n\n")



        ("j" "Journal")
		    ("jj" "Journal" entry (file org-inbox-file) "* %^{Title} %^g \n %(org-insert-time-stamp nil t nil nil nil nil) \n\n %?")
		    ("jp" "Problem" entry (file org-inbox-file) "* Problem - %^{Domain} %^g \n %(org-insert-time-stamp nil t nil nil nil nil) \n\n *Problem:* %?\n\n *Insight:*\n\n *Tomorrow:*\n\n")
		    ("jc" "Code" entry    (file org-inbox-file) "* Code - %^{Title} %^g \n %(org-insert-time-stamp nil t nil nil nil nil) \n\n#+BEGIN_SRC\n%i\n#+END_SRC\n\n%?")
        ("jf" "Focus Block" entry (file org-inbox-file) "* Focus - %^{Focus:} %^g \n %(org-insert-time-stamp nil t nil nil nil nil) \n\n %?")
		    ("jr" "Recovery Block" entry (file org-inbox-file) "* Recovery - %^{Recovery:} %^g \n %(org-insert-time-stamp nil t nil nil nil nil) \n\n %?")
		    ("ja" "Admin Block" entry (file org-inbox-file) "* Admin - %^{Admin:} %^g \n %(org-insert-time-stamp nil t nil nil nil nil) \n\n %?")
        ("ji" "Interruption" entry (file org-inbox-file) "* Interrupt - %? :interrupt:  \n %(org-insert-time-stamp nil t nil nil nil nil) \n\n")
        ("jg" "Github Issue" entry (file org-inbox-file) "* %? \n- Bug Description\n\n- Steps to Reproduce\n\n- Actual Result\n\n- Expected Result\n\n- Build Version  \n\n")

        ("r" "Routines")
        ("rr" "Morning Routine" entry (file org-inbox-file) (file "~/.doom.d/templates/morning-routine.org"))
        ("re" "Evening Routine" entry (file org-inbox-file) (file "~/.doom.d/templates/evening-routine.org"))
        ("rb" "Bedtime Routine" entry (file org-inbox-file) (file "~/.doom.d/templates/night-routine.org"))
        ("r!" "Meta Review" entry (file+olp+datetree "/tmp/reviews.org") (file "~/.doom.d/templates/meta.org"))

        ("w" "Review")
        ("wd" "Daily Review" entry (file org-inbox-file) (file "~/.doom.d/templates/daily-review.org"))
        ("ww" "Weekly Review" entry (file org-inbox-file) (file "~/.doom.d/templates/weekly-review.org"))
        ("wm" "Monthly Review" entry (file org-inbox-file) (file "~/.doom.d/templates/monthly-review.org"))
        ("wy" "Yearly Review" entry (file org-inbox-file) (file "~/.doom.d/templates/annual-review.org"))


        )))


(defun as/quick-capture-status ()
  "Create and return a TODO heading template"
  (interactive)
  (let* ((title (read-from-minibuffer "Capture: ")))
    (with-temp-buffer
      (org-mode)
      (org-insert-heading)
      (insert (concat title " "))
      (org-todo)
      (buffer-string))))

(defun as/quick-capture ()
  (mapconcat #'identity
             `(
               ,(as/quick-capture-status)
               "%(org-insert-time-stamp nil t nil nil nil nil)\n\n%?")
             "\n"))

(setq org-agenda-time-grid '((daily today require-timed)
                             (600 1000 1100 1200 1300 1400 1500 1600 1700 1800 1900 2000 2100 2200)
                             "......" "----------------"))

(setq org-capture-use-agenda-date t)


;;;; org-agenda
(after! org-agenda
  (require 'ox-org)


  (setq org-agenda-hide-tags-regexp "noexport\\|ATTACH")

  ;; (setq org-agenda-text-search-extra-files (quote (
  ;;                                                  )))


  (setq org-archive-location "/home/alexander/2-Reference/2-Time/Archive/%s_archive::")

  (setq org-time-stamp-rounding-minutes (quote (1 1)))

  (setq org-default-priority ?D)
  (setq org-agenda-follow-indirect t)

  (setq org-agenda-window-setup 'current-window)
  (setq org-agenda-dim-blocked-tasks nil)
  (setq org-agenda-use-tag-inheritance nil)
  (setq org-agenda-compact-blocks t)

  (setq org-agenda-restriction-lock-highlight-subtree nil)

  (add-hook 'org-agenda-mode-hook
	          '(lambda () (hl-line-mode 1))
	          'append)

  (setq org-agenda-tags-todo-honor-ignore-options t)
  (setq org-agenda-inhibit-startup t)

  (setq org-agenda-log-mode-items (quote (closed state clock)))
  (setq org-agenda-show-all-dates t)
  (setq org-agenda-start-on-weekday nil)

  (setq org-agenda-tags-column -100
	      org-agenda-start-with-log-mode nil)

  (setq org-agenda-entry-text-maxlines 20)
  (setq org-agenda-entry-text-leaders " ")
  (setq org-agenda-timegrid-use-ampm t)
  (setq org-agenda-include-diary nil)
  (setq org-agenda-insert-diary-extract-time t)
  (setq org-log-done t)
  (setq org-agenda-span 'day)
  (setq org-agenda-start-day nil)

  (defun my-org-agenda-recent-open-loops ()
    (interactive)
    (let ((org-agenda-start-with-log-mode t)
          (org-agenda-use-time-grid nil))
      (org-agenda-list nil (org-read-date nil nil "-2d") 4)))

  (defun my-org-agenda-longer-open-loops ()
    (interactive)
    (let ((org-agenda-start-with-log-mode t)
          (org-agenda-use-time-grid nil))
      (org-agenda-list 'file (org-read-date nil nil "-14d") 28)))


  (setq organization-task-id "0f1cf581-cd20-4acd-bf70-e2174635579c")


  (defun as/punch-in (arg)
    (interactive "p")
    (setq as/keep-clock-running t)
    (if (equal major-mode 'org-agenda-mode)
        (let* ((marker (org-get-at-bol 'org-hd-marker))
               (tags (org-with-point-at marker (org-get-tags-at))))
          (if (and (eq arg 4) tags)
              (org-agenda-clock-in '(16))
            (as/clock-in-organization-task-as-default)))
      (save-restriction
        (widen)
        (if (and (equal major-mode 'org-mode) (not (org-before-first-heading-p)) (eq arg 4))
            (org-clock-in '(16))
          (as/clock-in-organization-task-as-default)))))

  (defun as/clock-in-organization-task-as-default ()
    (interactive)
    (org-with-point-at (org-id-find organization-task-id 'marker)
      (org-clock-in '(16))))

  (defun as/punch-out ()
    (interactive)
    (setq as/keep-clock-running nil)
    (when (org-clock-is-active)
      (org-clock-out))
    (org-agenda-remove-restriction-lock))


;;;; org-super-agenda
  (org-super-agenda-mode t)
  (setq org-agenda-skip-scheduled-if-done t
        org-agenda-skip-deadline-if-done t
        org-agenda-include-deadlines t
        org-agenda-include-diary t
        org-agenda-block-separator t
        org-agenda-compact-blocks t
        org-agenda-start-with-log-mode nil)


  (setq org-agenda-custom-commands
	      '(
          ("o" "Overview"
           ((agenda "" ((org-agenda-span 1)
			                  (org-super-agenda-groups
			                   '((:name "Habits"
				                    :habit t
                            :order 2)

			                     (:name "Schedule"
				                    :time-grid t
				                    :scheduled t
				                    :order 1)
			                     (:discard (:anything t))))))


            (alltodo "" ((org-agenda-overriding-header "Stuck Project")
                         (org-super-agenda-groups
                          '( (:name none
                              :discard (:children "NEXT")
                              :discard (:children "IN-PROGRESS")
                              :order 4)

                             (:name none
                              :discard (:children nil)
                              :order 4)
                             (:name none
                              :children todo)))))


            (alltodo "" ((org-agenda-overriding-header "Active Project")
                         (org-super-agenda-groups
                          '((:name none
                             :children "NEXT"
                             :children "IN-PROGRESS"
                             :order 1)
                            (:discard (:anything t))))))

            (alltodo "" ((org-agenda-overriding-header "In-Progress")
                         (org-super-agenda-groups
                          '((:name none
                             :discard (:not (:todo ("IN-PROGRESS")))
                             :discard (:habit)
                             :order 6)
                            (:name none
                             :todo t
                             :face (:underline t))
                            ))))

            (alltodo "" ((org-agenda-overriding-header "Next Task")
                         (org-super-agenda-groups
                          '((:name none
                             :discard (:not (:todo "NEXT"))
                             :discard (:tag "HOLD")
                             :discard (:habit)
                             :order 5)
                            (:name none
                             :todo "NEXT"
                             :face (:background "" :underline t))))))

            (alltodo "" ((org-agenda-overriding-header "Project Task")
                         (org-agenda-skip-function 'bh/skip-non-project-tasks)
                         (org-super-agenda-groups
                          '((:name none
                             :discard (:tag "HOLD")
                             :todo t
                             :order 5)))))

            (alltodo "" ((org-agenda-overriding-header "Standalone Task")
                         (org-agenda-skip-function 'bh/skip-project-tasks)
                         (org-super-agenda-groups
                          '((:name none
                             :discard (:tag "REFILE")
                             :todo ("TODO")
                             :order 7)
                            (:discard (:anything t))))))

            (alltodo "" ((org-agenda-overriding-header "Waiting for, Delegate, Hold")
                         (org-agenda-skip-function 'bh/skip-project-tasks)
                         (org-super-agenda-groups
                          '((:name none
                             :discard (:tag "REFILE")
                             :tag ("HOLD")
                             :todo ("WAITING" "DELEGATE")
                             :order 7)
                            (:discard (:anything t))))))


            ))
          ))

        (defun bh/is-project-p ()
          "Any task with a todo keyword subtask"
          (save-restriction
            (widen)
            (let ((has-subtask)
                  (subtree-end (save-excursion (org-end-of-subtree t)))
                  (is-a-task (member (nth 2 (org-heading-components)) org-todo-keywords-1)))
              (save-excursion
                (forward-line 1)
                (while (and (not has-subtask)
                            (< (point) subtree-end)
                            (re-search-forward "^\*+ " subtree-end t))
                  (when (member (org-get-todo-state) org-todo-keywords-1)
                    (setq has-subtask t))))
              (and is-a-task has-subtask))))

        (defun bh/find-project-task ()
          "Move point to the parent (project) task if any"
          (save-restriction
            (widen)
            (let ((parent-task (save-excursion (org-back-to-heading 'invisible-ok) (point))))
              (while (org-up-heading-safe)
                (when (member (nth 2 (org-heading-components)) org-todo-keywords-1)
                  (setq parent-task (point))))
              (goto-char parent-task)
              parent-task)))

        (defun bh/skip-non-tasks ()
          "Show non-project tasks.
Skip project and sub-project tasks, habits, and project related tasks."
          (save-restriction
            (widen)
            (let ((next-headline (save-excursion (or (outline-next-heading) (point-max)))))
              (cond
               ((bh/is-task-p)
                nil)
               (t
                next-headline)))))

        (defun bh/skip-project-tasks ()
          "Show non-project tasks.
Skip project and sub-project tasks, habits, and project related tasks."
          (save-restriction
            (widen)
            (let* ((subtree-end (save-excursion (org-end-of-subtree t))))
              (cond
               ((bh/is-project-p)
                subtree-end)
               ((org-is-habit-p)
                subtree-end)
               ((bh/is-project-subtree-p)
                subtree-end)
               (t
                nil)))))

        (defun bh/is-task-p ()
          "Any task with a todo keyword and no subtask"
          (save-restriction
            (widen)
            (let ((has-subtask)
                  (subtree-end (save-excursion (org-end-of-subtree t)))
                  (is-a-task (member (nth 2 (org-heading-components)) org-todo-keywords-1)))
              (save-excursion
                (forward-line 1)
                (while (and (not has-subtask)
                            (< (point) subtree-end)
                            (re-search-forward "^\*+ " subtree-end t))
                  (when (member (org-get-todo-state) org-todo-keywords-1)
                    (setq has-subtask t))))
              (and is-a-task (not has-subtask)))))

        (defun bh/is-project-subtree-p ()
          "Any task with a todo keyword that is in a project subtree.
Callers of this function already widen the buffer view."
          (let ((task (save-excursion (org-back-to-heading 'invisible-ok)
                                      (point))))
            (save-excursion
              (bh/find-project-task)
              (if (equal (point) task)
                  nil
                t))))


        (defun bh/skip-non-project-tasks ()
          "Show project tasks.
Skip project and sub-project tasks, habits, and loose non-project tasks."
          (save-restriction
            (widen)
            (let* ((subtree-end (save-excursion (org-end-of-subtree t)))
                   (next-headline (save-excursion (or (outline-next-heading) (point-max)))))
              (cond
               ((bh/is-project-p)
                next-headline)
               ((org-is-habit-p)
                subtree-end)
               ((and (bh/is-project-subtree-p)
                     (member (org-get-todo-state) (list "NEXT" "IN-PROGRESS")))
                subtree-end)
               ((not (bh/is-project-subtree-p))
                subtree-end)
               (t
                nil)))))
        )

;;;; org-attach
  (setq org-attach-id-dir  "~/2-Reference/9-Codex")
  (setq org-attach-method 'mv)

  (defun as/goto-inbox ()
    (interactive)
    (find-file "~/1-Agenda/Inbox.org")
    (dired-other-window "~/0-Inbox")
    (dired-hide-details-mode)
    )

  (defun as/add-to-inbox ()
    (interactive)
    (dired-mark nil)
    (dired-copy-filename-as-kill '(4))
    (other-window 1 nil)
    (goto-char (point-max))
    (newline-and-indent)
    (end-of-line)
    (call-interactively 'org-insert-heading-respect-content nil)
    (yank)
    (org-id-get-create)
    (other-window 1 nil)
    (call-interactively 'org-attach-dired-to-subtree)
    )

;;;; Mapping with which-key

  (map! :leader
        :prefix "j"
        :desc "Jump to word" "w" #'avy-goto-word-0)

  (map! :leader
        :prefix "o"
        :desc "Open Daylog" "l" #'as/treefactor-hydra)

  (map! :leader
        :prefix "j"
        :desc "Jump to char" "j" #'avy-goto-char)

  (map! :leader
        :prefix "j"
        :desc "Goto-line" "l" #'avy-goto-line)

  (setq ivy-sort-matches-functions-alist
        '((t)
          (ivy-switch-buffer . ivy-sort-function-buffer)))


  (use-package! counsel
    :config
    (setq grep-command "rg -M 120 --with-filename --no-heading --line-number --color never %s")
    :bind
    ("C-s" . 'counsel-grep-or-swiper))


;;; flycheck-ledger
  (use-package! flycheck-ledger
    :demand t
    :after (flycheck ledger-mode)
    :init (progn
            (add-hook 'ledger-mode-hook #'flycheck-mode)
            (setq flycheck-ledger-zero-accounts '("Assets:Budget:Available"
                                                  "Assets:Budget:Unbudgeted"
                                                  "Assets:Expenses:InternalTransfer"))))

;;; elfeed
  (after! elfeed

    (setf url-queue-timeout 30)

    (elfeed-org)

    (setq rmh-elfeed-org-files (list "~/2-Reference/9-Codex/08/6e648e-2baf-4775-a0bc-8153ae99e505/rss.org"))

    (setq-default elfeed-search-filter "@4-hours-ago +unread")




    (defun as/elfeed-load-db-and-open ()
      "Wrapper to load the elfeed db from disk before opening"
      (interactive)
      (elfeed-db-load)
      (elfeed)
      (elfeed-update)
      (setq elfeed-search-filter "@1-hours-ago +unread")
      (elfeed-search-update--force))

    ;;write to disk when quiting
    (defun as/elfeed-save-db-and-bury ()
      "Wrapper to save the elfeed db to disk before burying buffer"
      (interactive)
      (elfeed-db-save)
      (quit-window))

    (define-key elfeed-search-mode-map "q" 'as/elfeed-save-db-and-bury)
    (define-key elfeed-search-mode-map "h"
      (lambda ()
        (interactive)
        (elfeed-search-set-filter (default-value 'elfeed-search-filter))))

    (define-key elfeed-search-mode-map (kbd "j") #'next-line)
    (define-key elfeed-search-mode-map (kbd "k") #'previous-line)

    (defun as/elfeed-yank-capture ()
      (interactive)
      (elfeed-search-yank)
      (org-capture nil "l")
      (message "Entry saved to Inbox"))

    (define-key elfeed-search-mode-map "y" #'as/elfeed-yank-capture)
    )

;;; frame-font
  (setq my-font-name "DejaVu Sans Mono")
  (defcustom my-font-size 12 "My font size")

  (defun set-frame-font-size (&optional font-size)
    "Change frame font size to FONT-SIZE.
If no FONT-SIZE provided, reset the size to its default variable."
    (let ((font-size
           (or font-size
               (car (get 'my-font-size 'standard-value)))))
      (customize-set-variable 'my-font-size font-size)
      (set-frame-font
       (format "%s %d" my-font-name font-size) nil t)))

  (defun increase-frame-font ()
    "Increase frame font by one."
    (interactive)
    (set-frame-font-size (+ my-font-size 1)))

  (defun decrease-frame-font ()
    "Decrease frame font by one."
    (interactive)
    (set-frame-font-size (- my-font-size 1)))

  (defun reset-frame-font ()
    "Reset frame font to its default value."
    (interactive)
    (set-frame-font-size))

;;; modalka
  (use-package! modalka
    :config
    (modalka-global-mode 1)

    (defun normal-mode-modalka ()
	    (interactive)
	    (if (modalka-mode nil)
		      (modalka-mode 1)
	      (nil)))

    (defun insert-mode-modalka ()
	    (interactive)
	    (modalka-mode 0))

    (setq-default cursor-type '(bar . 2))
    (setq modalka-cursor-type 'box)

    (setq org-capture-mode-hook 'insert-mode-modalka)

    (defun modalka-select-major-mode (modalka-mode-map)
	    (let ((modalka-mode-command (cdr (assoc major-mode modalka-mode-map))))
	      (if modalka-mode-command (apply modalka-mode-command))))

    (defun modalka-mode-hydra ()
	    (interactive)
	    (modalka-select-major-mode modalka-major-mode-hydra-list))


    (custom-set-variables
     '(modalka-excluded-modes
	     (quote
	      (
	       ediff-mode
	       helpful-mode
	       dired-mode
	       magit-mode
	       magit-popup-mode
	       debugger-mode
	       ediff-mode
	       help-mode
	       git-rebase-mode
	       help-mode
	       org-agenda-mode
	       org-capture-mode
	       emms-playlist-mode
	       pdf-tools-modes
	       undo-tree-visualizer-mode
	       ))))

    (define-key modalka-mode-map [remap self-insert-command] 'ignore)
    (define-key global-map [escape] #'normal-mode-modalka)

    (general-def modalka-mode-map

      "<tab>" '+workspace/other
      "<tab>" '+workspace/other
      "<backtab>" 'crux-switch-to-previous-buffer
      "<return>" 'hkey-either
      "SPC" 'ignore
      "<backspace>" 'ignore

      "a"  'ignore
      "b"  'ivy-switch-buffer
      "c"  'org-capture
      "d"  'dired-jump
      "e"  'ignore
      "f"  'counsel-find-file
      "g"  'ignore
      "h"  'bury-buffer
      "i"  #'insert-mode-modalka
      "j"  'avy-goto-char
      "k"  'kill-buffer
      "l"  'ignore
      "m"  'ignore
      "n"  'org-tree-to-indirect-buffer
      "o"  'ignore
      "p"  'treefactor-throw
      "q"  'ignore
      "r"  'counsel-recentf
      "s"  'ignore
      "t"  '+treemacs/toggle
      "u"  'ignore
      "v"  'point-to-register
      "w"  '+workspace/switch-to
      "x"  'ignore
      "y"  'ignore
      "z"  'ignore


      "A"  'ignore
      "B"  'ignore
      "C"  'ignore
      "D"  'ignore
      "E"  'ignore
      "F"  'ignore
      "G"  'ignore
      "H"  'unbury-buffer
      "I"  'ignore
      "J"  'ignore
      "K"  'kill-this-buffer
      "L"  'ignore
      "M"  'ignore
      "N"  'ignore
      "O"  'ignore
      "P"  'ignore
      "Q"  'ignore
      "R"  'ignore
      "S"  'ignore
      "T"  'ignore
      "U"  'ignore
      "V"  'jump-to-register
      "W"  'ignore
      "X"  'ignore
      "Y"  'ignore
      "Z"  'ignore


      "0"  'delete-window
      "1"  'delete-other-windows
      "2"  'split-window-below
      "3"  'split-window-right
      "4"  'ignore
      "5"  'ignore
      "6"  'ignore
      "8"  'ignore
      "9"  'ignore

      "!"  'shell-command
      "@"  'hycontrol-windows-grid
      "#"  'ignore
      "$"  'ignore
      "%"  'ignore
      "^"  'ignore
      "&"  'ignore
      "*"  'ignore
      "("  'ignore
      ")"  'ignore
      "-"  'ignore
      "+"  'ignore
      "<"  'ignore
      ">"  'ignore
      "?"  'ignore
      ";"  'ignore
      "'"  'ignore
      "\\" 'ignore
      "["  'ignore
      "]"  'ignore
      ","  'winner-undo
      "."  'winner-redo
      "/"  'ignore
      "'"  'ignore
      "|"  'ignore
      "="  'ignore
      "+"  'ignore
      "-"  'ignore
      "_"  'ignore)
    )

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

  (setq browse-url-browser-function #'browse-url-chrome)
  (setq hyperbole-web-search-browser-function #'browse-url-chrome)

(defun as/find-overview ()
  (interactive)
  (find-file "~/.doom.d/hyperb/HYPB"))

)

;;; org-journal
  (use-package! org-journal
    :init
    (setq org-journal-dir (format-time-string "/home/alexander/2-Reference/8-HUD/"))
    (setq org-journal-date-format "")
    (setq org-journal-file-format "%Y-%m-%d.org")
    (setq org-journal-time-format "")
    (setq org-journal-time-prefix "")
    (setq org-journal-carryover-items nil)

    (defun get-journal-file-today ()
      "Return filename for today's journal entry."
      (let ((daily-name (format-time-string "%Y-%m-%d.org")))
        (expand-file-name (concat org-journal-dir daily-name))))

    (defun journal-file-today ()
      "Create and load a journal file based on today's date."
      (interactive)
      (find-file (get-journal-file-today)))

    (global-set-key (kbd "C-c f j") 'journal-file-today)

    (defun get-journal-file-yesterday ()
      "Return filename for yesterday's journal entry."
      (let* ((yesterday (time-subtract (current-time) (days-to-time 1)))
             (daily-name (format-time-string "%Y-%m-%d.org" yesterday)))
        (expand-file-name (concat org-journal-dir daily-name))))

    (defun journal-file-yesterday ()
      "Creates and load a file based on yesterday's date."
      (interactive)
      (find-file (get-journal-file-yesterday)))

    (global-set-key (kbd "C-c f y") 'journal-file-yesterday)

    )


;;; Twittering-mode

;;; LearnInPublic
  (defun as/LearnInPublic? ()
    (interactive)
    (if (yes-or-no-p (message "#LearnInPublic?"))
        (let ((msg (read-from-minibuffer "Message: "))
              (buff (file-name-base buffer-file-name)))
          (if (buffer-modified-p)
              (if (yes-or-no-p (message "Update repository?"))
                  (as/commit msg buff))
            (if (yes-or-no-p (message "Post to Twitter?"))
                (as/plain-tweet msg buff)
              nil))
          nil)))

  (defun as/commit (msg buff)
    (interactive)
    (save-buffer)
    (magit-call-git "add" (concat buff ".org"))
    (magit-call-git "commit" "-m" msg)
    (magit-call-git "push" "origin")
    (if (yes-or-no-p (message "Post to Twitter?"))
        (as/tweet msg buff)
      nil))

  (defun as/tweet (msg url)
    (interactive)
    (let ((tags (read-from-minibuffer "Tags: ")))
      (setq tweet-template (concat
                            msg
                            "\n\n"
                            "https://github.com/alex-a-soto/LearnInPublic/blob/master/" url ".org\n\n"
                            "#LearnInPublic " tags
                            )))
    (twittering-update-status tweet-template nil nil 'normal t))

  (defun as/plain-tweet (msg url)
    (interactive)
    (let ((tags (read-from-minibuffer "Tags: ")))
      (setq tweet-template (concat
                            msg
                            "\n\n"
                            "#LearnInPublic " tags
                            )))
    (twittering-update-status tweet-template nil nil 'normal t))

  (setq browse-url-chrome-program "/usr/bin/google-chrome-stable")
  (setq browse-url-browser-function 'browse-url-chrome)

;;; xournal
  (openwith-mode t)

  (setq openwith-associations '(("\\.xopp\\'" "xournalpp" (file))))

;;; treefactor
(defhydra as/treefactor-mode-hydra ()
  ("0" as/goto-inbox "0-Inbox")
  ("1" as/treefactor-agenda "Agenda")
  ("2" as/treefactor-archive "Reference")
  ("o" other-window "Other Window")
  ("a" as/add-to-inbox "Org-attach")
  ("r" org-rename-heading "Rename heading")
  ("t" treefactor-throw "Throw")
  ("u" treefactor-up "Throw Up")
  ("s" treefactor-org-store-link-fold-drawer "Store")
  ("i" org-insert-link "Insert")
  ("k" org-cut-subtree "Cut Subtree")
  ("K" treefactor-delete-this-buffer-and-file "Delete Buffer & File")
  ("q" nil "cancel" :exit t))

(defun as/copy-to-journal ()
  (interactive)
  (let ((filename (buffer-file-name))
        (buffer (current-buffer))
        (name (buffer-name)))
    (if (not (and filename (file-exists-p filename)))
        (error "'%s' is not a file buffer" name)
      (copy-file  filename (format-time-string "/home/alexander/2-Reference/2-Time/Journal/%Y/%m/") t )
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

  (defun as/treefactor-archive ()
    (interactive)
    (delete-other-windows)
    (goto-char (point-min))
    (dired-other-window "~/2-Reference")
    (other-window -1))

;;; alert
  (use-package! alert
    :commands (alert)
    :init
    (setq alert-default-style 'libnotify))

 (setq treefactor-org-agenda-dir "~/1-Agenda/")
 (setq treefactor-org-id-extra-dir "~/2-Reference/")


(setq org-agenda-prefix-format '((agenda . " %i %?-12t% s")
 (todo . " ")
 (tags . " ")
 (search . " ")))



  (setq org-agenda-files (apply 'append
                                (mapcar
                                 (lambda (directory)
                                   (directory-files-recursively
                                    directory org-agenda-file-regexp))
                                 '("~/1-Agenda" "~/0-Inbox")
			                           )))

(require 'org-chef)

;;; Memacs functions
(defun as/get-chromehist ()
  (interactive)
  (shell-command "pkill chrome")
  (message "Killed Google Chrome")
  (shell-command "python3 ~/.bin/Memacs/bin/memacs_chrome.py -f '/home/alexander/.config/google-chrome/Default/History' -o '/home/alexander/2-Reference/2-Time/Archive/chromehist.org_archive'")
  (message (format-time-string "%Y-%m-%d-%H%M - Created chromehist.org_archive")))

(defun as/add-chromehist-to-agenda ()
  (interactive)
    (let ((buffer (current-buffer)))
      (with-temp-buffer
        (find-file "/home/alexander/1-Agenda/2-Time/chromehist.org")
        (org-agenda-file-to-front)
        (kill-buffer "chromehist.org")
        (switch-to-buffer buffer))))

(defun as/load-chromehist ()
  (interactive)
  (as/add-chromehist-to-agenda)
  (org-agenda nil "a")
  (org-agenda-archives-mode t)
  )


(defun as/write-chromehist ()
  (interactive)
  (let ((buffer (current-buffer)))
    (with-temp-buffer
      (org-agenda nil "a")
      (org-agenda-archives-mode t)
      (org-agenda-write (format-time-string "/home/alexander/2-Reference/2-Time/Journal/%Y/%m/%Y-%m-%d.org"))
      (switch-to-buffer buffer))
    ))

(defun as/memacs-chrome ()
  (interactive)
  (as/get-chromehist)
  (as/add-chromehist-to-agenda)
  (as/load-chromehist)
  (org-agenda nil "a"))


;;; org-gcal
  (load-file "~/.doom.d/gcal.el")
