;;;; modalka
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
      "<backtab>" 'crux-switch-to-previous-buffer
      "<return>" 'hkey-either
      "SPC" 'gbut:act
      "<backspace>" 'hhist:remove

      "a"  'ignore
      "b"  '+ivy/switch-workspace-buffer
      "c"  'org-capture
      "d"  'dired-jump
      "e"  'ignore
      "f"  'avy-goto-char ;'counsel-find-file
      "g"  'ignore
      "h"  'bury-buffer
      "i"  #'insert-mode-modalka
      "j"  'next-line ;'avy-goto-char
      "k"  'previous-line;'kill-buffer
      "l"  'ignore
      "m"  'ignore
      "n"  'org-tree-to-indirect-buffer
      "o"  'other-window
      "p"  'as/treefactor-hydra
      "q"  'org-working-set
      "r"  'counsel-recentf
      "s"  'swiper
      "t"  '+treemacs/toggle
      "u"  'ignore
      "v"  'point-to-register
      "w"  '+workspace/switch-to
      "x"  'counsel-M-x
      "y"  'ignore
      "z"  'ignore


      "A"  'ignore
      "B"  'counsel-switch-buffer
      "C"  'ignore
      "D"  'ignore
      "E"  'ignore
      "F"  'counsel-find-file
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
      "Q"  'as/working-set
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

(map!
 (
  "C-s" #'swiper
  "<f12>" #'as/find-overview
;  :desc "imenu" "C-c j" #'counsel-imenu
  ))
