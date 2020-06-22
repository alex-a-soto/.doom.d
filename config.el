(cond
 ((string-equal system-name "exomind")
  (load-file (concat "~/.doom.d/config/" system-name ".el"))))

(cond
 ((string-equal system-name "SurfaceGo")
  (load-file (concat "~/.doom.d/config/" system-name ".el"))))

(load-file "~/.doom.d/config/defaults.el")
