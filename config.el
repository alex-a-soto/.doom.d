(cond
 ((string-equal system-name "exobrain")
  (load-file (concat "~/.doom.d/config/" system-name ".el"))))

(cond
 ((string-equal system-name "SurfaceGo")
  (load-file (concat "~/.doom.d/config/" system-name ".el"))))
