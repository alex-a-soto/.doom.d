(cond
 ((string-equal system-name "exobrain")
  (load-file (concat "~/.doom.d/packages/" system-name ".el"))))

(cond
 ((string-equal system-name "SurfaceGo")
  (load-file (concat "~/.doom.d/packages/" system-name ".el"))))
