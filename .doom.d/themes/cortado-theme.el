(require 'autothemer)

(autothemer-deftheme
 cortado "A theme for everyone that loves coffee and emacs after dark"

 ((((class color) (min-colors #xFFFFFF))) ;; We're only concerned with graphical Emacs

  ;; Define our color palette
  (cortado-brown    "#A87139")
  (cortado-froth    "#f9ad81")
  (dk-blue          "#0a0b12")
  (purple           "#282828")
  (hallo-black      "#000000")
  (hallo-white      "#ffffff")
  (hallo-orange     "orange1")
  (hallo-dk-orange  "#eb6123")
  (hallo-purple     "MediumPurple2")
  (hallo-dk-purple  "MediumPurple4")
  (hallo-green      "LightGreen"))

 ;; Customize faces
 ((default                   (:foreground hallo-white :background cortado-brown))
  (cursor                    (:background purple))
  (region                    (:background cortado-froth))
  (mode-line                 (:background cortado-froth))
  (font-lock-keyword-face    (:foreground hallo-purple))
  (font-lock-constant-face   (:foreground dk-blue))
  (font-lock-string-face     (:foreground dk-blue))
  (font-lock-builtin-face    (:foreground dk-blue))

  (org-level-1               (:foreground purple))))

(provide-theme 'cortado)
