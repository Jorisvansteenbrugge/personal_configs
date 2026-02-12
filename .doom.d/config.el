;;; Config.el --- test                               -*- lexical-binding: t; -*-

;; Copyright (C) 2022  Joris van Steenbrugge

;; Author: Joris van Steenbrugge;;; $DOOMDIR/config.el -*- lexical-binding: t; -*- <jorisvsteenbrugge@protonmail.com>
;; Keywords:

;; Place your private configuration here! Remember, you do not need to run 'doom
;; sync' after modifying this file!

;; Some functionality uses this to identify you, e.g. GPG configuration, email
;; clients, file templates and snippets. It is optional.
(setq user-full-name "Joris van Steenbrugge"
      user-mail-address "jorisvsteenbrugge@protonmail.com")

;; Doom exposes five (optional) variables for controlling fonts in Doom:
;;
;; - `doom-font' -- the primary font to use
;; - `doom-variable-pitch-font' -- a non-monospace font (where applicable)
;; - `doom-big-font' -- used for `doom-big-font-mode'; use this for
;;   presentations or streaming.
;; - `doom-unicode-font' -- for unicode glyphs
;; - `doom-serif-font' -- for the `fixed-pitch-serif' face
;;
;; See 'C-h v doom-font' for documentation and more examples of what they
;; accept. For example:
;;
(setq doom-font (font-spec :family "Fira Code" :size 13 :weight 'regular))


;; (setq doom-font (font-spec :family "Fira Code" :width 'expanded :size 13.0))
;;
;; If you or Emacs can't find your font, use 'M-x describe-font' to look them
;; up, `M-x eval-region' to execute elisp code, and 'M-x doom/reload-font' to
;; refresh your font settings. If Emacs still can't find your font, it likely
;; wasn't installed correctly. Font issues are rarely Doom issues!

;; There are two ways to load a theme. Both assume the theme is installed and
;; available. You can either set `doom-theme' or manually load a theme with the
;; `load-theme' function. This is the default:
(setq doom-theme 'doom-flatwhite)

;; This determines the style of line numbers in effect. If set to `nil', line
;; numbers are disabled. For relative line numbers, set this to `relative'.
(setq display-line-numbers-type t)

;; If you use `org' and don't want your org files in the default location below,
;; change `org-directory'. It must be set before org loads!



;; Whenever you reconfigure a package, make sure to wrap your config in an
;; `after!' block, otherwise Doom's defaults may override your settings. E.g.
;;
;;   (after! PACKAGE
;;     (setq x y))
;;
;; The exceptions to this rule:
;;
;;   - Setting file/directory variables (like `org-directory')
;;   - Setting variables which explicitly tell you to set them before their
;;     package is loaded (see 'C-h v VARIABLE' to look up their documentation).
;;   - Setting doom variables (which start with 'doom-' or '+').
;;
;; Here are some additional functions/macros that will help you configure Doom.
;;
;; - `load!' for loading external *.el files relative to this one
;; - `use-package!' for configuring packages
;; - `after!' for running code after a package has loaded
;; - `add-load-path!' for adding directories to the `load-path', relative to
;;   this file. Emacs searches the `load-path' when you load packages with
;;   `require' or `use-package'.
;; - `map!' for binding new keys
;;
;; To get information about any of these functions/macros, move the cursor over
;; the highlighted symbol at press 'K' (non-evil users must press 'C-c c k').
;; This will open documentation for it, including demos of how they are used.
;; Alternatively, use `C-h o' to look up a symbol (functions, variables, faces,
;; etc).
;;
;; You can also try 'gd' (or 'C-c c d') to jump to their definition and see how
;; they are implemented.

(setq-default fill-column 100)


(after! which-key
  (setq which-key-idle-delay 0.2))


;;(setq-default TeX-master "master") ; All master files called "master"
(map! "<escape>" #'doom/escape)


(use-package! nextflow-mode
  :mode (("\\.nf\\'"             . nextflow-mode)
         ("nextflow\\.config\\'" . nextflow-mode))
  :hook (nextflow-mode . yas-minor-mode)
  :config
  (setq lsp-nextflow-server-file "/Users/jsteenbr/.config/doom/language-server-all.jar")

  ;; Start LSP once, predictably
  (add-hook 'nextflow-mode-hook #'lsp-deferred))




(after! ess

  (defun dtm-ess-modeline-show-busy ()
  "Display spinner if ESS process is busy.
   Ref: `ess--tb-start', https://github.com/seagle0128/doom-modeline/issues/410"
  (setq-local ess-busy-strings (cons "%s" (cdr ess-busy-strings))
              mode-line-process '("["
                                  ess--mode-line-process-indicator
                                  ess--local-mode-line-process-indicator
                                  "]: "
                                  (:eval (nth ess--busy-count ess-busy-strings))
                                  " ")))

  (add-hook! 'ess-r-mode-hook (flycheck-mode -1) )
  (defun joris-insert-rassign ()
    (interactive)
    (insert "<-"))
  (defun joris-insert-pipe ()
    (interactive)
    (insert "%>%"))

  (map! :map ess-r-mode-map
        "C-S-m" #'joris-insert-pipe
        "C-<" #'joris-insert-rassign)
  (map! :map inferior-ess-r-mode-map
        "C-S-m" #'joris-insert-pipe
        "C-<" #'joris-insert-rassign)

   (add-hook! 'inferior-ess-mode-hook
             #'dtm-ess-modeline-show-busy
             (visual-line-mode +1))
)


(after! tex-mode
  (defun joris-insert-itallics ()
    (interactive)
    (insert "\\textit{}"))

  (add-hook! 'LaTeX-mode-hook (auto-fill-mode 1)) ; Enable auto fill mode by default


  (map! :map LaTeX-mode-map
        "C-S-i" #'joris-insert-itallics)
  )


(defun dtm-ibuffer-workspace-filter-groups ()
  "Generate value for `ibuffer-filter-groups' based on perspectives."
  (mapcar #'(lambda (pn) (list pn (cons 'persp pn)))
          (nconc
           (cl-delete persp-nil-name (persp-names-current-frame-fast-ordered)
                      :test 'string=)
           (list persp-nil-name))))




(defun dtm-ibuffer-group-by-workspace-h ()
  "Set the current filter groups to filter by perspective.
Based on `ibuffer-projectile-set-filter-groups' from the ibuffer-projectile package:
https://github.com/purcell/ibuffer-projectile"
  (interactive)
  (setq ibuffer-filter-groups (dtm-ibuffer-workspace-filter-groups))
  (message "persp-ibuffer: grouping buffers by workspace")
  (let ((ibuf (get-buffer "*Ibuffer*")))
    (when ibuf
      (with-current-buffer ibuf
        (pop-to-buffer ibuf)
        (ibuffer-update nil t)))))


(defun dtm-doctor-running-p ()
  "Returns t when the doom doctor CLI is running.
Required because doctor sets `noninteractive' to nil."
  (boundp 'doom-doctor--errors))

(after! ibuffer
  ;; Ref: https://gist.github.com/Bad-ptr/1aca1ec54c3bdb2ee80996eb2b68ad2d#file-persp-mode-ibuffer-groups-el
  (unless (dtm-doctor-running-p)
    (define-ibuffer-filter persp
        "Toggle current view to buffers of current perspective."
      (:description "persp-mode"
       :reader (persp-read-persp nil nil (safe-persp-name (get-frame-persp)) t))
      (cl-find buf (safe-persp-buffers (persp-get-by-name qualifier)))))

 ;; Group buffers based on perspective/workspace
  (add-hook 'ibuffer-hook #'dtm-ibuffer-group-by-workspace-h))

(use-package! good-scroll
  :config
   (good-scroll-mode +1)
  (defun dtm/good-scroll-down-half ()
    (interactive)
    (good-scroll-move (/ (good-scroll--window-usable-height) 2)))


  (defun dtm/good-scroll-up-half ()
    (interactive)
    (good-scroll-move (/ (good-scroll--window-usable-height) -2)))
  (map!
        "C-v" #'dtm/good-scroll-down-half
        "M-v" #'dtm/good-scroll-up-half)

  (setq! good-scroll-duration .25
         good-scroll-algorithm 'good-scroll-linear
         good-scroll-step (round (/ (display-pixel-height) 15)))

  )



;;* Programming Languages
;; General interactive programming buffer settings
(after! comint
  (setq ansi-color-for-comint-mode 'filter
        comint-scroll-to-bottom-on-input t
        comint-scroll-to-bottom-on-output t
        comint-move-point-for-output t)
)





(after! ispell
  (setq ispell-dictionary "en_GB"
        ispell-personal-dictionary "~/org-roam/default.aspel.en.pws")
  (delete "--run-together" ispell-extra-args))



(defun dtm-straight-prioritize (dir)
  "Move straight package DIR to the front of `load-path'."
  (let ((lib-dir (file-name-concat straight-base-dir "straight"
                                   straight-build-dir dir)))
    (when (file-exists-p lib-dir)
      (setq load-path (cons lib-dir (delete lib-dir load-path))))))







;; Make csv mode only allign the visible region using the keybinding C-c C-C
(add-hook  'csv-mode-hook  (lambda ()    (define-key csv-mode-map (kbd "C-c C-c")      (defun csv-align-visible (&optional arg)        "Align visible fields"        (interactive "P")        (csv-align-fields nil (window-start) (window-end))))))


(defun joris/ibuffer-overview-all ()
  (interactive)
  (require 'ibuffer)
  (ibuffer nil "*Ibuffer*")
  (with-current-buffer "*Ibuffer*"
    (dtm-ibuffer-group-by-workspace-h)
    (ibuffer-update nil t)))

(defun joris/ibuffer-overview-here ()
  (interactive)
  (require 'ibuffer)
  (ibuffer nil "*Ibuffer*")
  (with-current-buffer "*Ibuffer*"
    (dtm-ibuffer-group-by-workspace-h)
    (ibuffer-filter-by-persp (safe-persp-name (get-frame-persp)))
    (ibuffer-update nil t)))


(defun joris/vterm--run (name cmd)
  "Open or reuse vterm buffer NAME and run CMD."
  (require 'vterm)
  (let* ((bufname (format "*vterm:%s*" name))
         (buf (get-buffer bufname)))
    (if (buffer-live-p buf)
        (pop-to-buffer buf)
      (vterm bufname))
    (when (and cmd (not (string-empty-p cmd)))
      (vterm-send-string cmd)
      (vterm-send-return))))

(defun joris/vterm-htop ()
  "Open/reuse a vterm and run htop."
  (interactive)
  (joris/vterm--run "htop" "htop"))

(defun joris/vterm-mequeue ()
  "Open/reuse a vterm and run htop."
  (interactive)
  (joris/vterm--run "ssh" "ssh hpc -T 'mequeue'"))


(defvar joris/overview-map (make-sparse-keymap)
  "Joris overview prefix map.")
(define-key global-map (kbd "C-c b") joris/overview-map)

(define-key joris/overview-map (kbd "b") #'joris/ibuffer-overview-here)
(define-key joris/overview-map (kbd "B") #'joris/ibuffer-overview-all)
(define-key joris/overview-map (kbd "h") #'joris/vterm-htop)
(define-key joris/overview-map (kbd "q") #'joris/vterm-mequeue)

(after! which-key
  (which-key-add-key-based-replacements
    "C-c b" "Joris Overview"))
