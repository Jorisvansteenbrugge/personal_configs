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
;;(setq doom-font (font-spec :family "Fira Code" :size 12 :weight 'semi-light)
;;      doom-variable-pitch-font (font-spec :family "Fira Sans" :size 13))

(setq doom-font (font-spec :family "Iosevka" :width 'expanded :size 11.0))
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
(setq org-directory "~/org-roam")


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

;; (use-package! elpy
;;   :commands elpy-enable
;;   :init
;;   (elpy-enable)
;;   (advice-add 'python-mode :before #'elpy-enable)
;;   :config
;;   (setq elpy-shell-starting-directory 'current-directory
;;         elpy-modules '(elpy-module-sane-defaults elpy-module-eldoc))

 ;; (set-company-backend! 'python-mode
  ;;   'elpy-company-backend 'company-yasnippet) 
  ;; (set-lookup-handlers! 'python-mode
  ;;   :definition #'elpy-goto-definition
  ;;   :references #'elpy-rgrep-symbol
  ;;   :documentation #'elpy-doc
  ;;   :async t)

 ;; (map! :map python-mode-map
        ;; [C-return] #'elpy-shell-send-statement-and-step))

(after! which-key
  (setq which-key-idle-delay 0.2))


;;(setq-default TeX-master "master") ; All master files called "master"
(map! "<escape>" #'doom/escape)


(require 'lsp-mode)

;; 1) Zorg dat lsp-mode beschikbaar is
(require 'lsp-mode)

;; 2) Eenvoudige logger om te zien wat er gebeurt
(defun joris/log (fmt &rest args)
  (apply #'message (concat "[nextflow-lsp] " fmt) args))

;; 3) Start LSP in nextflow-mode, maar alleen als hij nog niet draait
(defun joris/nextflow-start-lsp ()
  (joris/log "nextflow-mode-hook fired; lsp-mode active? %s" (bound-and-true-p lsp-mode))
  (unless (bound-and-true-p lsp-mode)
    (joris/log "starting lsp (deferred)…")
    (lsp-deferred)))

;; 4) Hook pas toevoegen zodra nextflow-mode geladen is
(with-eval-after-load 'nextflow-mode
  (joris/log "installing nextflow-mode-hook")
  (add-hook 'nextflow-mode-hook #'joris/nextflow-start-lsp))

(defun lsp-auto-for (mode)
  "Automatically start LSP for a given major MODE when available."
  (require 'lsp-mode)
  (with-eval-after-load mode
    (add-hook (intern (format "%s-hook" mode))
              (lambda ()
                (unless (bound-and-true-p lsp-mode)
                  (lsp-deferred))))))


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

;; Org mode Settings
(defun jvs/org-italicize-scientific-names ()
  "Search and italicize scientific organism names in the current buffer."
  (interactive)
  (save-excursion
    (goto-char (point-min))
    (while (re-search-forward "\\<[A-Z]\\.\s[a-z]+" nil t)
      (add-text-properties (match-beginning 0) (match-end 0) '(face italic)))))

(add-hook 'org-mode-hook 'jvs/org-italicize-scientific-names)
(add-hook 'before-save-hook 'jvs/org-italicize-scientific-names)




(setq org-hide-emphasis-markers t)


;;(font-lock-add-keywords 'org-mode
;;                        '(("^ +\\([-*]\\) "
;;                           (0 (prog1 () (compose-region (match-beginning 1) (match-end 1) "•"))))))



(defun dtm-org-mode-setup-h ()
  "Personal org-mode customisation's after mode startup"

    ;;(setq-local line-spacing dtm-org-line-spacing)
    (electric-quote-local-mode +1)
    (+org-pretty-mode +1)
    (auto-fill-mode +1)
    ;;(+zen-light-toggle +1)
    )


(defun dtm-org-get-title-value ()
  "Returns the value of #+TITLE for the current document"
  (cadar (org-collect-keywords '("TITLE"))))

(defun dtm-insert-exit-fill-paragraph ()
  "Perform `org-fill-paragraph' unless el at point is a src block"
  ;; Check if `auto-fill-mode' is active
  (when auto-fill-function
    (unless (memq (org-element-type (org-element-at-point))
                  '(src-block comment-block))
      (org-fill-paragraph))))



(after! org
  (setq org-ellipsis " ▾"
        org-indent-indentation-per-level 1
        org-list-demote-modify-bullet '(("+" . "-") ("-" . "+") ("*" . "+"))
        org-startup-folded t
        org-return-follows-link t
        org-use-property-inheritance t  ; can cause slowdown when searching
        org-image-actual-width '(800)   ; default if not ATTR is provided
        org-agenda-start-day nil
        org-agenda-span 14
        org-agenda-time-grid '((daily today require-timed)
                               (759 1159 1259 1659)
                               " ┄┄┄┄┄ " "┄┄┄┄┄┄┄┄┄┄┄┄┄┄┄")
        org-agenda-current-time-string "<- NOW ────────"
        ;; org-agenda-files "~/org-roam"
        )

  ;; Make headings bold and larger
  (custom-set-faces!
    '((org-document-title outline-1 outline-2 outline-3 outline-4 outline-5
       outline-6 outline-7 outline-8)
      :weight semi-bold)
    '(org-document-title :height 1.3)
    '(outline-1 :height 1.2)
    '(outline-2 :height 1.1)
    '(outline-3 :height 1.05))

  ;; Give ellipsis same colour as text
  (custom-set-faces!
    '(org-ellipsis :foreground nil :background nil :weight regular)
    '(org-headline-done :strike-through t))




  ;; Enable hard wrapping and automate paragraph filling
  ;; Allow for double quoting using '' and `` (`` -> “)
  (add-hook 'org-mode-hook #'dtm-org-mode-setup-h)
  (add-hook 'org-mode-hook #'org-modern-mode)
  )







;;* Programming Languages
;; General interactive programming buffer settings
(after! comint
  (setq ansi-color-for-comint-mode 'filter
        comint-scroll-to-bottom-on-input t
        comint-scroll-to-bottom-on-output t
        comint-move-point-for-output t)
)

;;(org-roam-db-autosync-mode)
(setq! citar-bibliography '("~/org-roam/zotero_library.bib"))
(after! citar
  (map! :map citar-map
        "C-c b" #'citar-insert-citation
         :map minibuffer-local-map
         "M-b" #'citar-insert-preset
         )
  )

(setq org-ref-bibliography-notes "~/org-roam/notes.org"
      org-ref-default-bibliography '("~/org-roam/zotero_library.bib"))

(defun jvs/org-ref-format-function (key)
  (format "[...]"))

(setq org-ref-hover-bibtex-format-function #'jvs/org-ref-format-function)



(use-package org-roam
  :custom
  (org-roam-directory (file-truename "~/org-roam"))
  (org-roam-dailies-directory "journals/")
  (org-roam-capture-templates
   '(("d" "default" plain
      "%?" :target
      (file+head "pages/${slug}.org" "#+title: ${title}\n")
      :unnarrowed t)))
  )

(after! org-roam

  (defun dtm-org-element-at-point-get-content ()
  "Return the current element's content without properties. Based on `org-mark-element'
and `org-roam-preview-default-function'."
  ;; Move to beginning of item to include children
  (when (org-in-item-p)
    (org-beginning-of-item))
  (let* ((element (org-element-at-point))
         (beg (org-element-property :begin element))
         (end (org-element-property :end element)))
    (string-trim (buffer-substring-no-properties beg end))))

  (setq org-roam-preview-function #'dtm-org-element-at-point-get-content)

  )

(after! ispell
  (setq ispell-dictionary "en_GB"
        ispell-personal-dictionary "~/org-roam/default.aspel.en.pws")
  (delete "--run-together" ispell-extra-args))

(after! spell-fu
  ;; Remove org-block from excluded-faces to enable spell checking in #+CAPTION blocks
  (when-let ((cell (assq 'org-mode +spell-excluded-faces-alist)))
    (setcdr cell (cl-remove 'org-block (cdr cell)))))

;; (defun my-correct-previous-spell-error ()
;;   "Move to the previous spelling error, correct it, and return to the original cursor position."
;;   (interactive)
;;     (save-excursion
;;       (+spell/previous-error)
;;       (while (not (ispell-word))
;;         (ispell-previous-word))))


(defun dtm-straight-prioritize (dir)
  "Move straight package DIR to the front of `load-path'."
  (let ((lib-dir (file-name-concat straight-base-dir "straight"
                                   straight-build-dir dir)))
    (when (file-exists-p lib-dir)
      (setq load-path (cons lib-dir (delete lib-dir load-path))))))

;;;###package org-mode-ox-odt
(after! doom-packages
  ;; Ensure `org-mode-ox-odt' takes precedence over org's ox-odt.el.
  ;; Ref: https://github.com/kjambunathan/org-mode-ox-odt/discussions/133
  (dtm-straight-prioritize "ox-odt")
  (setq org-odt-preferred-output-format "docx"))




;; Use nextflow-mode for Nextflow files and integrate LSP + snippets
;; (use-package! nextflow-mode
  ;; :mode "\\.nf\\'"                             ; open .nf in nextflow-mode
  ;(add-hook 'nextflow-mode-hook #'lsp) ; start LSP on entering nextflow-mode
  ;; (add-hook 'nextflow-mode-hook #'yas-minor-mode) ; enable yasnippet for templates
  ;; :config
  ;; (when (featurep! :tools lsp)
    ;; Optionally adjust LSP settings if needed:
    ;; e.g., specify a custom path or version for the Nextflow LS:
    ;; (setq lsp-nextflow-server-file "/path/to/language-server-all.jar")
    ;; (setq lsp-nextflow-server-file "/Users/jsteenbr/.config/doom/language-server-all.jar")
    ;; (setq lsp-nextflow-server-download-url "<custom-url>")
    ;; )
  ;; Documentation lookup: use Groovy docs for Nextflow, since Nextflow is Groovy-based
  ;; (when (featurep! :tools dash)  ; if using Dash/Zeal integration
    ;; (set-docsets! 'nextflow-mode "Groovy"))
  ;; )

;; (use-package! nextflow-mode
;;   :mode (("\\.nf\\'"             . nextflow-mode)
;;          ("nextflow\\.config\\'" . nextflow-mode))
;;   :hook ((nextflow-mode . yas-minor-mode)
;;          (nextflow-mode . lsp!))
;;   :config
;;   (setq lsp-nextflow-server-file "/Users/joris/.config/doom/language-server-all.jar"))


(use-package! nextflow-mode
  :mode (("\\.nf\\'"             . nextflow-mode)
         ("nextflow\\.config\\'" . nextflow-mode))
  :hook (nextflow-mode . yas-minor-mode)
  :config
  (setq lsp-nextflow-server-file "/Users/joris/.config/doom/language-server-all.jar")
  ;; Doom-conforme manier om LSP te starten zodra buffer volledig klaar is
  (add-hook 'nextflow-mode-local-vars-hook #'lsp!))


;; (lsp-auto-for 'nextflow-mode)
;;
;; Make csv mode only allign the visible region using the keybinding C-c C-C
(add-hook  'csv-mode-hook  (lambda ()    (define-key csv-mode-map (kbd "C-c C-c")      (defun csv-align-visible (&optional arg)        "Align visible fields"        (interactive "P")        (csv-align-fields nil (window-start) (window-end))))))
