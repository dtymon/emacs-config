(setq
 visible-bell nil
 font-lock-maximum-decoration t
 color-theme-is-global t
 truncate-partial-width-windows nil
 inhibit-startup-screen t

 ;; Don't defer screen updates when performing operations
 redisplay-dont-pause t
 )

;; Define the fonts to use
(when window-system
  ;; Load the available custom themes
  (setq custom-theme-directory (concat user-emacs-directory "themes"))
  (dolist
      (path (directory-files custom-theme-directory t "\\w+"))
    (when (file-directory-p path)
      (add-to-list 'custom-theme-load-path path)))

  ;; Set some basic appearance configuration
  (setq
   dtymon::theme                  'solarized-dtymon
   dtymon::font-family            "Fira Code"    ;; Use s-t to help choose fonts
   dtymon::font-size              150
   dtymon::presentation-font-size 180
   dtymon::in-presentation        nil

   frame-title-format '(buffer-file-name "%f" ("%b"))
  )

  ;; Set the default font and load the theme
  (set-face-attribute 'default nil :family dtymon::font-family :height dtymon::font-size)
  (load-theme dtymon::theme)

  ;; Not a big fan of overusing bold fonts
  ;;(set-face-attribute 'font-lock-keyword-face nil :weight 'normal :foreground "#74adf5")
  ;;(set-face-attribute 'font-lock-keyword-face nil :weight 'normal :foreground "#206cb2")
  (set-face-attribute 'font-lock-keyword-face nil :weight 'normal)
  ;;(set-face-attribute 'font-lock-keyword-face nil :foreground "#806080")
  (set-face-attribute 'font-lock-builtin-face nil :weight 'normal)
  (set-face-attribute 'font-lock-constant-face nil :weight 'normal)
  ;;(set-face-attribute 'default nil :background "#161821")

  ;; Just say no to tooltips and blinking cursors
  (tooltip-mode -1)
  (blink-cursor-mode -1)

  ;; Bind a key to toggle presentation mode
  (defun dtymon::toggle-presentation-mode ()
    (interactive)
    (cond (dtymon::in-presentation
           (set-face-attribute 'default nil :family dtymon::font-family :height dtymon::font-size))
          (t
           (set-face-attribute 'default nil :family dtymon::font-family :height dtymon::presentation-font-size)))
    (setq dtymon::in-presentation (not dtymon::in-presentation))
    )

  (global-set-key (kbd "C-<f9>") 'dtymon::toggle-presentation-mode)
)

;; Highlight current line in text and programming mode
(add-hook 'prog-mode-hook #'hl-line-mode)
(add-hook 'text-mode-hook #'hl-line-mode)
;;(global-hl-line-mode 1)

;; Always show line numbers
(global-display-line-numbers-mode)

;; Highlight matching parentheses when the point is on them.
(show-paren-mode 1)

;; This is really important otherwise the right-aligned messages displayed by
;; lsp-ui-sideline cause a line wrap.
;;
;; https://github.com/emacs-lsp/lsp-ui/issues/285
(fringe-mode '(12 . 0))


;; (defun use-default-theme ()
;;   (interactive)
;; ;;  (load-theme 'default-black)
;; ;;  (load-theme 'default-davidt)
;; ;;  (load-theme 'solarized-dark)
;; ;;  (load-theme 'solarized-dark-high-contrast)
;;   (load-theme 'solarized-dtymon)
;; ;;  (set-face-attribute 'default nil :background "#090c21")
;; ;;  (load-theme 'doom-moonlight-davidt t)
;; ;;  (load-theme 'doom-davidt t)
;;

;; Unclutter the modeline
;;(require 'diminish)
;;(eval-after-load "yasnippet" '(diminish 'yas-minor-mode))
;;(eval-after-load "eldoc" '(diminish 'eldoc-mode))
;;(eval-after-load "paredit" '(diminish 'paredit-mode))
;;(eval-after-load "tagedit" '(diminish 'tagedit-mode))
;;(eval-after-load "elisp-slime-nav" '(diminish 'elisp-slime-nav-mode))
;;(eval-after-load "skewer-mode" '(diminish 'skewer-mode))
;;(eval-after-load "skewer-css" '(diminish 'skewer-css-mode))
;;(eval-after-load "skewer-html" '(diminish 'skewer-html-mode))
;;(eval-after-load "smartparens" '(diminish 'smartparens-mode))
;;(eval-after-load "guide-key" '(diminish 'guide-key-mode))
;;(eval-after-load "whitespace-cleanup-mode" '(diminish 'whitespace-cleanup-mode))
;;(eval-after-load "subword" '(diminish 'subword-mode))

;; Some modes don't seem to play well with diminish
(defmacro dtymon::rename-modeline (package-name mode new-name)
  `(eval-after-load ,package-name
     '(defadvice ,mode (after rename-modeline activate)
        (setq mode-name ,new-name))))

(dtymon::rename-modeline "js2-mode" js2-mode "JS2")
(dtymon::rename-modeline "typescript-mode" typescript-mode "TS")

(provide 'appearance)
