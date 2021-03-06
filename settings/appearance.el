(defgroup dtymon-appearance nil
  "Variables used to set the appearance of Emacs"
  :group 'tools)

(defcustom dtymon::theme 'solarized-dtymon
  "The default theme to use"
  :group 'dtymon-appearance
  :type 'symbol)

(defcustom dtymon::font-family "Ubuntu Mono"
  "The default font family to use"
  :group 'dtymon-appearance
  :type 'string)

(defcustom dtymon::font-size 180
  "The default font size to use"
  :group 'dtymon-appearance
  :type 'number)

(defcustom dtymon::presentation-font-size 200
  "The default font size to use in presentation mode"
  :group 'dtymon-appearance
  :type 'number)

(defcustom dtymon::frame-width (* 0.9 dtymon::monitor-width)
  "The width of frames"
  :group 'dtymon-appearance
  :type 'float)

(defcustom dtymon::frame-height (* 0.85 dtymon::monitor-height)
  "The height of frames"
  :group 'dtymon-appearance
  :type 'float)

(defcustom dtymon::reposition-initial-frame t
  "If true, then reposition the initial frame"
  :group 'dtymon-appearance
  :type 'number)

(defcustom dtymon::frame-top 0
  "The vertical positioning of the frame's geometry"
  :group 'dtymon-appearance
  :type 'number)

(defcustom dtymon::frame-left -1
  "The horizontal positioning of the frame's geometry"
  :group 'dtymon-appearance
  :type 'number)

(setq
 visible-bell nil
 font-lock-maximum-decoration t
 color-theme-is-global t
 truncate-partial-width-windows nil
 inhibit-startup-screen t
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
;;   dtymon::theme                  'solarized-dtymon
;;   dtymon::font-family            "Fira Code"    ;; Use s-t to help choose fonts
;;   dtymon::font-size              150
;;   dtymon::font-family            "Ubuntu Mono"
;;   dtymon::font-size              (if (boundp 'dtymon::override-font-size) dtymon::override-font-size 190)
;;   dtymon::font-family            "Hack"
;;   dtymon::font-size              150
;;   dtymon::presentation-font-size 200
   dtymon::in-presentation        nil

   frame-title-format '(buffer-file-name "%f" ("%b"))
  )

  (add-to-list 'initial-frame-alist (cons 'width (cons 'text-pixels (round dtymon::frame-width))))
  (add-to-list 'initial-frame-alist (cons 'height (cons 'text-pixels (round dtymon::frame-height))))

  ;; Move the initial frame to its desired location
  (when dtymon::reposition-initial-frame
    (add-to-list 'initial-frame-alist (cons 'top dtymon::frame-top))
    (add-to-list 'initial-frame-alist (cons 'left dtymon::frame-left))
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
(setq
 ;; Do not shrink the width of the line numbers column as its really distracting
 ;; and compute the width required for line numbers upfront.
 display-line-numbers-grow-only t
 display-line-numbers-width-start t
 )

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

;; Some modes don't seem to play well with diminish
(defmacro dtymon::rename-modeline (package-name mode new-name)
  `(eval-after-load ,package-name
     '(defadvice ,mode (after rename-modeline activate)
        (setq mode-name ,new-name))))

(dtymon::rename-modeline "js2-mode" js2-mode "JS2")
(dtymon::rename-modeline "typescript-mode" typescript-mode "TS")

(provide 'appearance)
