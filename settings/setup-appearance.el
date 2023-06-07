(defgroup dtymon-appearance nil
  "Variables used to set the appearance of Emacs."
  :group 'tools)

;;(defcustom dtymon::theme 'solarized-dtymon
;;(defcustom dtymon::theme 'ef-night
(defcustom dtymon::theme 'modus-vivendi
  "The default theme to use."
  :group 'dtymon-appearance
  :type 'symbol)

(defcustom dtymon::font-family "Ubuntu Mono"
  "The default font family to use."
  :group 'dtymon-appearance
  :type 'string)

(defcustom dtymon::font-size 180
  "The default font size to use."
  :group 'dtymon-appearance
  :type 'number)

(defcustom dtymon::presentation-font-size 200
  "The default font size to use in presentation mode."
  :group 'dtymon-appearance
  :type 'number)

(defcustom dtymon::frame-width-factor 0.9
  "The percentage of monitor width for frames."
  :group 'dtymon-appearance
  :type 'float)

(defcustom dtymon::frame-height-factor 0.85
  "The percentage of monitor height for frames."
  :group 'dtymon-appearance
  :type 'float)

(defcustom dtymon::reposition-initial-frame t
  "If true, then reposition the initial frame."
  :group 'dtymon-appearance
  :type 'number)

(defcustom dtymon::frame-top 0
  "The vertical positioning of the frame's geometry."
  :group 'dtymon-appearance
  :type 'number)

(defcustom dtymon::frame-left -1
  "The horizontal positioning of the frame's geometry."
  :group 'dtymon-appearance
  :type 'number)

(when window-system
  ;; Set some basic appearance configuration
  (setq
   frame-title-format          "%b"
   focus-follows-mouse         'auto-raise
   mouse-autoselect-window     nil

   ;; Allow pasting selection outside of Emacs
   select-enable-clipboard t

   ;; When pasting with the mouse, paste at the current point not at the mouse
   ;; location.
   mouse-yank-at-point t
   )

  ;; Change the default font for Korean to get a TTF font instead of a bitmap
  (set-fontset-font t 'hangul (font-spec :name "NanumGothic"))
  (set-fontset-font t 'cyrillic (font-spec :name "Droid Sans Mono"))

  ;; This turns off bi-directional stuff but is supposed to make handling of
  ;; things like long lines faster.
  (setq
   bidi-inhibit-bpa         t
   bidi-paragraph-direction 'left-to-right
   )
  (setq-default
   bidi-inhibit-bpa t
   bidi-paragraph-direction 'left-to-right
   )

  ;; Set the default font
  (set-face-attribute 'default nil :family dtymon::font-family :height dtymon::font-size)
  (set-face-attribute 'fixed-pitch nil :family dtymon::font-family :height dtymon::font-size)
  (set-face-attribute 'fixed-pitch-serif nil :family dtymon::font-family :height dtymon::font-size)

  ;; Modus vivendi overrides
  (add-to-list 'load-path (expand-file-name "modus-themes" site-lisp-dir))
  (require 'modus-themes)
  (setq modus-themes-common-palette-overrides
        '(
          ;; (fg-main "#e0e0e0")
          (bg-main "#090c21")
          (comment "#507090")
          )
        )

  ;; Load the theme
  (load-theme dtymon::theme :no-confirm)

  ;; Set a red solid non-blinking cursor
  (set-face-background 'cursor "red")
  (blink-cursor-mode -1)

  ;; Set the fill colour indicator background
  (set-face-foreground 'fill-column-indicator "#884444")
  (set-face-background 'fill-column-indicator "#884444")

  (dolist (var '(default-frame-alist initial-frame-alist))
    ;; Set the desired locations for frames
    (when dtymon::reposition-initial-frame
      (add-to-list var (cons 'top dtymon::frame-top))
      (add-to-list var (cons 'left dtymon::frame-left))
      )

    ;; Set the dimensions of frames based on the factor configured
    (let ((initial-frame-width (round (* dtymon::frame-width-factor (dtymon::monitor-width))))
          (initial-frame-height (round (* dtymon::frame-height-factor (dtymon::monitor-height)))))
      (add-to-list var (cons 'width (cons 'text-pixels initial-frame-width)))
      (add-to-list var (cons 'height (cons 'text-pixels initial-frame-height)))
      )

    ;; Set the cursor colour in all frames. This doesn't seem to work for some
    ;; reason so setting it via the hook below.
    (add-to-list var (cons 'cursor-color "red"))
    )

  ;; This seems to be the only way to get a red cursor in new frames
  (defun dtymon::change-cursor-colour (frame)
    (set-frame-parameter frame 'cursor-color "red")
    )
  (add-hook 'after-make-frame-functions #'dtymon::change-cursor-colour)
)

;; Highlight current line in text and programming mode
(add-hook 'prog-mode-hook #'hl-line-mode)
(add-hook 'text-mode-hook #'hl-line-mode)
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
(fringe-mode '(16 . 0))

;; Some modes don't seem to play well with diminish
(defmacro dtymon::rename-modeline (package-name mode new-name)
  `(eval-after-load ,package-name
     '(defadvice ,mode (after rename-modeline activate)
        (setq mode-name ,new-name))))

(dtymon::rename-modeline "js2-mode" js2-mode "JS2")
(dtymon::rename-modeline "typescript-mode" typescript-mode "TS")

(provide 'setup-appearance)

;;; setup-appearance.el ends here
