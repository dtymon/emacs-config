(defface dtymon::mode-line-intense
  '((((class color) (min-colors 88) (background light))
     :background "gray40" :foreground "white")
    (((class color) (min-colors 88) (background dark))
     :background "gray70" :foreground "black")
    (t :inverse-video t))
  "Face for intense mode line constructs.")

(setq mode-line-defining-kbd-macro "")
;; (setq mode-line-defining-kbd-macro
;;       (propertize " ▶ " 'face 'dtymon::mode-line-intense))

(defvar dtymon::mode-line-kbd-macro
  '(:eval (when (and defining-kbd-macro (mode-line-window-selected-p))
            (propertize " ▶ " 'face 'dtymon::mode-line-intense))))

(defvar dtymon::mode-line-flymake
  '(:eval (if (bound-and-true-p flymake-mode)
            flymake-mode-line-format)))

(defvar dtymon::mode-line-align-right
  '(:eval (propertize
           " " 'display
           `((space :align-to
                    (- (+ right right-fringe right-margin)
                       ,(string-width
                          (format-mode-line mode-line-misc-info))))))))

(defvar dtymon::mode-line-misc-info
  '(:eval
    (when (mode-line-window-selected-p)
      mode-line-misc-info)))

(dolist (construct '(dtymon::mode-line-align-right
                     dtymon::mode-line-kbd-macro
                     dtymon::mode-line-flymake
                     dtymon::mode-line-misc-info))
  (put construct 'risky-local-variable t))

(setq
 mode-line-compact                     nil
 mode-line-percent-position            '(-3 "%p")
 mode-line-position-column-line-format '("%l,%c")
 )

(setq-default mode-line-format
              '("%e"
                dtymon::mode-line-kbd-macro
                " "
                mode-line-mule-info
                mode-line-modified
                mode-line-remote
                " "
                mode-line-frame-identification
                mode-line-buffer-identification
                "  "
                mode-line-position
                (vc-mode vc-mode)
                " "
                mode-line-modes
                dtymon::mode-line-flymake
                " "
                " "
                dtymon::mode-line-align-right
                dtymon::mode-line-misc-info)
              )

(provide 'setup-modeline)
