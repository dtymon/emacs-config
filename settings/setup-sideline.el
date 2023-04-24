;; Taken from Protesilaos' prot-sideline.el found here:
;; https://git.sr.ht/~protesilaos/dotfiles/tree/master/item/emacs/.emacs.d/prot-lisp/prot-sideline.el
;;
(require 'display-line-numbers)
(require 'hl-line)
(require 'whitespace)

(define-minor-mode prot-sideline-mode
  "Buffer-local wrapper mode for presentations."
  :init-value nil
  :global nil)

(defun prot-sideline--numbers-toggle ()
  "Toggle line numbers."
  (if (or (bound-and-true-p display-line-numbers-mode)
          (not (bound-and-true-p prot-sideline-mode)))
      (display-line-numbers-mode -1)
    (display-line-numbers-mode 1)))

(add-hook 'prot-sideline-mode-hook #'prot-sideline--numbers-toggle)

(defun prot-sideline--hl-line-toggle ()
  "Toggle line highlight."
  (if (or (bound-and-true-p hl-line-mode)
          (not (bound-and-true-p prot-sideline-mode)))
      (hl-line-mode -1)
    (hl-line-mode 1)))

(add-hook 'prot-sideline-mode-hook #'prot-sideline--hl-line-toggle)

;; We keep this separate, as I do not want it bundled up together with
;; the rest of the functionality included here.
;;;###autoload
(defun prot-sideline-negative-space-toggle ()
  "Toggle the display of indentation and space characters."
  (interactive)
  (if (bound-and-true-p whitespace-mode)
      (whitespace-mode -1)
    (whitespace-mode)))


(setq display-line-numbers-type t)
(setq display-line-numbers-major-tick 0)
(setq display-line-numbers-minor-tick 0)

;; Use absolute numbers in narrowed buffers
(setq-default display-line-numbers-widen t)

(setq hl-line-sticky-flag nil)
(setq hl-line-overlay-priority -50)

(provide 'setup-sideline)
