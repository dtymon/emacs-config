(setq
 ;; The archives to use
 package-archives
 '(("elpa"       . "https://elpa.gnu.org/packages/")
   ("elpa-devel" . "https://elpa.gnu.org/devel/")
   ("nongnu"     . "https://elpa.nongnu.org/nongnu/")
   ("melpa"      . "https://melpa.org/packages/"))

 ;; Set the priorities
 package-archive-priorities
 '(("elpa"   . 2)
   ("nongnu" . 1))

 )

;; Make sure use-package is installed
(when (not (package-installed-p 'use-package))
  (package-install 'use-package))

;; Make sure blackout is installed to be able to cleanup the minor modes in the
;; modeline.
(use-package blackout
  :ensure t
  )

;; Remove some annoying modes from the modeline
(blackout 'subword-mode)
(blackout 'eldoc-mode)
(blackout 'auto-fill-mode)

(add-hook 'package-menu-mode-hook #'hl-line-mode)

(provide 'setup-package)
