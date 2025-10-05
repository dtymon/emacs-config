;; -*- lexical-binding: t -*-

(use-package all-the-icons
  :ensure t
  :if (display-graphic-p)
  )

;; I think I'm doing something wrong here because the icons shown when
;; completing are not as pretty as those shown in say dired.
;; (use-package all-the-icons-completion
;;   :ensure t
;;   :if (display-graphic-p)
;;   :config
;;   (all-the-icons-completion-mode)
;;   )

(provide 'setup-all-the-icons)
