(require 'vc-annotate)

(defun vc-annotate-quit ()
  "Restores the previous window configuration and kills the vc-annotate buffer"
  (interactive)
  (kill-buffer)
  (jump-to-register :vc-annotate-fullscreen))

;;(use-package git-commit
;;  :ensure t
;;  )

(use-package magit
  :ensure t
  :init

  (let ((map global-map))
    (define-key map (kbd "C-c g") #'magit-dispatch)
    (define-key map (kbd "C-c f") #'magit-file-dispatch)
    (define-key map (kbd "C-x m") #'magit-status)
    (define-key map (kbd "C-x M") (lambda ()
                                    (interactive)
                                    (other-frame-prefix)
                                    (magit-status)))

    (define-key map (kbd "C-c C-m l") #'magit-log-buffer-file)
    (define-key map (kbd "C-c C-m b") #'magit-blame-addition)
    (define-key map (kbd "C-c C-m d") #'magit-diff)
    (define-key map (kbd "C-c C-m s") #'magit-stash)
    )

  (setq magit-display-buffer-function #'magit-display-buffer-fullframe-status-v1
        magit-bury-buffer-function #'magit-restore-window-configuration
        magit-auto-revert-mode nil
        magit-define-global-key-bindings 'recommended
        )

  :config
  (let ((map magit-status-mode-map))
    (define-key map (kbd "S-<prior>") #'magit-section-up)
    )
)

;;
;;  (add-hook 'git-commit-mode-hook (lambda ()
;;                                    (beginning-of-buffer)
;;                                    (when (looking-at "#")
;;                                      (forward-line 2))))
;;
;;  (defadvice vc-annotate (around fullscreen activate)
;;    (window-configuration-to-register :vc-annotate-fullscreen)
;;    ad-do-it
;;    (delete-other-windows))
;;
;;  (define-key vc-annotate-mode-map
;;    (kbd "q") `vc-annotate-quit)
;;  )

;; Bring in libgit to make magit faster
;; (use-package libgit
;;   :ensure t
;;   )
;;
;; (use-package magit-libgit
;;   :ensure t
;;   :after (magit libgit)
;;   )

;; (use-package difftastic
;;   :ensure t
;;   :defer t
;;   :after magit-diff
;;   :config
;;   (transient-append-suffix 'magit-diff '(-1 -1)
;;        [("D" "Difftastic diff (dwim)" difftastic-magit-diff)
;;         ("S" "Difftastic show" difftastic-magit-show)]))

(provide 'setup-magit)
