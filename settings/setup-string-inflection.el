;; -*- lexical-binding: t -*-

(use-package string-inflection
  :ensure t
  :config
  (let ((map global-map))
    (define-key map (kbd "C-c i") (lambda ()
                                    (interactive)
                                    (save-excursion
                                      (string-inflection-cycle))
                                    ))
    )
  )

(provide 'setup-string-inflection)
