(setq visible-bell nil
      font-lock-maximum-decoration t
      color-theme-is-global t
      truncate-partial-width-windows nil)

;; Highlight current line
(global-hl-line-mode 1)

;; Set custom theme path
(setq custom-theme-directory (concat user-emacs-directory "themes"))

(dolist
    (path (directory-files custom-theme-directory t "\\w+"))
  (when (file-directory-p path)
    (add-to-list 'custom-theme-load-path path)))

;; Default theme
(defun use-presentation-theme ()
  (interactive)
  (when (boundp 'magnars/presentation-font)
    (set-face-attribute 'default nil :font magnars/presentation-font)))

(defun use-default-theme ()
  (interactive)
;;  (load-theme 'default-black)
;;  (load-theme 'default-davidt)
;;  (load-theme 'solarized-dark)
;;  (load-theme 'solarized-dark-high-contrast)
  (load-theme 'solarized-dtymon)
;;  (set-face-attribute 'default nil :background "#090c21")
;;  (load-theme 'doom-moonlight-davidt t)
;;  (load-theme 'doom-davidt t)
  (when (boundp 'magnars/default-font)
    (set-face-attribute 'default nil :font magnars/default-font)))

(defun toggle-presentation-mode ()
  (interactive)
  (if (string= (frame-parameter nil 'font) magnars/default-font)
      (use-presentation-theme)
    (use-default-theme)))

(global-set-key (kbd "C-<f9>") 'toggle-presentation-mode)

(use-default-theme)

;; Not a big fan of overusing bold fonts
;;(set-face-attribute 'font-lock-keyword-face nil :weight 'normal :foreground "#74adf5")
;;(set-face-attribute 'font-lock-keyword-face nil :weight 'normal :foreground "#206cb2")
(set-face-attribute 'font-lock-keyword-face nil :weight 'normal)
;;(set-face-attribute 'font-lock-keyword-face nil :foreground "#806080")
(set-face-attribute 'font-lock-builtin-face nil :weight 'normal)
(set-face-attribute 'font-lock-constant-face nil :weight 'normal)
;;(set-face-attribute 'default nil :background "#161821")

;; Don't defer screen updates when performing operations
(setq redisplay-dont-pause t)

;; org-mode colors
;; (setq org-todo-keyword-faces
;;       '(
;;         ("INPR" . (:foreground "yellow" :weight bold))
;;         ("DONE" . (:foreground "green" :weight bold))
;;         ("IMPEDED" . (:foreground "red" :weight bold))
;;         ))

;; Highlight matching parentheses when the point is on them.
(show-paren-mode 1)

(when window-system
  (setq frame-title-format '(buffer-file-name "%f" ("%b")))
  (tooltip-mode -1)
  (blink-cursor-mode -1))

;; Make zooming affect frame instead of buffers
;;(require 'zoom-frm)

;; Unclutter the modeline
(require 'diminish)
(eval-after-load "yasnippet" '(diminish 'yas-minor-mode))
(eval-after-load "eldoc" '(diminish 'eldoc-mode))
(eval-after-load "paredit" '(diminish 'paredit-mode))
(eval-after-load "tagedit" '(diminish 'tagedit-mode))
(eval-after-load "elisp-slime-nav" '(diminish 'elisp-slime-nav-mode))
(eval-after-load "skewer-mode" '(diminish 'skewer-mode))
(eval-after-load "skewer-css" '(diminish 'skewer-css-mode))
(eval-after-load "skewer-html" '(diminish 'skewer-html-mode))
(eval-after-load "smartparens" '(diminish 'smartparens-mode))
(eval-after-load "guide-key" '(diminish 'guide-key-mode))
(eval-after-load "whitespace-cleanup-mode" '(diminish 'whitespace-cleanup-mode))
(eval-after-load "subword" '(diminish 'subword-mode))

(defmacro rename-modeline (package-name mode new-name)
  `(eval-after-load ,package-name
     '(defadvice ,mode (after rename-modeline activate)
        (setq mode-name ,new-name))))

(rename-modeline "js2-mode" js2-mode "JS2")

;; This is really important otherwise the right-aligned messages displayed by
;; lsp-ui-sideline cause a line wrap.
;;
;; https://github.com/emacs-lsp/lsp-ui/issues/285
(fringe-mode '(12 . 0))

(provide 'appearance)
