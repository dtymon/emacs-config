(global-set-key (kbd "C-x C-c") (lambda ()
                                  (interactive)
                                  (if (y-or-n-p "Do you really want to exit emacs ? ")
                                      (save-buffers-kill-emacs))))

;; Avoid exiting abruptly if I accidentally hit s-q instead of M-q
(global-set-key (kbd "s-q") (lambda ()
                              (interactive)
                              (fill-comment-paragraph)))

;; Font scaling
(global-set-key (kbd "s-+") 'text-scale-increase)
(global-set-key (kbd "s--") 'text-scale-decrease)
(global-set-key (kbd "s-0") (lambda ()
                              (interactive)
                              (text-scale-adjust 0)))

(require 'misc)
(global-set-key (kbd "s-.") 'copy-from-above-command)

;; Experimental multiple-cursors
(global-set-key (kbd "C-S-c C-S-c") 'mc/edit-lines)
(global-set-key (kbd "C-S-c C-e") 'mc/edit-ends-of-lines)
(global-set-key (kbd "C-S-c C-a") 'mc/edit-beginnings-of-lines)

;; Mark additional regions matching current region
;;(global-set-key (kbd "M-æ") 'mc/mark-all-dwim)
;;(global-set-key (kbd "C-å") 'mc/mark-previous-like-this)
;;(global-set-key (kbd "C-æ") 'mc/mark-next-like-this)
;;(global-set-key (kbd "C-Æ") 'mc/mark-more-like-this-extended)
;;(global-set-key (kbd "M-å") 'mc/mark-all-in-region)
;;
;;;; Symbol and word specific mark-more
;;(global-set-key (kbd "s-æ") 'mc/mark-next-word-like-this)
;;(global-set-key (kbd "s-å") 'mc/mark-previous-word-like-this)
;;(global-set-key (kbd "M-s-æ") 'mc/mark-all-words-like-this)
;;(global-set-key (kbd "s-Æ") 'mc/mark-next-symbol-like-this)
;;(global-set-key (kbd "s-Å") 'mc/mark-previous-symbol-like-this)
;;(global-set-key (kbd "M-s-Æ") 'mc/mark-all-symbols-like-this)

;; Extra multiple cursors stuff
(global-set-key (kbd "C-~") 'mc/reverse-regions)
(global-set-key (kbd "M-~") 'mc/sort-regions)
(global-set-key (kbd "H-~") 'mc/insert-numbers)

(global-set-key (kbd "C-S-<mouse-1>") 'mc/add-cursor-on-click)

;; Set anchor to start rectangular-region-mode
(global-set-key (kbd "H-SPC") 'set-rectangular-region-anchor)

;; Replace rectangle-text with inline-string-rectangle
;; (global-set-key (kbd "C-x r t") 'inline-string-rectangle)

;; Perform general cleanup.
(global-set-key (kbd "C-c C-<return>") 'delete-blank-lines)

;; M-i for back-to-indentation
(global-set-key (kbd "M-i") 'back-to-indentation)

;; Turn on the menu bar for exploring new modes
(global-set-key (kbd "C-<f10>") 'menu-bar-mode)

;; Use shell-like backspace C-h, rebind help to F1
;; (define-key key-translation-map [?\C-h] [?\C-?])
;; (global-set-key (kbd "<f1>") 'help-command)

(global-set-key (kbd "M-h") 'kill-region-or-backward-word)

;; Transpose stuff with M-t
(global-unset-key (kbd "M-t")) ;; which used to be transpose-words
(global-set-key (kbd "M-t l") 'transpose-lines)
(global-set-key (kbd "M-t w") 'transpose-words)
(global-set-key (kbd "M-t s") 'transpose-sexps)

;; Interactive selective display
(global-set-key (kbd "C-x $") 'inc-selective-display)

;; Change next underscore with a camel case
(global-set-key (kbd "C-c C--") 'replace-next-underscore-with-camel)
(global-set-key (kbd "M-s M--") 'snakeify-current-word)

;; Change word separators
(global-unset-key (kbd "C-x +")) ;; used to be balance-windows
(global-set-key (kbd "C-x + -") (lambda (replace-region-by 's-dashed-words)))
(global-set-key (kbd "C-x + _") (lambda (replace-region-by 's-snake-case)))
(global-set-key (kbd "C-x + c") (lambda (replace-region-by 's-lower-camel-case)))
(global-set-key (kbd "C-x + C") (lambda (replace-region-by 's-upper-camel-case)))

;; Killing text
(global-set-key (kbd "C-S-k") 'kill-and-retry-line)
;; (global-set-key (kbd "C-w") 'kill-region-or-backward-word)
(global-set-key (kbd "C-c C-w") 'kill-to-beginning-of-line)

;; Use M-w for copy-line if no active region
;; (global-set-key (kbd "M-w") 'save-region-or-current-line)
;; (global-set-key (kbd "s-w") 'save-region-or-current-line)
(global-set-key (kbd "M-W") (lambda (save-region-or-current-line 1)))

;; Make shell more convenient, and suspend-frame less
;; (global-set-key (kbd "C-z") 'shell)
;; (global-set-key (kbd "C-x M-z") 'suspend-frame)

;; Zap to char
(global-set-key (kbd "M-z") 'zap-up-to-char)
(global-set-key (kbd "s-z") (lambda (char) (interactive "cZap up to char backwards: ") (zap-up-to-char -1 char)))

(global-set-key (kbd "M-Z") (lambda (char) (interactive "cZap to char: ") (zap-to-char 1 char)))
(global-set-key (kbd "s-Z") (lambda (char) (interactive "cZap to char backwards: ") (zap-to-char -1 char)))

;; Create new frame
;;(define-key global-map (kbd "C-x C-n") 'make-frame-command)

;; Jump to a definition in the current file. (This is awesome)
(global-set-key (kbd "C-x C-i") 'ido-imenu)

;; File finding
(global-set-key (kbd "C-x M-f") 'ido-find-file-other-window)
;; (global-set-key (kbd "C-x f") 'recentf-ido-find-file)
(global-set-key (kbd "C-c y") 'bury-buffer)
(global-set-key (kbd "C-c r") 'revert-buffer)
(global-set-key (kbd "M-`") 'file-cache-minibuffer-complete)
(global-set-key (kbd "C-x C-b") 'ibuffer)

;; toggle two most recent buffers
(fset 'quick-switch-buffer [?\C-x ?b return])
(global-set-key (kbd "s-b") 'quick-switch-buffer)

(global-set-key (kbd "s-y") 'bury-buffer)

;; Revert without any fuss
;; (global-set-key (kbd "M-<escape>") (lambda (revert-buffer t t)))

;; Window switching
(global-set-key (kbd "C-x C-<left>") 'rotate-frame-clockwise)
(global-unset-key (kbd "C-x C-+")) ;; don't zoom like this

;; Add region to *multifile*
(global-set-key (kbd "C-!") 'mf/mirror-region-in-multifile)

;; Indentation help
(global-set-key (kbd "M-j") (lambda (join-line -1)))

;; Help should search more than just commands
(global-set-key (kbd "<f1> a") 'apropos)

;; Should be able to eval-and-replace anywhere.
(global-set-key (kbd "C-c C-e") 'eval-and-replace)
(global-set-key (kbd "M-s-e") 'eval-and-replace)

;;(global-set-key (kbd "<prior>") 'beginning-of-buffer)
;;(global-set-key (kbd "<next>") 'end-of-buffer)
(global-set-key (kbd "<home>") 'beginning-of-line)
(global-set-key (kbd "<end>") 'end-of-line)
(global-set-key (kbd "M-p") 'backward-paragraph)
(global-set-key (kbd "M-n") 'forward-paragraph)

;; Webjump let's you quickly search google, wikipedia, emacs wiki
;;(global-set-key (kbd "C-x g") 'webjump)
;;(global-set-key (kbd "C-x M-g") 'browse-url-at-point)

;; Completion at point
(global-set-key (kbd "C-<tab>") 'completion-at-point)

;; Move more quickly
(global-set-key (kbd "C-S-n") (lambda (ignore-errors (next-line 5))))
(global-set-key (kbd "C-S-p") (lambda (ignore-errors (previous-line 5))))
(global-set-key (kbd "C-S-f") (lambda (ignore-errors (forward-char 5))))
(global-set-key (kbd "C-S-b") (lambda (ignore-errors (backward-char 5))))

(global-set-key (kbd "H-*") 'beginning-of-buffer) ;; H-p
(global-set-key (kbd "H-n") 'end-of-buffer)

;; Convenience on ThinkPad Keyboard: Use back/forward as pg up/down
(global-set-key (kbd "<XF86Back>") 'scroll-down)
(global-set-key (kbd "<XF86Forward>") 'scroll-up)
(global-set-key (kbd "<XF86WakeUp>") 'beginning-of-buffer)

;; Query replace regex key binding
(global-set-key (kbd "M-&") 'query-replace-regexp)

;; Comment/uncomment block
(global-set-key (kbd "C-c c") 'comment-or-uncomment-region)
(global-set-key (kbd "C-c u") 'uncomment-region)

;; Eval buffer
(global-set-key (kbd "C-c C-k") 'eval-buffer)

;; Move windows, even in org-mode
(global-set-key (kbd "<C-S-kp-6>") 'other-frame)
(global-set-key (kbd "<C-S-kp-4>") (lambda() (interactive (other-frame -1))))

;; Clever newlines
(global-set-key (kbd "<C-return>") 'open-line-below)
(global-set-key (kbd "<C-S-return>") 'open-line-above)
;;(global-set-key (kbd "<M-return>") 'new-line-dwim)
(global-set-key (kbd "<M-return>") (lambda ()
                                     (interactive)
                                     (save-excursion
                                       (end-of-line)
                                       (open-line 1))))


;; Duplicate region
(global-set-key (kbd "C-c d") 'duplicate-current-line-or-region)

;; Fold the active region
(global-set-key (kbd "C-c C-f") 'fold-this-all)
(global-set-key (kbd "C-c C-F") 'fold-this)
(global-set-key (kbd "C-c M-f") 'fold-this-unfold-all)

;; Yank and indent
(global-set-key (kbd "C-S-y") 'yank-unindented)

;; Toggle quotes
(global-set-key (kbd "C-\"") 'toggle-quotes)

;; Sorting
(global-set-key (kbd "M-s l") 'sort-lines)

;; Increase number at point (or other change based on prefix arg)
(global-set-key (kbd "C-+") 'change-number-at-point)
(global-set-key (kbd "C-?") 'subtract-number-at-point)
(eval-after-load 'undo-tree '(define-key undo-tree-map (kbd "C-?") nil))

;; Jump from file to containing directory
(global-set-key (kbd "C-x C-j") 'dired-jump)
(autoload 'dired-jump "dired")
(global-set-key (kbd "C-x M-j") (lambda (dired-jump 1)))

;; Easy-mode fullscreen rgrep
(global-set-key (kbd "M-s s") 'git-grep-fullscreen)
(global-set-key (kbd "M-s S") 'rgrep-fullscreen)

;; Multi-occur
(global-set-key (kbd "M-s m") 'multi-occur)
(global-set-key (kbd "M-s M") 'multi-occur-in-matching-buffers)

;; Display and edit occurances of regexp in buffer
(global-set-key (kbd "C-c o") 'occur)

;; Find files by name and display results in dired
(global-set-key (kbd "M-s f") 'find-name-dired)

;; Find file in project
;; (global-set-key (kbd "C-x o") 'find-file-in-project)

;; Find file in project, with specific patterns
;;(global-unset-key (kbd "C-x C-o")) ;; which used to be delete-blank-lines (also bound to C-c C-<return>)
;;(global-set-key (kbd "C-x C-o ja") (ffip-create-pattern-file-finder "*.java"))
;;(global-set-key (kbd "C-x C-o js") (ffip-create-pattern-file-finder "*.js"))
;;(global-set-key (kbd "C-x C-o jn") (ffip-create-pattern-file-finder "*.json"))
;;(global-set-key (kbd "C-x C-o ht") (ffip-create-pattern-file-finder "*.html"))
;;(global-set-key (kbd "C-x C-o jp") (ffip-create-pattern-file-finder "*.jsp"))
;;(global-set-key (kbd "C-x C-o cs") (ffip-create-pattern-file-finder "*.css"))
;;(global-set-key (kbd "C-x C-o ft") (ffip-create-pattern-file-finder "*.feature"))
;;(global-set-key (kbd "C-x C-o cl") (ffip-create-pattern-file-finder "*.clj"))
;;(global-set-key (kbd "C-x C-o el") (ffip-create-pattern-file-finder "*.el"))
;;(global-set-key (kbd "C-x C-o ed") (ffip-create-pattern-file-finder "*.edn"))
;;(global-set-key (kbd "C-x C-o md") (ffip-create-pattern-file-finder "*.md"))
;;(global-set-key (kbd "C-x C-o rb") (ffip-create-pattern-file-finder "*.rb"))
;;(global-set-key (kbd "C-x C-o or") (ffip-create-pattern-file-finder "*.org"))
;;(global-set-key (kbd "C-x C-o ph") (ffip-create-pattern-file-finder "*.php"))
;;(global-set-key (kbd "C-x C-o tx") (ffip-create-pattern-file-finder "*.txt"))
;;(global-set-key (kbd "C-x C-o vm") (ffip-create-pattern-file-finder "*.vm"))
;;(global-set-key (kbd "C-x C-o xm") (ffip-create-pattern-file-finder "*.xml"))
;;(global-set-key (kbd "C-x C-o in") (ffip-create-pattern-file-finder "*.ini"))
;;(global-set-key (kbd "C-x C-o pr") (ffip-create-pattern-file-finder "*.properties"))
;;(global-set-key (kbd "C-x C-o in") (ffip-create-pattern-file-finder "*.ini"))
;;(global-set-key (kbd "C-x C-o gr") (ffip-create-pattern-file-finder "*.groovy"))
;;(global-set-key (kbd "C-x C-o ga") (ffip-create-pattern-file-finder "*.gradle"))
;;(global-set-key (kbd "C-x C-o sc") (ffip-create-pattern-file-finder "*.scala"))
;;(global-set-key (kbd "C-x C-o ss") (ffip-create-pattern-file-finder "*.scss"))
;;(global-set-key (kbd "C-x C-o co") (ffip-create-pattern-file-finder "*.conf"))
;;(global-set-key (kbd "C-x C-o j2") (ffip-create-pattern-file-finder "*.j2"))
;;(global-set-key (kbd "C-x C-o sh") (ffip-create-pattern-file-finder "*.sh"))
;;(global-set-key (kbd "C-x C-o ic") (ffip-create-pattern-file-finder "*.ico"))
;;(global-set-key (kbd "C-x C-o sv") (ffip-create-pattern-file-finder "*.svg"))
;;(global-set-key (kbd "C-x C-o !") (ffip-create-pattern-file-finder "*"))

;; View occurrence in occur mode
(define-key occur-mode-map (kbd "v") 'occur-mode-display-occurrence)
(define-key occur-mode-map (kbd "n") 'next-line)
(define-key occur-mode-map (kbd "p") 'previous-line)

;; More neat bindings for C-x 8
(global-set-key (kbd "C-x 8 t m") (lambda (insert "™")))
(global-set-key (kbd "C-x 8 ( c )") (lambda (insert "©")))
(global-set-key (kbd "C-x 8 - >") (lambda (insert "→")))
(global-set-key (kbd "C-x 8 8") (lambda (insert "∞")))
(global-set-key (kbd "C-x 8 ( c )") (lambda (insert "©")))
(global-set-key (kbd "C-x 8 v") (lambda (insert "✓")))

;; No need to ask for confirmation on these
(put 'downcase-region 'disabled nil)
(put 'upcase-region 'disabled nil)
(put 'narrow-to-region 'disabled nil)

(provide 'key-bindings)
