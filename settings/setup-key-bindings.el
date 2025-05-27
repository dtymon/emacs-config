(let ((map global-map))
  ;; Simple navigation
  (define-key map (kbd "<home>") #'beginning-of-line)
  (define-key map (kbd "<end>")  #'end-of-line)
  (define-key map (kbd "M-p")    #'backward-paragraph)
  (define-key map (kbd "M-n")    #'forward-paragraph)

  ;; Page up and down
  (define-key map (kbd "C-v")     #'scroll-up-command)
  (define-key map (kbd "C-z")     #'scroll-down-command)
  (define-key map (kbd "<next>")  #'scroll-up-command)
  (define-key map (kbd "<prior>") #'scroll-down-command)

  ;; Move more quickly
  (define-key map (kbd "M-<up>")    #'backward-sexp)
  (define-key map (kbd "M-<down>")  #'forward-sexp)
  (define-key map (kbd "M-<left>")  #'backward-sexp)
  (define-key map (kbd "M-<right>") #'forward-sexp)
  (define-key map (kbd "C-S-n")     (lambda () (interactive) (ignore-errors (next-line 5))))
  (define-key map (kbd "C-S-p")     (lambda () (interactive) (ignore-errors (previous-line 5))))
  (define-key map (kbd "C-S-f")     (lambda () (interactive) (ignore-errors (forward-char 5))))
  (define-key map (kbd "C-S-b")     (lambda () (interactive) (ignore-errors (backward-char 5))))

  ;; Buffer navigation
  (define-key map (kbd "C-x <down>") #'next-buffer)
  (define-key map (kbd "C-x <up>")   #'previous-buffer)
  (define-key map (kbd "C-x C-n")    #'next-buffer)
  (define-key map (kbd "C-x C-p")    #'previous-buffer)
  (define-key map (kbd "C-x !")      #'delete-other-windows-vertically)
  (define-key map (kbd "C-x _")      #'balance-windows)
  (define-key map (kbd "C-x -")      #'fit-window-to-buffer)
  (define-key map (kbd "C-x +")      #'balance-windows-area)
  (define-key map (kbd "C-x }")      #'enlarge-window)
  (define-key map (kbd "C-x {")      #'shrink-window)
  (define-key map (kbd "C-x >")      #'enlarge-window-horizontally)
  (define-key map (kbd "C-x <")      #'shrink-window-horizontally)

  (define-key map (kbd "M-w")     #'kill-ring-save)
  (define-key map (kbd "C-m")     #'newline-and-indent)
  (define-key map (kbd "C-S-k")   #'delete-region)
  (define-key map (kbd "C-M-z")   #'scroll-other-window-down)
  (define-key map (kbd "C-x C-j") #'goto-line)
  (define-key map (kbd "C-x r N") #'rectangle-insert-number-sequence)
  (define-key map (kbd "C-x M-e") #'append-to-lines-in-region)

  (define-key map (kbd "C-x x")   #'copy-to-register)
  (define-key map (kbd "C-x g")   #'insert-register)
  (define-key map (kbd "C-x /")   #'point-to-register)
  (define-key map (kbd "C-x j")   #'jump-to-register)

  (define-key map [(shift insert)] #'dtymon::x-cut-buffer-yank)
  )

;; Resizing windows horizontally
(let ((map resize-window-repeat-map))
  (define-key map ">" #'enlarge-window-horizontally)
  (define-key map "<" #'shrink-window-horizontally)
  )

;; Undo and restore window configurations
(let ((map global-map))
  (define-key map (kbd "C-x <left>")  #'winner-undo)
  (define-key map (kbd "C-x <right>") #'winner-redo)
  )

;; Rotate buffers clockwise and anticlockwise
(let ((map global-map))
  (define-key map (kbd "C-x C-<left>")  #'dtymon::rotate-window-buffers-clockwise)
  (define-key map (kbd "C-x C-<right>") #'dtymon::rotate-window-buffers-anticlockwise)
  )

;; Hide and show the sideline
(let ((map global-map))
  (define-key map (kbd "<f5>") #'prot-sideline-mode)
  (define-key map (kbd "<f6>") #'prot-sideline-negative-space-toggle)
  )

;; Frames
(let ((map global-map))
  ;; Create a new frame showing the *scratch* buffer
  (define-key map (kbd "<f7>") (lambda ()
                                 (interactive)
                                  (cond ((y-or-n-p "Create new frame ? ")
                                         (other-frame-prefix)
                                         (switch-to-buffer "*scratch*")))
                                 ))
  ;; Cycle through frames
  (define-key map (kbd "<f8>") #'other-frame)
  )

;; Commenting regions
(let ((map global-map))
  (define-key map (kbd "C-c c") #'comment-or-uncomment-region)
  (define-key map (kbd "C-c u") #'uncomment-region)
  )

;; Stop me minimising the frame when I accidentally hit C-z instead of C-x
(global-unset-key (kbd "C-x C-z"))

;; No need to ask for confirmation on these
(put 'downcase-region 'disabled nil)
(put 'upcase-region 'disabled nil)
(put 'narrow-to-region 'disabled nil)

(provide 'setup-key-bindings)
