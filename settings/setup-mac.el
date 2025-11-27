;; -*- lexical-binding: t -*-

;; Windows keyboard layout
(setq
 mac-option-modifier 'meta
 mac-command-modifier 'super
 ns-function-modifier 'hyper
 )

;; Macbook keyboard layout with Command key operating as Meta
;;(setq mac-option-modifier 'super)
;;(setq mac-command-modifier 'meta)
;;(setq ns-function-modifier 'hyper)

(let ((map global-map))
  ;; When using Windows keyboard on Mac, the insert key is mapped to <help>
  ;; copy ctrl-insert, paste shift-insert on windows keyboard
  (define-key map [C-help] #'clipboard-kill-ring-save)
  (define-key map [S-help] #'clipboard-yank)

  ;; insert to toggle `overwrite-mode'
  (define-key map [help] #'overwrite-mode)

  ;; Bind Cmd-v to paste for when there is no Insert key
  (define-key map (kbd "s-v") #'clipboard-yank)

  (define-key map (kbd "s-u") #'universal-argument)
  (define-key map (kbd "s--") #'negative-argument)

  ;; Super-<n> for quick universal arguments
  (define-key map (read-kbd-macro "s-1") 'digit-argument)
  (define-key map (read-kbd-macro "s-2") 'digit-argument)
  (define-key map (read-kbd-macro "s-3") 'digit-argument)
  (define-key map (read-kbd-macro "s-4") 'digit-argument)

  ;; Toggle full screen mode
  (define-key map (quote [M-f10]) #'toggle-frame-fullscreen)
  )

;; Ignore .DS_Store files with ido mode
(eval-after-load 'ido
  (lambda ()
    (add-to-list 'ido-ignore-files "\\.DS_Store")
    (add-to-list 'ido-ignore-buffers "\\*EGLOT")
    ))

(setq
 ;; Move to trash when deleting stuff
 delete-by-moving-to-trash t
 trash-directory           "~/.Trash/emacs"

 ;; Don't open files from the workspace in a new frame
 ns-pop-up-frames nil

 ;; Use aspell for spell checking: brew install aspell --lang=en
 ispell-program-name "aspell"

 ;; Disable automatic interaction with the clipboard as this copies kill-ring
 ;; entries too. We only want dragged selections to be copied.
 select-enable-clipboard nil
 )

;; When the region is selected via a mouse drag, copy the selection to the
;; system clipboard.
(defun dtymon::dragged-selection-to-clipboard (orig-fun &rest args)
  (let ((res (apply orig-fun args)))
    (shell-command-on-region (region-beginning) (region-end) "pbcopy")
    res))

(advice-add 'mouse-set-region :around #'dtymon::dragged-selection-to-clipboard)

;; Setup environment variables from the user's shell.
(use-package exec-path-from-shell
  :ensure t
  :config
  (setq exec-path-from-shell-arguments '("-l"))
  (exec-path-from-shell-initialize)
  )

;; Tweak the path
(setq dtymon::nvm-bin (getenv "NVM_BIN"))
(cond (dtymon::nvm-bin
         (add-to-list 'exec-path dtymon::nvm-bin)
         (setenv "PATH" (concat dtymon::nvm-bin ":" (getenv "PATH")))
         (setenv "NODE_PATH" (concat dtymon::nvm-bin "/lib/node_modules:" (getenv "NODE_PATH")))
         )
      (t
       (error "There is no Node version configured")
        ))

;; Other env vars
(setenv "MYPATH" "src")

(provide 'setup-mac)
