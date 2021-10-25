(require 'dash)

;; Windows keyboard layout
(setq mac-option-modifier 'meta)
(setq mac-command-modifier 'super)
(setq ns-function-modifier 'hyper)

;; Macbook keyboard layout with Command key operating as Meta
;;(setq mac-option-modifier 'super)
;;(setq mac-command-modifier 'meta)
;;(setq ns-function-modifier 'hyper)

;; When using Windows keyboard on Mac, the insert key is mapped to <help>
;; copy ctrl-insert, paste shift-insert on windows keyboard
(global-set-key [C-help] #'clipboard-kill-ring-save)
(global-set-key [S-help] #'clipboard-yank)
;; insert to toggle `overwrite-mode'
(global-set-key [help] #'overwrite-mode)

;; Bind Cmd-v to paste for when there is no Insert key
(global-set-key (kbd "s-v") 'clipboard-yank)

(global-set-key (kbd "s-u") 'universal-argument)
(global-set-key (kbd "s--") 'negative-argument)
(--dotimes 5 (global-set-key (read-kbd-macro (format "s-%d" it)) 'digit-argument))

;; keybinding to toggle full screen mode
(global-set-key (quote [M-f10]) (quote ns-toggle-fullscreen))

;; Move to trash when deleting stuff
(setq delete-by-moving-to-trash t
      trash-directory "~/.Trash/emacs")

;; Ignore .DS_Store files with ido mode
(add-to-list 'ido-ignore-files "\\.DS_Store")

;; Don't open files from the workspace in a new frame
(setq ns-pop-up-frames nil)

;; Use aspell for spell checking: brew install aspell --lang=en
(setq ispell-program-name "/usr/local/bin/aspell")

;; Open files
(defun mac-open-current-file ()
  (interactive)
  (shell-command (concat "open " (buffer-file-name))))

(global-set-key (kbd "C-c C-S-o") 'mac-open-current-file)

;; fix osx weirdness with magit avatars

(setq-default magit-revision-use-gravatar-kludge t)

;; Disable automatic interaction with the clipboard as this copies kill-ring
;; entries too. We only want dragged selections to be copied.
(setq select-enable-clipboard nil)

;; When the region is selected via a mouse drag, copy the selection to the
;; system clipboard.
(defun dragged-selection-to-clipboard (orig-fun &rest args)
  (let ((res (apply orig-fun args)))
    (shell-command-on-region (region-beginning) (region-end) "pbcopy")
    res))

(advice-add 'mouse-set-region :around #'dragged-selection-to-clipboard)

;; Setup environment variables from the user's shell.
(require-package 'exec-path-from-shell)
(exec-path-from-shell-initialize)

(provide 'setup-mac)
