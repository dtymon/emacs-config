;; Avoid exiting abruptly if I accidentally hit s-q instead of M-q
(global-set-key (kbd "s-q") (lambda ()
                              (interactive)
                              (fill-comment-paragraph)))

 ;; Set path to dependencies
(defconst site-lisp-dir (expand-file-name "site-lisp" user-emacs-directory))
(defconst settings-dir (expand-file-name "settings" user-emacs-directory))

;; Whether to use flymake/eglot or flycheck/lsp
(defconst use-flymake nil)

;; Are we on a mac?
(defconst is-mac (equal system-type 'darwin))

;; Define some basic variables
(setq
 ;; Make sure we always default to the home directory when opening files
 default-directory (concat (getenv "HOME") "/")

 ;; Set path to dependencies
 site-lisp-dir (expand-file-name "site-lisp" user-emacs-directory)
 settings-dir (expand-file-name "settings" user-emacs-directory)

 ;; Don't make backups of files that are under version control
 vc-make-backup-files nil
 )

;; Set up load path
(add-to-list 'load-path settings-dir)
(add-to-list 'load-path site-lisp-dir)

;; Set some sensible defaults
(require 'setup-defaults)

;; Configure the packaging
(require 'setup-package)

;; Miscellaneous
(require 'setup-dash)
(require 'setup-defuns)
(require 'setup-no-littering)
(require 'setup-undo-tree)
(require 'setup-which-key)
(require 'setup-whitespace-cleanup)
(require 'setup-browse-kill-ring)
(require 'setup-hydra)

;; Saving history, state etc
;; now setup earlier in early-init.el
;;(require 'setup-desktop)
(require 'setup-savehist)
(require 'setup-saveplace)

;; Frames, windows and buffers
(require 'setup-minibuffer)
(require 'setup-windmove)
(require 'setup-beframe)
(require 'setup-sideline)
(require 'setup-key-bindings)
(require 'setup-transpose-frame)
(require 'setup-rotate)
(require 'setup-recentf)
(require 'setup-uniquify)

;; Visuals
(require 'setup-appearance)
(require 'setup-modeline)
(require 'setup-dimmer)
(require 'setup-rainbow-delimiters)
(require 'setup-all-the-icons)
(require 'setup-vscode-icon)
(require 'setup-highlight-escape-sequences)

;; Minibuffer
(require 'setup-ido)
(require 'setup-smex)

;; Completions
(require 'setup-company)
(require 'setup-consult)
(require 'setup-mct)
(require 'setup-hippie)
;; (require 'setup-helm)

;; Dired
(require 'setup-dired)

;; Search and Replace
(require 'setup-rgrep)
(require 'setup-wgrep)
(require 'setup-visual-regexp)
(require 'setup-ripgrep)
(require 'setup-iedit)

;; Coding
(cond
 (use-flymake
  (require 'setup-flymake)
  (require 'setup-eglot))
 (t
  (require 'setup-flycheck)
  (require 'setup-lsp-mode))
  ;; (require 'setup-dap-mode))
 )

(require 'setup-prog-mode)
(require 'setup-compilation-mode)
(require 'setup-ediff)
(require 'setup-projectile)
(require 'setup-treemacs)
(require 'setup-tree-sitter)
;; (require 'setup-treesit)
(require 'setup-magit)
(require 'setup-prettier)
(require 'setup-elisp-slime-nav)
(require 'setup-typescript-mode)
(require 'setup-js2-mode)
(require 'setup-json-mode)
(require 'setup-python)
(require 'setup-yaml-mode)
(require 'setup-yasnippet)
(require 'setup-makefile-mode)
(require 'setup-sh-script)
(require 'setup-markdown-mode)
(require 'setup-dockerfile-mode)
(require 'setup-nxml-mode)
(require 'setup-terraform-mode)
(require 'setup-groovy-mode)
(require 'setup-cmake-mode)
(require 'setup-go-mode)
(require 'setup-restclient)
(require 'setup-ztree)
(require 'setup-string-inflection)

;; Terminals
(require 'setup-vterm)
(require 'setup-eshell)

(when is-mac (require 'setup-mac))

;; Load any post-init-hooks
(load (locate-user-emacs-file "post-init-hooks.el") :no-error :no-message)

;; Load this as late as possible
(require 'setup-envrc)

;;; init.el ends here
