;; Define some functions to get the monitor dimensions
(defun dtymon::monitor-width ()
  "The width of the monitor in pixels"
  (nth 3 (assq 'geometry (frame-monitor-attributes)))
  )

(defun dtymon::monitor-height ()
  "The height of the monitor in pixels"
  (nth 4 (assq 'geometry (frame-monitor-attributes)))
  )

;; Load any pre-init-hooks
(let ((hooks-file (expand-file-name "pre-init-hooks.el" user-emacs-directory)))
  (when (file-exists-p hooks-file)
    (load-file hooks-file)))

;; Turn off GUI components if present
(if (fboundp 'menu-bar-mode) (menu-bar-mode -1))
(if (fboundp 'tool-bar-mode) (tool-bar-mode -1))
(if (fboundp 'scroll-bar-mode) (scroll-bar-mode -1))

;; Bump up the GC cons limit during initialisation but reset it after
;; loading to avoid large pauses during GC.
;;(setq gc-cons-threshold 100000000)
(setq gc-cons-threshold (* 1024 1024 128))
;; let's try leaving it large for a bit
;;(add-hook 'after-init-hook (lambda () (setq gc-cons-threshold 800000)))

;; Define some basic variables
(setq
 ;; Turn on native compiling of external packages if available
 package-native-compile t

 ;; Bump up the maximum number of bytes that can be read in one chunk from a
 ;; process
 read-process-output-max (* 1024 1024 4)
 
 ;; Do not compact font caches during GC
 inhibit-compacting-font-caches t

 ;; Make sure we always default to the home directory when opening files
 default-directory (concat (getenv "HOME") "/")

 ;; Set path to dependencies
 site-lisp-dir (expand-file-name "site-lisp" user-emacs-directory)
 settings-dir (expand-file-name "settings" user-emacs-directory)

 ;; Are we on a mac?
 is-mac (equal system-type 'darwin)

 ;; Make backups of files, even when they're in version control
 vc-make-backup-files t
 )

;; Add extra CA certificates to trust
;; (require 'gnutls)
;; (add-to-list 'gnutls-trustfiles (concat (getenv "HOME") "/certs/bundle-firewall.pem"))
(eval-after-load 'gnutls '(add-to-list 'gnutls-trustfiles (concat (getenv "HOME") "/certs/bundle-firewall.pem")))

;; Set up load path
(add-to-list 'load-path settings-dir)
(add-to-list 'load-path site-lisp-dir)

;; Setup basics
(require 'setup-package)
(require 'setup-dash)
(require 'setup-defuns)

;; This is required as a workaround to bug:
;;    https://debbugs.gnu.org/cgi/bugreport.cgi?bug=25228
;;
;; which causes the default to be reset. Looks like it fetches
;; the default font from GConf and forces that as the default.
;; This line ignores events coming from GConf.
;;
(define-key special-event-map [config-changed-event] 'ignore)

;; Keep emacs Custom-settings in separate file
(setq custom-file (expand-file-name "custom.el" user-emacs-directory))
(load custom-file)

;; Add external projects to load path
(dolist (project (directory-files site-lisp-dir t "\\w+"))
  (when (file-directory-p project)
    (add-to-list 'load-path project)))

(require 'sane-defaults)
(require 'setup-no-littering)
(require 'setup-mode-mappings)
 
;; Functions (load all files in defuns-dir)
(setq defuns-dir (expand-file-name "defuns" user-emacs-directory))
(dolist (file (directory-files defuns-dir t "\\w+"))
  (when (file-regular-p file)
    (load file)))

;; Setup key bindings
(require 'key-bindings)

;; Misc
;; (require 'project-archetypes)
(require 'my-misc)

;; Set up appearance early
(require 'setup-doom-themes)
(require 'setup-solarized-theme)
(require 'setup-themes)
(require 'appearance)
(require 'setup-dimmer)
;;(require 'setup-fira-code)

(require 'setup-desktop-mode)
(require 'setup-global-bindings)
(require 'setup-minibuffer)
(require 'setup-windmove)
(require 'setup-transpose-frame)
(require 'setup-rotate)
(require 'setup-move-text)
(require 'setup-diminish)
(require 'setup-all-the-icons)
(require 'setup-vscode-icon)
(require 'setup-undo-tree)
(require 'setup-savehist)
(require 'setup-saveplace)
(require 'setup-recentf)
(require 'setup-uniquify)
(require 'setup-ido)
(require 'setup-smex)
(require 'setup-dired)
;;(require 'setup-dired-sidebar)
(require 'setup-flycheck)
;;(require 'setup-flymake)
(require 'setup-fci-mode)
;;(require 'setup-highlight-indent)
;;(require 'setup-multiple-cursors)
(require 'setup-hippie)
(require 'setup-rgrep)
(require 'setup-wgrep)
;;(require 'setup-deadgrep)
(require 'setup-magit)
(require 'setup-git-gutter)
(require 'setup-forge)
(require 'setup-gitlab)
(require 'setup-elisp-slime-nav)
;;(require 'setup-helm-mode)
;;(require 'setup-treemacs)
;;(require 'setup-neotree)
;;(require 'setup-projectile)
;;(require 'setup-perspective)
;;(require 'setup-counsel)
(require 'setup-prettier)
(require 'setup-company)
(require 'setup-which-key)
(require 'setup-lsp-mode)
;;(require 'setup-eglot)
(require 'setup-yasnippet)
(require 'setup-c-coding)
(require 'setup-typescript-mode)
(require 'setup-js2-mode)
(require 'setup-json-mode)
(require 'setup-jq-mode)
(require 'setup-yaml-mode)
(require 'setup-ruby-mode)
;;(require 'setup-tide)
;;(require 'setup-skewer-mode)
(require 'setup-tree-sitter)
(require 'setup-restclient)
;;(require 'setup-doom-modeline)
;;(require 'setup-spaceline)
(require 'setup-browse-kill-ring)
(require 'setup-guide-key)
(require 'setup-expand-region)
(require 'setup-change-inner)
(require 'setup-visual-regexp)
(require 'setup-highlight-escape-sequences)
(require 'setup-smart-forward)
;;(require 'setup-emacs-server)
;;(require 'setup-prodigy)
(require 'setup-whitespace-cleanup)
(require 'setup-makefile-mode)
(require 'setup-nxml-mode)
;;(require 'setup-antlr-mode)
(require 'setup-terraform-mode)
(require 'setup-cmake-mode)
(require 'setup-go-mode)
;;(require 'setup-swift-mode)
(require 'setup-sh-script)
(require 'setup-markdown-mode)
(require 'setup-css-eldoc)
(require 'setup-dockerfile-mode)
(require 'setup-htmlize)
(require 'setup-smartparens)
(require 'setup-rainbow-delimiters)
(require 'setup-vterm)

(when is-mac (require 'setup-mac))

(eval-after-load 'shell '(require 'setup-shell))

;; Load any post-init-hooks
(let ((hooks-file (expand-file-name "post-init-hooks.el" user-emacs-directory)))
  (when (file-exists-p hooks-file)
    (load-file hooks-file)))
