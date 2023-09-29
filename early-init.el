;; Bump up the GC cons limit during initialisation but reset it after
;; loading to avoid large pauses during GC.
;;(setq gc-cons-threshold 100000000)
(setq gc-cons-threshold (* 1024 1024 128))
;; let's try leaving it large for a bit
;;(add-hook 'after-init-hook (lambda () (setq gc-cons-threshold 800000)))

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
(load (locate-user-emacs-file "pre-init-hooks.el") :no-error :no-message)

;; Don't want a custom file
(setq custom-file "/dev/null")

;; Turn off GUI components if present
(if (fboundp 'menu-bar-mode) (menu-bar-mode -1))
(if (fboundp 'tool-bar-mode) (tool-bar-mode -1))
(if (fboundp 'scroll-bar-mode) (scroll-bar-mode -1))

;; Enable saving and restoring the desktop session
(desktop-save-mode 1)
(setq
  desktop-auto-save-timeout 300
  desktop-missing-file-warning nil
;;   desktop-files-not-to-save ".*"
;;   desktop-buffers-not-to-save ".*"
;;   desktop-globals-to-clear nil
;;   desktop-load-locked-desktop t
;;   desktop-restore-eager 0
;;   desktop-restore-frames 0
;;   desktop-save 'ask-if-new
  )

;; (dolist (symbol '(kill-ring log-edit-comment-ring))
;;   (add-to-list 'desktop-globals-to-save symbol))

;; Turn off other stuff too
(setq
 ;; We want confirmation on kill
 confirm-kill-emacs 'yes-or-no-p

 ;; Don't flash the screen on a bell
 visible-bell nil

 ;; In fact, just ignore the bell altogether
 ring-bell-function 'ignore

 ;; Don't want modal dialogs
 use-dialog-box nil

 ;; Don't want file dialogs
 use-file-dialog nil

 ;; Don't want a splash or startup screen
 inhibit-splash-screen t
 inhibit-startup-screen t
 inhibit-startup-echo-area-message user-login-name
 inhibit-startup-buffer-menu t
 )

;; Configure the package management
(setq
 package-enable-at-startup t

 ;; Allow loading from the package cache
 ;; package-quickstart t
 )

;; Native compilation
(when (native-comp-available-p)
  (setq
   ;; Turn on package compilation
   package-native-compile t

   ;; Do not report errors during native compilation
   native-comp-async-report-warnings-errors 'silent

   ;; Remove any .eln files that are no longer relevant
   native-compile-prune-cache t
  )
)

;; Define some basic variables
(setq
 ;; Bump up the maximum number of bytes that can be read in one chunk from a
 ;; process.
 read-process-output-max (* 1024 1024)

 ;; Do not compact font caches during GC
 inhibit-compacting-font-caches t
 )
