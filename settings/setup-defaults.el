;; -*- lexical-binding: t -*-

;; Seed the random-number generator
(random t)

;; Some basic variables
(setq
 frame-title-format          "%b"
 focus-follows-mouse         'auto-raise
 make-pointer-invisible      t
 ;;mouse-autoselect-window     0.5
 mouse-autoselect-window     nil
 dabbrev-case-fold-search    nil
 grep-command                "egrep --color -nHr -e "
 history-delete-duplicates   t
 use-short-answers           t

 ;; Don't indent anything that has been yanked
 yank-indent-modes '()

 ;; Turn off key binding suggestions
 suggest-key-bindings nil

 ;; Don't be greedy when isearching for whitespace
 isearch-lax-whitespace nil

 ;; Allow pasting selection outside of Emacs
 x-select-enable-clipboard t

 ;; Assume themes are safe
 custom-safe-themes t

 ;; Show keystrokes in progress
 echo-keystrokes 0.1

 ;; Move files to trash when deleting
 delete-by-moving-to-trash t

 ;; Don't use shift select
 shift-select-mode nil

 ;; When pasting with the mouse, paste at the current point not at the mouse
 ;; location.
 mouse-yank-at-point t

 ;; After an initial C-u C-SPC, additional C-SPC calls continue to pop the mark
 set-mark-command-repeat-pop t

 ;; Always display line and column numbers in the modeline
 line-number-mode t
 column-number-mode t

 ;; Column numbers should start from one
 column-number-indicator-zero-based nil

 ;; Lines should be 80 characters wide by default
 fill-column 80
 comment-fill-column 80

 electric-indent-mode t

 ;; Show the full stack when eval'ing an expression
 eval-expression-print-level nil

 ;; Dictionary to use for spelling
 ispell-local-dictionary "en_AU"

 ;; Comment region should include empty lines
 comment-empty-lines t

 ;; Don't generate messages with defadvice when redefinitions occur
 ad-redefinition-action 'accept

 ;; Scroll interpreter output automatically
 comint-move-point-for-output t
 compilation-scroll-output t

 ;; Timezones to show with world-clock
 world-clock-time-format "%Y-%m-%dT%H:%M:%S%z %a, %d %b %H:%M %Z"
 world-clock-list '(("Etc/UTC" "UTC")
                    ("Pacific/Auckland" "Auckland")
                    ("Australia/Melbourne" "Melbourne")
                    ("Asia/Tokyo" "Tokyo")
                    ("Asia/Singapore" "Singapore")
                    ("Asia/Dubai" "Dubai")
                    ("Europe/Athens" "Athens")
                    ("Europe/Paris" "Paris")
                    ("Europe/London" "London")
                    ("America/New_York" "New York")
                    ("America/Denver" "Denver")
                    ("America/Los_Angeles" "Los Angeles"))
 )

;; 80 chars is the default fill column
(set-default 'fill-column 80)

;; Enable syntax highlighting where possible
(global-font-lock-mode t)

;; Enable UTF-8 where possible
(setq locale-coding-system 'utf-8)
(set-terminal-coding-system 'utf-8)
(set-keyboard-coding-system 'utf-8)
(set-selection-coding-system 'utf-8)
(prefer-coding-system 'utf-8)

;; Don't show active region
(transient-mark-mode nil)
(make-variable-buffer-local 'transient-mark-mode)
(put 'transient-mark-mode 'permanent-local nil)
(setq-default transient-mark-mode nil)

;; Typing will replace the selected region
(delete-selection-mode t)

;; Undo/redo window configuration with C-c <left>/<right>
(winner-mode 1)

;; Never insert tabs
(set-default 'indent-tabs-mode nil)

;; Show me empty lines after buffer end
(set-default 'indicate-empty-lines t)

;; Enable navigating CamelCase words
(global-subword-mode 1)

;; Sentences do not need double spaces to end
(set-default 'sentence-end-double-space nil)

;; Enable this to avoid long lines wrapping and taking up two or more screen
;; lines
;; (setq-default truncate-lines t)

;; This works better. Protect against very long lines.
(global-so-long-mode 1)

;; Keep cursor away from edges when scrolling up/down
;;(require 'smooth-scrolling)
;;(smooth-scrolling-mode 1)
(setq
 scroll-step           1
 scroll-margin         10
 scroll-conservatively 10000
 auto-window-vscroll   nil
 )

;; Configure completion styles
(setq
 completion-styles             '(basic substring initials flex partial-completion)
 completion-category-overrides '((file (styles . (basic partial-completion initials substring))))

 ;; Never use completion cycling
 completion-cycle-threshold    nil

 ;; Case is significant
 completion-ignore-case        nil

 ;; No inline help
 completion-show-inline-help   nil

 ;; Include more details
 completions-detailed          t
 )

;; Transparently open compressed files
(auto-compression-mode t)


;; When a file is not found due to missing parent directories, ask if they
;; should be created.
(defun dtymon::create-non-existent-directory ()
  (let ((parent-directory (file-name-directory buffer-file-name)))
    (when (and (not (file-exists-p parent-directory))
               (y-or-n-p (format "Directory `%s' does not exist! Create it?" parent-directory)))
      (make-directory parent-directory t))))

(add-to-list 'find-file-not-found-functions 'dtymon::create-non-existent-directory)

(provide 'setup-defaults)
