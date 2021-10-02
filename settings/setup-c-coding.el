;; ;; ----------------------------------------------------------------------
;; ;;    Top-level defaults
;; ;; ----------------------------------------------------------------------
;;
;; ;; Some constants
;; (defconst dtymon::shift-width 4)
;; (defconst dtymon::tab-width   8)
;; (defconst dtymon::fill-column 80)
;;
;; (setq
;;  ;;c-basic-offset         dtymon::shift-width
;;  tab-width              dtymon::tab-width
;;  indent-tabs-mode       nil
;;  )
;;
;;
;; ;; ----------------------------------------------------------------------
;; ;;    Coding styles
;; ;; ----------------------------------------------------------------------
;;
;; (defconst my-c-style
;;   '((c-basic-offset                        . 4)
;;     (fill-column                           . 80)
;;     (c-tab-always-indent                   . t)
;;     (c-comment-only-line-offset            . 0)
;;     (c-echo-syntactic-information-p        . t)
;;     (c-offsets-alist .
;;                      ((substatement-open . 0)
;;                       (inline-open       . 0)
;;                       (case-label        . +)
;;                       (arglist-close     . 0)
;;                       ))
;;
;;     )
;;   "My C/C++ Programming Style")
;;
;;
;; (defconst davidt-style
;;   (let ((tmp-sinefa-style
;;
;;   '((c-basic-offset               . 4)
;;     (c-hanging-braces-alist       . ((class-open        . exp-class-open)
;;                                      (class-close         before)
;;                                      (defun-open          before after)
;;                                      (defun-close         before after)
;;                                      (inline-open         before after)
;;                                      (inline-close        before after)
;;                                      (brace-list-open     after)
;;                                      (brace-list-close    before)
;;                                      (block-open          before after)
;;                                      (block-close       . exp-snug-close)
;;                                      (substatement-open   after)
;;                                      (substatement-close  before after)
;;                                      (statement-case-open after)
;;                                      (extern-lang-open    after)
;;                                      (extern-lang-close   before after)))
;;     (c-hanging-colons-alist       . ((case-label          )
;;                                      (label               after)
;;                                      (access-label        after)
;;                                      (member-init-intro   before)
;;                                      (inher-intro         before)))
;;     (c-cleanup-list               .  (empty-defun-braces
;;                                       defun-close-semi
;;                                       list-close-coma
;;                                       scope-operator))
;;     (c-offsets-alist . (
;;                         (innamespace           . 0)
;;                         (case-label            . +)
;;                         (inline-open           . 0)
;;                         (label                 . 2)
;;                         (arglist-close         . 0)
;;                         (substatement-open     . 0)
;;                         (statement-block-intro . +)
;;                         (arglist-cont-nonempty . +)
;;                         )
;;                      )) ))
;;
;;     tmp-sinefa-style )
;;   "David's Codebase Style Guide")
;;
;;
;; ;; ----------------------------------------------------------------------
;; ;;    C/C++ mode setup
;; ;; ----------------------------------------------------------------------
;;
;; ;; Return t if we can find a line starting with an opening brace or a C++
;; ;; access keyword and the next line starts with a tab
;; (defmacro dtymon::c++-tab-search1 ()
;;   (beginning-of-buffer)
;;   (re-search-forward
;;    "^\\({\\|public:\\|private:\\|protected:\\)\n\t" (point-max) t))
;;
;; ;; Return t if we can find an opening brace preceded only by tabs with the
;; ;; following line indented by one more tab
;; (defmacro dtymon::c++-tab-search2 ()
;;   (let ((found nil) (pattern nil))
;;     (beginning-of-buffer)
;;     (while (and (not found)
;;                 (re-search-forward "^\\(\t*\\){\n" (point-max) t))
;;       (setq pattern (concat "^" (match-string 1) "\t"))
;;       (setq found (looking-at pattern))
;;       )
;;     found))
;;
;;
;; (defun c++-tab-width ()
;;   "Determine the tab-width of a C/C++ source file"
;;   (save-excursion
;;     (beginning-of-buffer)
;;     (cond
;;
;;      ;; Opening brace (or public, protected or private) followed by a line
;;      ;; starting with a tab suggests a tab-width of 4
;;      ((dtymon::c++-tab-search1) 4)
;;
;;      ;; Search for an opening brace that only has preceding tabs and the next
;;      ;; line is indented by one more tab
;;      ((dtymon::c++-tab-search2) 4)
;;
;;      ;; Default to my preferred size
;;      (t
;;       (message "Setting default C++ tab-width: %d" dtymon::tab-width)
;;       dtymon::tab-width)
;;      )))
;;
;; ;; If this value is set to t, buffers contaiing C files will be cleaned before
;; ;; saving. All trailing whitespace will be removed and the buffer will be
;; ;; untabified.
;; (defconst c-clean-buffer-before-saving nil
;;   "*If non-nil, remove-trailing-spaces and untabify buffer before saving.")
;;
;;
;; (defun cc-mode-common-code-setup ()
;;   ;; Ensure my style exists
;;   (if (not (assoc "davidt-cc" c-style-alist))
;;       (progn
;;         (c-add-style "davidt-cc" davidt-style t)
;;         (setq c-default-style "davidt-cc"))
;;     )
;;   (c-set-style "davidt-cc")
;;
;;   ;; Configure auto-fill
;;   (auto-fill-mode 1)
;;   (set-fill-column dtymon::fill-column)
;;   (fci-mode 1)
;;
;;   ;; Other stuff
;;   (setq c-electric-pound-behavior       '(alignleft)
;;         ;; c-comment-continuation-stars    "* "
;;         c-tab-always-indent             1
;;         ;; c-basic-offset                  dtymon::shift-width
;;         tab-width                       (c++-tab-width)
;;         c-backslash-max-column          88
;;         indent-tabs-mode                nil
;;         truncate-lines                  nil
;;         ;; c-hanging-comment-ender-p       nil
;;         )
;;
;;   (define-key c++-mode-map "\M-."     'dabbrev-expand)
;;   (define-key c++-mode-map [f7]       'dtymon::align-data-members)
;;   (define-key c++-mode-map [f8]       'dtymon::insert-method-separator)
;;   (define-key c++-mode-map [f9]       'dtymon::find-file-cycle)
;;   (define-key c++-mode-map [f10]      'find-tag-at-point)
;;   (define-key c++-mode-map [f11]      'pop-tag-mark)
;;
;;   ;; Cleaning File on Save
;;   (if c-clean-buffer-before-saving
;;       (progn
;;         (add-hook 'local-write-file-hooks 'remove-trailing-spaces)
;;         (add-hook 'local-write-file-hooks 'untabify-buffer)
;;         ))
;;   )
;;
;; ;; Ensure the common hook is run for modes defined in cc-mode
;; (add-hook 'c-mode-common-hook 'cc-mode-common-code-setup)
;;

(provide 'setup-c-coding)
