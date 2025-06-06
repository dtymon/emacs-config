(defun dtymon::kill-buffer-and-window (buffer window)
  (and (kill-buffer buffer)
       (not (eq window nil))
       (delete-window window)))

(defun dtymon::fill-comment-paragraph ()
  (interactive)
  (let ((fill-column comment-fill-column))
    (c-fill-paragraph)))

(defun dtymon::auto-fill-comments-only-hook ()
  (auto-fill-mode 1)
  (set (make-local-variable 'fill-nobreak-predicate)
       (lambda ()
         (let ((text-face (get-text-property (point) 'face)))
           (not (or (eq text-face 'font-lock-comment-face)
                    (eq text-face 'font-lock-doc-face)
                    (eq text-face 'tree-sitter-hl-face:comment)
                    ))
           ))))

(defun dtymon::x-cut-buffer-yank ()
  (interactive "*")
  (let ((cut-text (gui-get-selection 'PRIMARY)))
    (cond ((and cut-text (> (length cut-text) 0))
           (set-mark (point))
           (insert cut-text)
           )
          )))

(defun uuidgen ()
  "Generate a new uuid and insert at the point"
  (interactive)
  (save-excursion
    (insert (downcase (substring (shell-command-to-string "uuidgen") 0 -1)))
    ))

(defun uuidgen-short ()
  "Generate a new uuid, remove the dashes and insert at the point"
  (interactive)
  (save-excursion
    (insert (replace-regexp-in-string "-" "" (downcase (substring (shell-command-to-string "uuidgen") 0 -1))))
    ))

(defun uuidgen-rectangle (start end)
  "For each line in the rectangle insert a new uuid, removing the dashes"
  (interactive "r")
  (kill-rectangle start end)
  (apply-on-rectangle
   (lambda (startcol endcol &rest args)
     (delete-extract-rectangle startcol endcol)
     (move-to-column startcol)
     (uuidgen-short)) (region-beginning) (region-end) nil))

(defun rectangle-insert-number-sequence (start end &optional no-padding)
  "For each line in the rectangle insert an increasing number"
  (interactive "r")
  (let ((counter 1)
        (num-lines (count-lines (region-beginning) (region-end)))
        num-digits
        num-format
        )
    ;; How many digits do we need in the numbers?
    (setq num-digits (fceiling (/ (log num-lines) (log 10)))
          num-format (cond (no-padding (format "%%%dd" num-digits))
                           (t (format "%%0%dd" num-digits)))
          )
    (apply-on-rectangle
     (lambda (startcol endcol &rest args)
       (move-to-column startcol)
       (insert (format num-format counter))
       (setq counter (+ 1 counter))) (region-beginning) (region-end) nil)))

(defun rectangle-insert-number-sequence-no-pad (start end)
  "For each line in the rectangle insert an increasing number"
  (interactive "r")
  (rectangle-insert-number-sequence start end t))

(defun random-hex (&optional len)
  "Generate a random hex string and insert at the point"
  (interactive "p")
  (let ((str "") (len (or current-prefix-arg 6)))
    (while (< (length str) len)
      (setq str (concat str (format "%x" (random 16)))))
    (insert str)
    ))

(defun random-dec (&optional len)
  "Generate a random decimal string and insert at the point"
  (interactive "p")
  (let ((str "") (len (or current-prefix-arg 6)))
    (while (< (length str) len)
      (setq str (concat str (format "%d" (random 10)))))
    (insert str)
    ))

(defun random-dec-no-leading-zero (&optional len)
  "Generate a random decimal string and insert at the point"
  (interactive "p")
  (let (digit (str "") (len (or current-prefix-arg 6)))
    (while (< (length str) len)
      (cond ((equal "" str)
             (setq digit (+ 1 (random 9))))
             (t
             (setq digit (random 10))))
      (setq str (concat str (format "%d" digit))))
    (insert str)
    ))

(defun random-dec-no-leading-zero (&optional len)
  "Generate a random decimal string and insert at the point"
  (interactive "p")
  (let ((idx 0) (len (or current-prefix-arg 6)))
    (while (<= (incf idx) len)
      (insert (int-to-string
               (cond ((= 1 idx) (+ 1 (random 9)))
                     (t (random 10)))))
      )))

(defun append-to-lines-in-region (str start end)
  (interactive "sText: \nr")
    (save-excursion
      (goto-char end)
      (setq end (point-marker))
      (goto-char start)
      (while (< (point) end)
        (beginning-of-line)
        (or (eolp)
            (progn
              (end-of-line)
              (insert str)
              ))
        (forward-line 1))
      (move-marker end nil)))

(defun timet-secs ()
  "Insert the current time as secs since Epoch"
  (interactive)
  (save-excursion
    (insert (int-to-string (floor (float-time))))
    ))

(defun timet-msecs ()
  "Insert the current time as msecs since Epoch"
  (interactive)
  (save-excursion
    (insert (int-to-string (floor (* 1000 (float-time)))))
    ))

(defun timet-usecs ()
  "Insert the current time as usecs since Epoch"
  (interactive)
  (save-excursion
    (insert (int-to-string (floor (* 1000000 (float-time)))))
    ))

(defun dtymon::project-root-dir ()
  (file-name-as-directory (project-root (project-current t))))

(defun dtymon::file-exists-in-project (filename)
  (let ((project-filename (concat (dtymon::project-root-dir) filename)))
    (file-exists-p project-filename))
  )

(defun dtymon::jest-config-file ()
  (let ((project-root-dir (dtymon::project-root-dir))
        (node-package-config-file "jest.config.js")
        (node-application-config-file "application/jest.config.js"))
    (cond ((file-exists-p (concat project-root-dir node-application-config-file)) node-application-config-file)
           (t node-package-config-file))
    ))

(defun dtymon::kelpie-config-dir ()
  (let ((project-root-dir (dtymon::project-root-dir))
        (node-package-config-dir "./src/config")
        (node-application-config-dir "./application/src/config"))
    (cond ((file-exists-p (concat project-root-dir node-application-config-dir)) node-application-config-dir)
           (t node-package-config-dir))
    ))

(defun dtymon::get-windows-in-clockwise-order (&optional frame)
  "Return a list of windows in FRAME ordered clockwise around the centre."
  (let* ((windows (window-list frame 'no-minibuf frame))
         (win-centres
          (mapcar (lambda (win)
                    (let* ((edges (window-edges win))
                           (left (nth 0 edges))
                           (top (nth 1 edges))
                           (right (nth 2 edges))
                           (bottom (nth 3 edges))
                           (cx (/ (+ left right) 2.0))
                           (cy (/ (+ top bottom) 2.0)))
                      (list win cx cy)))
                  windows))
         ;; Calculate centre point of all windows
         (avg-x (/ (apply #'+ (mapcar (lambda (w) (nth 1 w)) win-centres)) (float (length win-centres))))
         (avg-y (/ (apply #'+ (mapcar (lambda (w) (nth 2 w)) win-centres)) (float (length win-centres)))))
    ;; Sort windows clockwise relative to centre (0° = up)
    (mapcar #'car
            (sort win-centres
                  (lambda (a b)
                    (let* ((ax (- (nth 1 a) avg-x))
                           (ay (- (nth 2 a) avg-y))
                           (bx (- (nth 1 b) avg-x))
                           (by (- (nth 2 b) avg-y))
                           (angle-a (mod (- (/ pi 2) (atan ay ax)) (* 2 pi)))
                           (angle-b (mod (- (/ pi 2) (atan by bx)) (* 2 pi))))
                      (> angle-a angle-b)))))))

(defun dtymon::rotate-window-buffers-clockwise ()
  "Rotate the buffers displayed in windows clockwise in the current frame."
  (interactive)
  (let* ((windows (dtymon::get-windows-in-clockwise-order))
         (buffers (mapcar #'window-buffer windows)))
    ;; Assign each buffer to the next window in the list
    (cl-loop for win in windows
             for buf in (append (last buffers) (butlast buffers))
             do (set-window-buffer win buf))))

(defun dtymon::rotate-window-buffers-anticlockwise ()
  "Rotate the buffers displayed in windows anticlockwise in the current frame."
  (interactive)
  (let* ((windows (reverse (dtymon::get-windows-in-clockwise-order)))
         (buffers (mapcar #'window-buffer windows)))
    ;; Assign each buffer to the next window in the list
    (cl-loop for win in windows
             for buf in (append (last buffers) (butlast buffers))
             do (set-window-buffer win buf))))

(provide 'setup-defuns)
