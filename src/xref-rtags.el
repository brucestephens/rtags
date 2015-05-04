(require 'rtags)

(defun xref-rtags-location (location)
  "Convert location passed in, returns descr file line column"
  (when (> (length location) 0)
    (cond ((string-match "\\(.*\\):\\([0-9]+\\):\\([0-9]+\\):?[[:space:]]*\\(.*\\)" location)
           (let ((file (match-string-no-properties 1 location))
		 (line (string-to-number (match-string-no-properties 2 location)))
                 (column (string-to-number (match-string-no-properties 3 location))))
             (list (match-string-no-properties 4 location) file line column)))
	  ((string-match "\\(.*\\):\\([0-9]+\\):?[[:space:]]*\\(.*\\)" location)
           (let ((file (match-string-no-properties 1 location))
		 (line (string-to-number (match-string-no-properties 2 location))))
             (list (match-string-no-properties 3 location) file line 0)))
	  (t nil))))

(defun xref-rtags-handle-results-buffer ()
  (setq rtags-last-request-not-indexed nil)
  (rtags-reset-bookmarks)
  (cond ((= (point-min) (point-max))
         (message "RTags: No results") nil)
        ((= (count-lines (point-min) (point-max)) 1)
         (let ((string (buffer-string)))
           (if (rtags-not-indexed/connected-message-p string)
               (progn
                 (setq rtags-last-request-not-indexed t)
                 nil)
             (let ((r (xref-rtags-location string)))
	       (list (xref-make (nth 0 r)
				(xref-make-file-location (nth 1 r)
							 (nth 2 r)
							 (nth 3 r))))))))
        (t
	 (with-current-buffer rtags-buffer-name
	   (goto-char (point-min))
	   (let (locs)
	     (while (not (eobp))
	       (let ((r (xref-rtags-location
			   (buffer-substring (point)
					     (progn (end-of-line) (point))))))
		 (setq locs (cons
			     (xref-make (nth 0 r)
					(xref-make-file-location (nth 1 r)
								 (nth 2 r)
								 (nth 3 r)))
			     locs)))
	       (forward-line 1))
	     (reverse locs))))))

(defun rtags--xref-find-definition (id)
  (let ((loc (xref-rtags-location id))
	(fn (buffer-file-name)))
    (cond (loc
	   (rtags-reparse-file-if-needed)
	   (with-current-buffer (rtags-get-buffer)
	     (rtags-call-rc :path fn :path-filter nil "-f" id)
	     (xref-rtags-handle-results-buffer))))))

(defun rtags--xref-find-references (id)
  (let ((loc (xref-rtags-location id))
	(fn (buffer-file-name)))
    (cond (loc
	   (rtags-reparse-file-if-needed)
	   (with-current-buffer (rtags-get-buffer)
	     (rtags-call-rc :path fn :path-filter nil "-r" id)
	     (xref-rtags-handle-results-buffer))))))

(defun xref-rtags-function (action id)
  (pcase action
    (`definitions (rtags--xref-find-definition id))
    (`references (rtags--xref-find-references id))
    (`apropos (rtags--xref-find-apropos id))))

(defun xref-rtags ()
  (make-variable-buffer-local 'xref-find-function)
  (make-variable-buffer-local 'xref-identifier-at-point-function)
  (make-variable-buffer-local 'xref-identifier-completion-table-function)
  (setq xref-identifier-at-point-function #'rtags-current-location)
  (setq xref-find-function #'xref-rtags-function)
  (setq xref-identifier-completion-table-function #'rtags-symbolname-complete))
  
