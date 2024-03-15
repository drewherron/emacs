(require 'json)

(defconst random-quote-version "1.0"
  "Version number of random-quote-json.el")

(defvar quotes-list nil
  "List of quotes loaded from a JSON file.")

(defvar quotes-json-file-path "~/Sync/quotes"
  "Path to the JSON file containing quotes.")

;;;###autoload
(defun insert-random-quote ()
  "Insert a random quote from `quotes-list` in 'Quote' - Author format into the current buffer."
  (interactive)
  (if quotes-list
      (let* ((quote-entry (nth (random (length quotes-list)) quotes-list))
             (quote-text (gethash "quote" quote-entry))
             (author (gethash "author" quote-entry))
             (formatted-quote (format "\"%s\"\n- %s" quote-text author)))
        (insert formatted-quote))
    (message "No quotes loaded. Please load quotes with `load-quotes-from-json-file`.")))

;;;###autoload
(defun load-quotes-from-json-file (file-path)
  "Load quotes from a JSON file into `quotes-list`."
  (interactive "fPath to JSON file: ")
  (setq quotes-list nil) ;; Clear existing quotes
  (when (file-readable-p file-path)
    (with-temp-buffer
      (insert-file-contents file-path)
      (setq quotes-list (json-parse-buffer :object-type 'hash-table :array-type 'list :null-object nil :false-object nil))
      (setq quotes-list (gethash "quotes" quotes-list))))
  (message "Loaded %d quotes." (length quotes-list)))

;;;###autoload
(defun display-random-quote ()
  "Display a random quote from `quotes-list` in 'Quote' - Author format."
  (interactive)
  (if quotes-list
      (let* ((quote-entry (nth (random (length quotes-list)) quotes-list))
             (quote-text (gethash "quote" quote-entry))
             (author (gethash "author" quote-entry)))
        (message "\"%s\"\n- %s" quote-text author))
    (message "No quotes loaded. Please load quotes with `load-quotes-from-json-file`.")))

(load-quotes-from-json-file quotes-json-file-path)

(provide 'random-quote)
;;; random-quote.el ends here
