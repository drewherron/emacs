;;; faith.el --- hepls spreading the true faith
;; Time-stamp: <2003-08-19 13:38:28 deego>
;; GPL'ed under GNU'S public license..
;; Copyright (C) Deepak Goel 2000
;; Emacs Lisp Archive entry
;; Filename: faith.el
;; Author: Deepak Goel <deego@glue.umd.edu>
;; Version: 1.9

(defconst faith-version "2.0"
  "Version number of faith.el")

;; This file is not (yet) part of GNU Emacs.

;; WEBSITE: http://www.glue.umd.edu/~deego/emacspub/faith/
;; for this file and for associated READMEs LOGFILEs etc..

;;; Copyright (C) Deepak Goel
;; AUTHORS: Deepak Goel (deego@glue.umd.edu) ,
;; Robert Fenk <Robert.Fenk@gmx.de>,
;; Roberto Selbach Teixeira <teixeira@conectiva.com>
;; Remi Vanicat<vanicat@labri.u-bordeaux.fr>

;; YOU ARE VERY WELCOME TO CONTRIBUTE TO FAITH. YOUR SUGGESTIONS OR
;; CONTRIBUTIONS OR CORRECTIONS WILL BE CONSIDERED VERY FAVORABLY,
;; AND WILL PROVE YOUR UTMOST DEVOTION TO HIM. Even minor
;; contributions to this holy work will earn you a name on the list
;; of authors.

;; If you have been invited to become priest (author) of faith,
;; please send deego@glue.umd.edu an email agreeing to accept the
;; "GNU FREEness" of faith, and agreeing that if at any point in
;; future, you don't agree to sign the appropriate copyleft
;; agreement, deego@glue.umd.edu will remove you from the author's
;; list. You will be promptly listed as an author.

;; Commentary: In this world of infidelity and blasphemy,
;; FAITH tries to reinforce faith in you.

;;; QUICKSTART INSTALLATION FOR THOSE LOST:
;;; Drop faith.el somewhere in yr load-path, and add to your .emacs:
;;; (load "faith.el") 
;;;  then type M-x faith, and enjoy..


;;; FAITH 2.0


(defconst faith-version "1.9"
  "Version number of faith.el")

;;; Code:
(defvar faith-user-quotes nil
  "*These are any additional quotes a user might like included.")

(defvar faith-quotes-separator "\n__________________________\n\n"
  "*The string which is inserted before a quote.")

(defvar faith-replacement-strings nil
  "True Replacements for bad Gods and other words.
Each replacement is a list of BADLIST and GOODLIST. All matches from BADLIST will be replaced by a random word from GOODLIST. For consistency, the random word chosen will be the same for the entire quote.")

(defvar faith-user-before-replacement-strings nil
  "Will be appended before faith-replacement-strings to allow user-defined replacements.")

(defvar faith-user-after-replacement-strings nil
  "Will be appended after faith-replacement-strings for additional user-defined replacements.")

(defvar faith-fill-column 70
  "Default fill column for faith quotes.")

;;;###autoload
(defun faith-insert (&rest args)
  "Insert a quote right here, right now, in the current buffer."
  (interactive)
  (insert (apply 'faith-quote args)))

;;;###autoload
(defun faith ()
  "Switch to buffer *faith* and insert faith-snippets there."
  (interactive)
  (unless (equal (buffer-name) "*faith*")
    (switch-to-buffer "*faith*")
    (get-buffer-create "*faith*"))
  (let ((go-this-time t))
    (while go-this-time
      (goto-char (point-max))
      (insert faith-quotes-separator (faith-quote))
      (goto-char (point-max))
      (recenter)
      (setq fill-column faith-fill-column)
      (fill-paragraph nil)
      (unless (y-or-n-p "Care for more wise words? ")
        (setq go-this-time nil))))
  (message "Use M-x faith-correct on your own documents to correct them."))

;;;###autoload
(defun faith-quote (&optional quotes leave-alone-p)
  "Return a randomly chosen snippet, optionally correcting it for faith."
  (let* ((init-quote (nth (random (length (or quotes (append faith-false-quotes faith-user-quotes))))
                          (or quotes (append faith-false-quotes faith-user-quotes))))
         (final-quote (if leave-alone-p init-quote (faith-correct-string init-quote)))
         (justified-quote (faith-justify-string final-quote)))
    (if (called-interactively-p 'any)
        (message justified-quote)
      justified-quote)))

;;;###autoload
(defun faith-correct-buffer ()
  "Replace false gods with the ONE TRUE GOD in the current buffer."
  (interactive)
  (let ((case-fold-search t))
    (mapc (lambda (froms-tos)
            (let ((tos (cadr froms-tos)))
              (mapc (lambda (from)
                      (goto-char (point-min))
                      (while (search-forward-regexp (concat "\\b" from "\\b") nil t)
                        (replace-match (nth (random (length tos)) tos) nil nil)))
                    (car froms-tos))))
          (append faith-user-before-replacement-strings
                  faith-replacement-strings
                  faith-user-after-replacement-strings))))

;;;###autoload
(defun faith-correct-region (b e)
  "Replace false gods with the ONE TRUE GOD in the region."
  (interactive "r")
  (save-restriction
    (narrow-to-region b e)
    (faith-correct-buffer)))

;;;###autoload
(defun faith-correct-string (snippet)
  "Correct a SNIPPET, replacing false gods with the ONE TRUE GOD."
  (with-temp-buffer
    (insert snippet)
    (faith-correct-buffer)
    (buffer-string)))

(defun faith-justify-string (string)
  "Justify STRING within a temp buffer."
  (with-temp-buffer
    (insert string)
    (fill-paragraph nil)
    (buffer-string)))

(defun faith-load-quotes (file-path)
  "Load quotes from a file into `faith-false-quotes`."
  (setq faith-false-quotes nil) ;; Clear existing quotes
  (when (file-readable-p file-path)
    (with-temp-buffer
      (insert-file-contents file-path)
      (goto-char (point-min))
      (while (not (eobp))
        (let ((line (buffer-substring-no-properties (line-beginning-position)
                                                    (line-end-position))))
          (unless (string-match-p "^\\s-*$" line) ;; Ignore empty lines
            (push line faith-false-quotes)))
        (forward-line 1)))))

(let ((quotes-file (expand-file-name "quotes.txt"
     (file-name-directory (or load-file-name buffer-file-name)))))
  (faith-load-quotes quotes-file))



(provide 'faith)
;;; faith.el ends here
