;; Org Capture

(setq org-default-notes-file (concat org-directory "~/inbox/capture.org"))

(global-set-key (kbd "C-c c") 'org-capture)

;;; Enables a good journal system with plain old org capture
(defun myqorg-capture-journal-file ()
  "Return the journal file path for the current year, creating the file if it does not exist."
  (let ((current-year (format-time-string "%Y")))
    (unless (file-exists-p (concat org-directory "/log/" current-year ".org"))
      (with-temp-buffer
        (insert (concat
                 "#+TITLE: " current-year "\n"
                 "#+OPTIONS: " "^:nil " "title:nil " "author:nil " "date:nil " "toc:nil " "H:4 " "num:nil " "\\n:t" "\n"
                 "#+LATEX: \\setcounter{secnumdepth}{0}\n"
                 "#+LATEX: \\setlength\\parindent{0pt}\n"
                 "#+LATEX_HEADER: \usepackage[margin=1in]{geometry}\n"
                 "\n"))
        (write-file (concat org-directory "/log/" current-year ".org"))))
    (concat org-directory "/log/" current-year ".org")))

;; Don't need
;(defun my/org-capture-journal-entry ()
;  (let ((entry-file (my/org-capture-journal-file)))
;    (with-current-buffer (find-file-noselect entry-file)
;      (goto-char (point-min))
;      (search-forward (format-time-string "%Y-%m-%d") nil t)
;      (org-capture-put-target-region-and-position)
;      (org-capture-place-entry))))

;;
(defun my/org-capture-find-datetree-location ()
  "Find or create the current date's location in the datetree within an Org buffer."
  (org-datetree-find-date-create (calendar-current-date)))

(setq org-capture-templates
      '(

   ;; Generic inbox entries
   ("c" "Inbox" entry (file "~/org/inbox/capture.org")
    "* %^{Note} %^g\n:PROPERTIES:\n:CREATED:  %U\n:END:\n\n%?"
    :prepend t
    :created t
    )

   ;; Actions
   ("t" "Todo" entry (file "~/org/inbox/capture.org")
    "* TODO %^{Todo} %^g\n:PROPERTIES:\n:CREATED:  %U\n:END:\n\n%?"
    :prepend t
    :created t
    )

   ;; Log
   ("l" "Log" entry
    (file+function (lambda () (my/org-capture-journal-file)) my/org-capture-find-datetree-location)
    "* %<%H:%M> %^{Title}\n%?" :empty-lines 1)

   ;; Reminders
   ("r" "Reminder" entry (file "~/org/gtd/tickler.org")
    "* %^{Reminder} %^g\nSCHEDULED:  %^t\n:PROPERTIES:\n:CREATED:  %U\n:END:\n\n%?"
    :prepend t
    :created t
    )

   ;; Shopping
   ("s" "Shopping" entry (file "~/Share/shopping.org")
    "* %^{Need} %^g\n:PROPERTIES:\n:CREATED:  %U\n:END:\n\n%?"
    :prepend t
    :created t
    )

   ;; People
   ("p" "People" entry (file "~/org/ref/people.org")
  "* %^{First} %^{Last}
:PROPERTIES:
:First:    %\\1
:Middle:
:Last:     %\\2
:Birthday: %^{Birth Date}t
:Phone:    %^{Phone}
:Email:    %^{Email}
:Website:
:Address:  %^{Address}
:City:     %^{City}
:State:    %^{State}
:Zip:      %^{Zip}
:Company:
:W-Title:
:W-Phone:
:W-Email:
:Github:
:Linkedin:
:END:

** gifts

** notes\n\n%?" :empty-lines 1)
   ))
