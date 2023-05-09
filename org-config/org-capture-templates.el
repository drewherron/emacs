(setq org-capture-templates
      '(

   ;; Generic inbox entries
   ("c" "Inbox" entry (file "~/org/gtd/capture.org")
    "* %^{Note} %^g\n:PROPERTIES:\n:CREATED:  %U\n:END:\n\n%?"
    :prepend t
    :created t
    )

   ;; Actions
   ("t" "Todo" entry (file "~/org/gtd/capture.org")
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
