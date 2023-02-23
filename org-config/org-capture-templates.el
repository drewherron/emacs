(setq org-capture-templates
      '(

   ;; Generic inbox entries
   ("i" "Inbox" entry (file "capture.org")
    "* %^{Note} %^g\n:PROPERTIES:\n:CREATED:  %U\n:END:\n\n%?"
    :prepend t
    :created t
    )

   ;; Actions
   ("t" "Todo" entry (file "capture.org")
    "* TODO %^{Todo} %^g\n:PROPERTIES:\n:CREATED:  %U\n:END:\n\n%?"
    :prepend t
    :created t
    )

   ;; Reminders
   ("r" "Reminder" entry (file "tickler.org")
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
   ("p" "People" entry (file "people.org")
  "* %^{First} %^{Last}
:PROPERTIES:
:First:    %\\1
:Middle:
:Last:     %\\2
:Birthday: %^{Birth Date}u
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
