(setq org-capture-templates
      '(

   ;; Inbox/generic capture
   ("i" "Inbox" entry (file "capture.org")
    "* %^{Note} %^g\n:PROPERTIES:\n:CREATED:  %U\n:END:\n\n%?"
    :prepend t
    :empty-lines 1
    :created t
    )

   ;;  Tickler file
   ("t" "Tickler" entry (file "tickler.org")
    "* %^{Reminder} %^g\nSCHEDULED:  %^t\n:PROPERTIES:\n:CREATED:  %U\n:END:\n\n%?"
    :prepend t
    :empty-lines 1
    :created t
    )


   ;; Shopping
   ("s" "Shopping" entry (file "~/Share/shopping.org")
    "* %^{Need} %^g\n:PROPERTIES:\n:CREATED:  %U\n:END:\n\n%?"
    :prepend t
    :empty-lines 1
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
:CREATED:  %U
:END:

** Wish List

** Notes\n\n%?" :empty-lines 1)
   ))
