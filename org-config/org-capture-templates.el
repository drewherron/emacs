(setq org-capture-templates
      '(

   ;; Inbox/generic capture
   ("i" "Inbox" entry (file "capture.org")
    "* %^{Note} %^g \n:PROPERTIES:\n:CREATED:  %U\n:END:\n%?"
    :prepend t
    :empty-lines 1
    :created t
    )

   ;;  Tickler file
   ("r" "Reminder" entry (file "tickler.org")
    "* %^{Reminder} %^g \n:PROPERTIES:\n:CREATED:  %U\n:SCHEDULED:  %^t\n:END:\n%?"
    :prepend t
    :empty-lines 1
    :created t
    )


   ;; Shopping
   ("s" "Shopping" entry (file "~/Share/shopping.org")
    "* %^{Need} %^g \n:PROPERTIES:\n:CREATED:  %U\n:END:\n%?"
    :prepend t
    :empty-lines 1
    :created t
    )

   ;; People
   ("p" "People" entry (file "capture.org")
    "* %^{First} %^{Last}%? %?\nCREATED: %u
    
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
  :Map:      [[google-maps:%\\5+%\\6+%\\7+%\\8][Google Maps]]
  :Company:
  :W-Group:
  :W-Title:
  :W-Phone:
  :W-Email:
  :W-Website:
  :W-Address:
  :W-Office:
  :W-City:
  :W-State:
  :W-Zip:
  :W-Map:
  :Facebook:
  :Github:
  :Instagram:
  :Linkedin:
  :Via:
  :Note:
  :END:

** Wish List

** TODO Wish %\\1 %\\2 a Happy Birthday
    DEADLINE: %^{Birthday}t
    :PROPERTIES:
    :Via:
    :Note:
    :END:" :empty-lines 1)


   ))

;; Add ID automatically on capture
;(add-hook 'org-capture-prepare-finalize-hook 'org-id-store-link)
