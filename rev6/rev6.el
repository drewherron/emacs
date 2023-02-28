
;;; rev6.el --- Four movement keys, vim-style

;; Copyright (C) 2023 Drew Herron

;; Author: Drew Herron <herron@mailbox.org>
;; Version: 1.0
;; Package-Requires: ?
;; Keywords: 
;; URL: https://www.github.com/drewherron

;;; Commentary:

;-----;-------------------;--------------;-------------------;------------------;
;     ;       h           ;      j       ;        k          ;       l          ;
;-----;-------------------;--------------;-------------------;------------------;
;     ;  (backward-char)  ;  (next-line  ;  (previous-line)  ;  (forward-char)  ;
;-----;-------------------;--------------;-----------;-----------;
;  S  ; (move-beginning   ; (scroll-down ;           ;
;     ;  -of line)        ;  -command    ;           ;           ;
;-----;-------------------;--------------;-----------;-----------;
;  M  ;   (backward       ;              ;           ;           ;
;     ;    -sentence)      ;           ;           ;           ;
;-----;-----------;-----------;-----------;-----------;
;  C  ;           ;           ;           ;           ;
;     ;           ;           ;           ;           ;
;-----;-----------;-----------;-----------;-----------;

windmove-up
windmove-down
windmove-left
windmove-right



;hjkl: movement
; +S:  home, pgdn, pgup, end
; +M:  Begin sentence, end section, etc
; +C:  Windows
; +MC: Buffers

;;; Code:

(windmove-default-keybindings)



;; code goes here

(provide 'test)
;;; test.el ends here
