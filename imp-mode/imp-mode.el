
;;; rev6.el --- Four movement keys, vim-style

;; Copyright (C) 2023 Drew Herron

;; Author: Drew Herron <herron@mailbox.org>
;; Version: 1.0
;; Package-Requires: ?
;; Keywords: 
;; URL: https://www.github.com/drewherron

;;; Commentary:

;; nameofsomekey enters imp-mode (ESC by default)
;; nameofotherkey leaves imp-mode (; by default)

;;|   | h                        | j                   | k                     | l                  |
;;|---|--------------------------|---------------------|-----------------------|--------------------|
;;|   | (backward-char)          | (next-line)         | (previous-line)       | (forward-char)     |
;;| S | (move-beginning-of-line) | (scroll-up-command) | (scroll-down-command) | (move-end-of-line) |
;;| M | (backward-sentence)      | (forward-paragraph) | (backward-paragraph)  | (forward-sentence) |
;;| C | (windmove-left)          | (next-buffer)       | (previous-buffer)     | (windmove-right)   |


Note to myself:
It may be annoying to go in and out of this mode all the time.
You may want to add selection and movement, copy paste, etc,
maybe just the emacs commands without ctrl, or.... something
Just an idea
What would those be?
space: (set-mark-command) or (push-mark)
w
y
k (in Colemak)
Compare god mode keybindings for more..
WAIT
Think about C-d and M-d
So, keep Ctrl and Meta,
I'm using mnei for movement
Ctrl/Meta e doesn't matter because I'm rebinding it in that same space.
What else is there?
C-m RET - worthless
M-m back-to-indentation - first non-whitespace char on line
C-M-m undefined
C-n next-line               no prob 
M-n undefined               no probM
C-M-n forward-list
C-e move-end-of-line        no prob
M-e forward-sentence        no prob
C-M-e end-of-defun
C-i indent-for-tab          
M-i tab-to-tab-stop - should be... on TAB?
C-M-i completion-at-point

hmmmm

Woah what about... don't have an imp-mode???
Instead of a mode, keep mods (more like emacs)
Can't have shift though.
Ctrl    left down up right 
Meta    sentences and regions
C+M     windows and buffers 

;;; Code:

(windmove-default-keybindings)



;; code goes here

(provide 'test)
;;; test.el ends here
