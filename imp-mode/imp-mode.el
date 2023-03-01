
;;; rev6.el --- Four movement keys, vim-style

;; Copyright (C) 2023 Drew Herron

;; Author: Drew Herron <herron@mailbox.org>
;; Version: 1.0
;; Package-Requires: ?
;; Keywords: 
;; URL: https://www.github.com/drewherron

;;; Commentary:

;;|   | h                        | j                   | k                     | l                  |
;;|---|--------------------------|---------------------|-----------------------|--------------------|
;;|   | (backward-char)          | (next-line)         | (previous-line)       | (forward-char)     |
;;| S | (move-beginning-of-line) | (scroll-up-command) | (scroll-down-command) | (move-end-of-line) |
;;| M | (backward-sentence)      | (forward-paragraph) | (backward-paragraph)  | (forward-sentence) |
;;| C | (windmove-left)          | (next-buffer)       | (previous-buffer)     | (windmove-right)   |
 

;;; Code:

(windmove-default-keybindings)



;; code goes here

(provide 'test)
;;; test.el ends here
