
;;; imp-mode.el --- Four movement keys, four ways

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



;;; Code:

;; This is a TEST



(provide 'imp-mode)
;;; imp-mode.el ends here
