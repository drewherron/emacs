
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

; Doesn't work...
;(global-set-key (kbd "H-h") 'left-char)
;(global-set-key (kbd "H-j") 'next-line)
;(global-set-key (kbd "H-k") 'previous-line)
;(global-set-key (kbd "H-l") 'right-char)
;
;(defvar my-hyper-map
;  (let ((map (make-sparse-keymap)))
;    (define-key map (kbd "h") 'move-beginning-of-line)
;    (define-key map (kbd "j") 'scroll-up-command)
;    (define-key map (kbd "k") 'scroll-down-command)
;    (define-key map (kbd "l") 'move-end-of-line)
;    map))
;
;(defun my-hyper-mode ()
;  (interactive)
;  (let ((in-map t)
;        (keymap (current-global-map)))
;    (set-keymap-parent my-hyper-map keymap)
;    (while in-map
;      (let ((event (read-key)))
;        (if (eq event (kbd ";"))
;            (setq in-map nil)
;          (setq keymap (key-binding event))
;          (if (eq keymap 'my-hyper-mode)
;              (setq keymap my-hyper-map))
;          (setq unread-command-events (list event)))))))
;
;(define-key global-map (kbd "H-a") 'my-hyper-mode)

; Doesn't work...
;(defvar my-hyper-map (make-sparse-keymap))
;(define-key my-hyper-map (kbd "h") 'backward-char)
;(define-key my-hyper-map (kbd "j") 'next-line)
;(define-key my-hyper-map (kbd "k") 'previous-line)
;(define-key my-hyper-map (kbd "l") 'forward-char)
;
;(defun my-enter-hyper-mode ()
;  (interactive)
;  (set-transient-map my-hyper-map t)
;  (message "Entered Hyper mode. Press ';' to exit."))
;
;(defun my-exit-hyper-mode ()
;  (interactive)
;  (message "Exited Hyper mode.")
;  (set-transient-map nil))
;
;(global-set-key (kbd "<H-a>") #'my-enter-hyper-mode)
;(define-key my-hyper-map (kbd ";") #'my-exit-hyper-mode)

(defvar imp-mode-map nil "Keymap for evg-status page")

(progn
  (setq imp-mode-map (make-sparse-keymap))
  (setq imp-file-mode-map (make-sparse-keymap))


  (define-key imp-mode-map (kbd "b") 'mdb/evg-show-all-patches)
  (define-key imp-mode-map (kbd "o") 'mdb/evg-open-patch-at-point)
  
  (define-key imp-file-mode-map (kbd "<RET>") 'mdb/evg-show-file-at-point)
  (define-key imp-file-mode-map (kbd "q") 'kill-this-buffer)
  )

(define-derived-mode
  imp-mode
  fundamental-mode
  "Imp"
  "Major mode for impy tests")


(global-set-key (kbd "<H-a>") #'imp-mode)

(provide 'imp-mode)
;;; imp-mode.el ends here
