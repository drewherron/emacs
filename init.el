;;===========================;;
;;  This Is MY emacs config  ;;
;;  There are many like it   ;;
;;  But this one is mine     ;;
;;  Below this block will go ;;
;;  only things I understand ;;
;;===========================;;

;; Don't show the splash screen
; I'll use this later:
;(setq inhibit-startup-message t)

;;==========;;
;; Packages ;;
;;==========;;

(require 'package)

(if (and (= emacs-major-version 26)
         (<= emacs-minor-version 1))
    (progn (message "Fixing GNU TLS algorithm")
           (setq gnutls-algorithm-priority "NORMAL:-VERS-TLS1.3")))


(setq package-archives '(("org" . "https://orgmode.org/elpa/")
                         ("melpa" . "https://melpa.org/packages/")
                         ("gnu" . "https://elpa.gnu.org/packages/")))

(package-initialize)
;; Uncomment when you're updating this file less frequently
;;(package-refresh-contents)

;;==========;;
;; Files    ;;
;;==========;;

;; Load config files
(load "~/.config/emacs/org-config/org-capture-templates")

;;===========;;
;; Evil Mode ;;
;;===========;;

;; Download Evil
(unless (package-installed-p 'evil)
  (package-install 'evil))

;; Enable Evil
(require 'evil)
(evil-mode 1)

;;=============;;
;; Environment ;;
;;=============;;

(menu-bar-mode -1)
(tool-bar-mode -1)
(scroll-bar-mode -1)
(set-fringe-mode 10)

;;;;;;;;;;;;;;;;;;;;;;;;;;; re-org by M-x customize groups?

(global-display-line-numbers-mode)

;; Themes
(add-to-list 'custom-theme-load-path "~/.config/emacs/themes")
(load-theme 'aldalome t)

;; Fix colors in st
(add-to-list 'term-file-aliases
	         '("st-256color" . "xterm-256color"))

;; set default font
(set-face-attribute 'default nil
                    :family "Cascadia Mono"
                    :height 140
                    :weight 'normal
                    :width 'normal)

;;==========;;
;; Org Mode ;;
;;==========;;

(require 'org)
(define-key org-mode-map (kbd "RET") nil)

(global-set-key (kbd "C-c a") 'org-agenda)

(setq org-todo-keywords
	'((sequence "TODO" "NEXT" "DONE")))

(setq org-log-done 'time)

(setq org-directory "~/org/")

(setq org-refile-targets '((org-agenda-files :maxlevel . 2)))

;; Org Capture
(setq org-default-notes-file (concat org-directory "~/capture.org"))

(global-set-key (kbd "C-c c") 'org-capture)

;; Org Agenda
(setq org-agenda-files (list "~/org/"))

;; Org Journal
(require 'org-journal)

(setq org-journal-dir (concat org-directory "journal/"))

;; Calendar stuff
;(eval-after-load "calendar"
;  `(progn
;     (define-key calendar-mode-map (kbd "RET") 'org-journal-display-entry)
;    ; (define-key calendar-mode-map (kbd "M-[") 'calendar-backward-month)
;     ))
(define-key calendar-mode-map (kbd "RET") 'org-journal-display-entry)

;; LaTeX
(plist-put org-format-latex-options :scale 2)

