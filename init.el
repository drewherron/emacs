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
(package-refresh-contents)

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

(unless (display-graphic-p)
   (menu-bar-mode -1))

;;;;;;;;;;;;;;;;;;;;;;;;;;; re-org by M-x customize groups

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
                    :height 110
                    :weight 'normal
                    :width 'normal)

;;==========;;
;; God Mode ;;
;;==========;;

;(setq god-mode-enable-function-key-translation nil)
;(require 'god-mode)
;(god-mode)
;(global-set-key (kbd "<escape>") #'god-mode-all)
;(setq god-exempt-major-modes nil)
;(setq god-exempt-predicates nil)



;;==========;;
;; Org Mode ;;
;;==========;;

(require 'org)

(setq org-todo-keywords
	'((sequence "TODO" "IN-PROGRESS" "WAITING" "DONE")))
;; Latin? Pensum, Pendens, Finitus

;; Org Agenda
(setq org-agenda-files '("~/Documents/Notes/Capture.org"
                         "~/Documents/Notes/Projects.org"
                         "~/Documents/Notes/Tickler.org"))
;; Org Capture
(setq org-default-notes-file (concat org-directory "/notes.org"))

;;(setq org-format-latex-options (plist-put org-format-latex-options :scale 2.0))

;;(after! org (plist-put org-format-latex-options :scale 1.75))

;;(plist-put org-format-latex-options :scale 2)


(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages '(evil ## tron-legacy-theme night-owl-theme base16-theme))
 '(scroll-bar-mode nil))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
