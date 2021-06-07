;;; init.el --- my own emacs init.el file

;; --- running emacsclient on uhura ---
(if (string-equal (system-name) "uhura")
    (server-start))

;;; Commentary:
;;
;;; Initialize:
;;; Code:

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(ansi-color-faces-vector
   [default default default italic underline success warning error])
 '(ansi-color-names-vector
   ["black" "red3" "ForestGreen" "yellow3" "blue" "magenta3" "DeepSkyBlue" "gray50"])
 '(column-number-mode t)
 '(current-language-environment "UTF-8")
 '(custom-enabled-themes '(wb2))
 '(custom-safe-themes
   '("0d556a058bd9af9dbe18fc2678830a75693f51b6b8be74ebff3cf7e8875a1272" "2a406e90ddfee681c9302303d76faaabb57a3affffb7adf3472e84115b609b27" "1feebe84eeee8f7f94b590b5109aa3aefd3d72cea5c35aa6c9636feec0bb6201" "fcfb725e0908cc1a7921fddfce8cf6835a811e5b685671a62ddc76c522723f14"))
 '(easy-hugo-bin "/home/niclas/bin/hugo")
 '(inhibit-startup-screen t)
 '(package-selected-packages
   '(magit edit-server auctex dockerfile-mode docker sphinx-doc easy-hugo company-box visual-fill-column org org-bullets sr-speedbar elpy flycheck blacken 2048-game which-key try use-package pandoc-mode pandoc markdown-mode))
 '(show-paren-mode t)
 '(text-mode-hook '(turn-on-auto-fill text-mode-hook-identify))
 '(tool-bar-mode nil)
 '(tooltip-mode nil))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(default ((t (:family "Consolas" :foundry "MS  " :slant normal :weight normal :height 90 :width normal)))))

;; ---------------
;; --- my personal settings
;; ---------------

;; --- satter lite kortbindningar --------------------------
;; ---------------
(global-set-key "\M-1" 'other-window)
(global-set-key "\C-cq" 'query-replace)
(global-set-key "\C-ca" 'mark-whole-buffer)
(global-set-key "\C-c;" 'comment-region)
(global-set-key "\C-cw" 'ispell-word)
(global-set-key "\C-ci" 'indent-region)
(global-set-key "\C-cl" 'count-lines-region)
(global-set-key "\C-cd" 'delete-trailing-whitespace)
(global-set-key "\C-cp" 'org-toggle-inline-images)


;; opens the todo list in a new buffer ---------------------
(defun nst-open-todolist ()
  (interactive)
  (find-file "~/ownCloud/docs/todoStuff.org"))
(global-set-key "\C-ct" 'nst-open-todolist)


;; --- tyst *scratch* buffer och inget pling ---------------
(setq initial-scratch-message "")
(setq ring-bell-function 'ignore)

;; --- oh! the blank spaces! -------------------------------
(add-hook 'before-save-hook
          'delete-trailing-whitespace)

;; --- melpa packages settings -----------------------------
;; ----------------
(require 'package)
;; (setq package-enable-at-startup nil)
(setq package-archives '(("melpa" . "https://melpa.org/packages/")
                         ("org" . "https://orgmode.org/elpa/")
                         ("elpa" . "https://elpa.gnu.org/packages/")))

(package-initialize)

(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))


;; --- the packages

;; --- simple stuff ----------------------------------------
(use-package try
  :ensure t)

(use-package which-key
  :ensure t
  :config (which-key-mode))


;; --- python help stuff -----------------------------------
(use-package elpy
  :ensure t
  :init
  (elpy-enable)
  (when (require 'flycheck nil t)
  (setq elpy-modules (delq 'elpy-module-flymake elpy-modules))
  (add-hook 'elpy-mode-hook 'flycheck-mode))
  :custom
  (python-shell-interpreter "python3"))


(add-hook 'python-mode-hook (lambda ()
                                  (require 'sphinx-doc)
                                  (sphinx-doc-mode t)))


;; --- markdown and pandoc ---------------------------------
(use-package markdown-mode
  :ensure t
  :mode (("README\\.md\\'" . gfm-mode)
         ("\\.md\\'" . markdown-mode)
         ("\\.markdown\\'" . markdown-mode))
  :init (setq markdown-command "multimarkdown"))
(add-hook 'markdown-mode-hook 'pandoc-mode)

(defun nst-disable-company-mode()
  (company-mode -1))
(add-hook 'markdown-mode-hook 'nst-disable-company-mode)


(use-package pandoc
  :ensure t)
(use-package pandoc-mode
  :ensure t)


;; --- the fantastic speedbar ------------------------------
(use-package sr-speedbar
  :ensure t)
;;(require 'sr-speedbar)
(global-set-key "\C-cs" 'sr-speedbar-toggle)
(setq speedbar-use-images nil)
(setq sr-speedbar-refresh-turn-on t)


;; --- hugo!! ----------------------------------------------
(use-package easy-hugo
  :ensure t
  :init
  (setq easy-hugo-basedir "/home/niclas/hugo/blog/")
  (setq easy-hugo-url "https://blog.nist.se")
  (setq easy-hugo-sshdomain "prime2.inleed.net")
  (setq easy-hugo-root "public_html/blog/")
  (setq easy-hugo-previewtime "300")
  :bind ("C-c C-e" . easy-hugo))

;; deploying hugo blog post
(defun easy-hugo-deploy-site ()
  "Runs the deploy command found in ~/bin."
  (interactive)
  (shell-command "deploy"))
;; (add-hook 'markdown-mode
;; 	  (lambda () (local-set-key "\C-ce"
;; 	  'easy-hugo-deploy-site)))
(global-set-key "\C-ce" 'easy-hugo-deploy-site)


;; --- auctex stuff ----------------------------------------
(use-package tex
  :defer t
  :ensure auctex
  :config
  (setq TeX-auto-save t))

;; ---------------
;; --- docker stuff ----------------------------------------
;; ---------------
(use-package dockerfile-mode
  :ensure t)


;; --- magit ---
(use-package magit
  :ensure t)


;; ----------------
;; Org Mode Configuration ----------------------------------
;; ----------------

(defun ns/org-mode-setup ()
  (org-indent-mode)
  ;; (variable-pitch-mode 1)
  (visual-line-mode 1)
  )

;; (defun efs/org-font-setup ()
;;   ;; Replace list hyphen with dot
;;   (font-lock-add-keywords 'org-mode
;;                           '(("^ *\\([-]\\) "
;;                              (0 (prog1 () (compose-region (match-beginning 1) (match-end 1) "•"))))))

  ;; Set faces for heading levels
  (dolist (face '((org-level-1 . 1.2)
                  (org-level-2 . 1.1)
                  (org-level-3 . 1.05)
                  (org-level-4 . 1.0)
                  (org-level-5 . 1.1)
                  (org-level-6 . 1.1)
                  (org-level-7 . 1.1)
                  (org-level-8 . 1.1)))
    ;; (set-face-attribute (car face) nil :font "Cantarell" :weight 'regular :height (cdr face))
    )

  ;; ;; Ensure that anything that should be fixed-pitch in Org files appears that way
  ;; (set-face-attribute 'org-block nil :foreground nil :inherit 'fixed-pitch)
  ;; (set-face-attribute 'org-code nil   :inherit '(shadow fixed-pitch))
  ;; (set-face-attribute 'org-table nil   :inherit '(shadow fixed-pitch))
  ;; (set-face-attribute 'org-verbatim nil :inherit '(shadow fixed-pitch))
  ;; (set-face-attribute 'org-special-keyword nil :inherit '(font-lock-comment-face fixed-pitch))
  ;; (set-face-attribute 'org-meta-line nil :inherit '(font-lock-comment-face fixed-pitch))
  ;; (set-face-attribute 'org-checkbox nil :inherit 'fixed-pitch))

(use-package org
  :hook (org-mode . ns/org-mode-setup)
  :config
  (setq org-ellipsis " ▾")
  (setq org-todo-keywords
  '((sequence "TODO" "DOING" "DONE")))
  (setq org-todo-keyword-faces
 '(("TODO" . "red3") ("DOING" . "magenta3") ("DONE" . "green3")))
   ;; (efs/org-font-setup)
)

(use-package org-bullets
  :ensure t
  :after org
  :hook (org-mode . org-bullets-mode)
  :custom
  (org-bullets-bullet-list '("◉" "○" "●" "○" "●" "○" "●")))


(defun ns/org-mode-visual-fill ()
  (setq visual-fill-column-width 100
        visual-fill-column-center-text t)
  (visual-fill-column-mode 1))


(use-package visual-fill-column
  :ensure t
  :hook ((org-mode . ns/org-mode-visual-fill)
        (markdown-mode . ns/org-mode-visual-fill))
  )



;; --- org-eq-viewer ---
;; -setting latex font size in org eq viewing ---
(setq my-org-latex-preview-scale 1.5)

; plist-put is maybe-destructive, weird. So, we have to restore old value ourselves
(defun org-latex-preview-advice (orig-func &rest args)
  (let ((old-val (copy-tree org-format-latex-options)))
    (setq org-format-latex-options (plist-put org-format-latex-options
                                              :scale
                                              (* my-org-latex-preview-scale (expt text-scale-mode-step text-scale-mode-amount))))
    (apply orig-func args)
    (setq org-format-latex-options old-val)))
(advice-add 'org-latex-preview :around #'org-latex-preview-advice)

;; --- END org-mode

;; --- company stuff ---------------------------------------
(use-package company
  :after elpy-mode
  :hook (elpy-mode . company-mode)
  :bind (:map company-active-map
         ("<tab>" . company-complete-selection))
        (:map elpy-mode-map
         ("<tab>" . company-indent-or-complete-common))
  :custom
  (company-minimum-prefix-length 1)
  (company-idle-delay 0.0)
  )


;; ;; --- some colors for company mode when dark
;; (require 'color)

;; (custom-theme-set-variables
;;  ;; company-mode
;;  ;;
;;  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;  '(company-tooltip                  ((t (:inherit nil :background "grey23" :foreground "white")))))


 ;; (let ((bg (face-attribute 'default :background)))
 ;;    (custom-set-faces
 ;;     `(company-tooltip ((t (:inherit default :background ,(color-lighten-name bg 2)))))
 ;;     `(company-scrollbar-bg ((t (:background ,(color-lighten-name bg 10)))))
 ;;     `(company-scrollbar-fg ((t (:background ,(color-lighten-name bg 5)))))
 ;;     `(company-tooltip-selection ((t (:inherit font-lock-function-name-face))))
 ;;     `(company-tooltip-common ((t (:inherit font-lock-constant-face))))))

;; company mode global
(add-hook 'after-init-hook 'global-company-mode)
(global-company-mode t)
(setq company-minimum-prefix-length 1)
(setq company-idle-delay 0)
;; company quick-help
;; (company-quickhelp-mode 1)
;; (setq company-quickhelp-delay 0)


; Use tab key to cycle through suggestions.
; ('tng' means 'tab and go')
; (company-tng-configure-default)
;; ---

;; (use-package company-box
;;   :hook (company-mode . company-box-mode))



(use-package edit-server
  :ensure t
  :commands edit-server-start
  :init (if after-init-time
              (edit-server-start)
            (add-hook 'after-init-hook
                      #'(lambda() (edit-server-start))))
  :config (setq edit-server-new-frame-alist
                '((name . "Edit with Emacs FRAME")
                  (top . 200)
                  (left . 200)
                  (width . 80)
                  (height . 25)
                  (minibuffer . t)
                  (menu-bar-lines . t)
                  (window-system . x))))

;; ----------------
;; --- init for lisp-files in ~/.emacs.d/lisp --------------
;; ----------------
(add-to-list 'load-path "/home/niclas/.emacs.d/lisp/")

;; --- abaqus ---
(autoload 'abaqus-mode "abaqus" "Enter abaqus mode." t)
(setq auto-mode-alist (cons '("\\.inp\\'" . abaqus-mode) auto-mode-alist))
(add-hook 'abaqus-mode-hook 'font-lock-mode)

;; --- lsdyna ---
(autoload 'lsdyna-mode "lsdyna" "Enter ls-dyna mode." t)
(setq auto-mode-alist (cons '("\\.k\\'" . lsdyna-mode) auto-mode-alist))
(add-hook 'lsdyna-mode-hook 'font-lock-mode)


;; org-present ---

(defun org-present-preview-latex ()
  "Shows equations as latex preview"
  (interactive)
  (custom-set-variables
   '(org-format-latex-options
     (quote
      (:foreground default
                 :background default
                 :scale 2.0
                 :html-foreground "Black"
                 :html-background "Transparent"
                 :html-scale 3.0
                 :matchers
                 ("begin" "$1" "$" "$$" "\\(" "\\[")))))
  (org-toggle-latex-fragment)
 )

(autoload 'org-present "org-present" nil t)
(eval-after-load "org-present"
  '(progn
     (add-hook 'org-present-mode-hook
               (lambda ()
                 (org-present-big)
                 (org-display-inline-images)
                 (org-present-hide-cursor)
                 (org-present-read-only)
                 (org-present-preview-latex)
                 ))
     (add-hook 'org-present-mode-quit-hook
               (lambda ()
                 (org-present-small)
                 (org-remove-inline-images)
                 (org-present-show-cursor)
                 (org-present-read-write)))))


;; --- disable mouse clicking on laptop ---
(require 'disable-mouse)
(if (string-equal (system-name) "niclap")
    (global-disable-mouse-mode))


;; --- make octave to use matlab.el ---
(setq auto-mode-alist (remq '("\\.m\\'" . objc-mode) auto-mode-alist))
(autoload 'matlab-mode "matlab" "Enter MATLAB mode." t)
(setq auto-mode-alist (cons '("\\.m\\'" . matlab-mode) auto-mode-alist))
(autoload 'matlab-shell "matlab" "Interactive MATLAB mode." t)
( setq matlab-shell-command "octave_in_emacs"
       shell-command-echoes nil)
(setq matlab-shell-command-switches '("--no-gui"))
(setq matlab-shell-buffer-name "octave")

;; (defalias 'octave-shell 'matlab-shell)
(setq matlab-verify-on-save-flag nil) ; turn off auto-verify on save
(defun my-matlab-mode-hook ()
  (setq fill-column 76))		; where auto-fill should wrap
(add-hook 'matlab-mode-hook 'my-matlab-mode-hook)
(defun my-matlab-shell-mode-hook ()
  '())
(add-hook 'matlab-shell-mode-hook 'my-matlab-shell-mode-hook)

;; ---------------------------------------------------------

;; ---
;; ---   shortcuts for insertion of chunks of text into tex files.
;; ---

(defun insert-beamer-frame()
  ;; "inserts a text block that is a skeleton for a beamer frame"
  (interactive)
  (insert
   "%   ---------  frame ---------\n
\\begin{sframe}{}\n

\\end{sframe}\n"
   )
  (backward-char 17))
; (add-hook 'tex-mode-hook 'insert-beamer-frame)
(global-set-key "\C-cf" 'insert-beamer-frame)


(defun insert-beamer-bild()
  ;; "inserts a text block that is a skeleton for a beamer frame"
  (interactive)
  (insert
  "
\\begin{center}
\\includegraphics[width=100mm]{bilder/.png}
\\end{center}\n"
  )
  (backward-char 19))
(global-set-key "\C-cb" 'insert-beamer-bild)


(defun insert-latex-bild()
;; "inserts the text lines that is used for images"
  (interactive)
  (insert
  "
\\begin{figure}[!b]
  \\begin{center}
    \\includegraphics[width=110mm]{bilder/.png}
    \\caption{}
    \\label{fig:}
  \\end{center}
\\end{figure}\n"
  )
  (backward-char 67))
(global-set-key "\C-cy" 'insert-latex-bild)


;; ---
;; --- insertion of basic html text ------------------------
;; ---

(defun insert-html-basic-start()
;; "inserts the text lines that is used for images"
  (interactive)
  (insert
  "<!DOCTYPE html>
<html>
  <head>
    <meta charset=\"utf-8\">
    <link rel=\"stylesheet\" href=\"\">
  <title></title>
  </head>
  <body>

  </body>
</html>
"
  )
  (backward-char 59))

;; ---
;; ---
;; ---
