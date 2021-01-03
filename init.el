;;; init.el --- my own emacs init.el file

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
 '(custom-enabled-themes (quote (wb2)))
 '(custom-safe-themes
   (quote
    ("fcfb725e0908cc1a7921fddfce8cf6835a811e5b685671a62ddc76c522723f14" default)))
 '(inhibit-startup-screen t)
 '(package-selected-packages
   (quote
    (better-defaults
     company-box
     visual-fill-column
     org
     org-bullets
     sr-speedbar
     elpy
     flycheck
     blacken
     2048-game
     which-key
     try
     use-package
     pandoc-mode
     pandoc
     markdown-mode)))
 '(show-paren-mode t)
 '(text-mode-hook (quote (turn-on-auto-fill text-mode-hook-identify)))
 '(tool-bar-mode nil)
 '(tooltip-mode nil))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(default ((t (:family "Consolas"
                        :foundry "MS  "
                        :slant normal
                        :weight normal
                        :height 90
                        :width normal)))))

;; ---------------
;; --- my personal settings
;; ---------------

;; --- satter lite kortbindningar
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

;; --- tyst *scratch* buffer och inget pling
(setq initial-scratch-message "")
(setq ring-bell-function 'ignore)

(add-hook 'before-save-hook
          'delete-trailing-whitespace)

;; --- melpa packages settings
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

;; (use-package better-defaults
;;   :ensure t
;;   )
;; (menu-bar-mode t)


(use-package try
  :ensure t)


(use-package which-key
  :ensure t
  :config (which-key-mode))


(use-package elpy
  :ensure t
  :init
  (elpy-enable)
  (when (require 'flycheck nil t)
  (setq elpy-modules (delq 'elpy-module-flymake elpy-modules))
  (add-hook 'elpy-mode-hook 'flycheck-mode))
  :custom
  (python-shell-interpreter "python3"))


;; (use-package company
;;   :after elpy-mode
;;   :hook (elpy-mode . company-mode)
;;   :bind (:map company-active-map
;;          ("<tab>" . company-complete-selection))
;;         (:map elpy-mode-map
;;          ("<tab>" . company-indent-or-complete-common))
;;   :custom
;;   (company-minimum-prefix-length 1)
;;   (company-idle-delay 0.0))

;; (use-package company-box
;;   :hook (company-mode . company-box-mode))

;; ---


(use-package markdown-mode
  :ensure t
  :mode (("README\\.md\\'" . gfm-mode)
         ("\\.md\\'" . markdown-mode)
         ("\\.markdown\\'" . markdown-mode))
  :init (setq markdown-command "multimarkdown"))
(add-hook 'markdown-mode-hook 'pandoc-mode)

(use-package pandoc
  :ensure t)


(use-package pandoc-mode
  :ensure t)


(use-package sr-speedbar
  :ensure t)
;;(require 'sr-speedbar)
(global-set-key "\C-cs" 'sr-speedbar-toggle)
(setq speedbar-use-images nil)
(setq sr-speedbar-refresh-turn-on t)

(use-package texfrag
  :ensure t)

(use-package easy-hugo
  :ensure t
  :init
  (setq easy-hugo-basedir "~/hugo/blog/")
  (setq easy-hugo-url "https://blog.nist.se")
  (setq easy-hugo-sshdomain "ns5.inleed.net")
  (setq easy-hugo-root "public_html/blog/")
  (setq easy-hugo-previewtime "300")
  :bind ("C-c C-e" . easy-hugo))

;; deploying hugo blog post
(defun easy-hugo-deploy-site ()
  "Runs the deploy command found in ~/bin."
  (interactive)
  (shell-command "deploy"))
(global-set-key "\C-ce" 'easy-hugo-deploy-site)

;; ----------------
;; Org Mode Configuration ------------------------------------------------------
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
 '(("TODO" . "red4") ("DOING" . "magenta4") ("DONE" . "green4")))
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
  :hook (org-mode . ns/org-mode-visual-fill))

;; --- END org-mode


;; ----------------
;; --- init for lisp-files in ~/.emacs.d/lisp
;; ----------------
(add-to-list 'load-path "~/.emacs.d/lisp/")

;; abaqus
(autoload 'abaqus-mode "abaqus" "Enter abaqus mode." t)
(setq auto-mode-alist (cons '("\\.inp\\'" . abaqus-mode) auto-mode-alist))
(add-hook 'abaqus-mode-hook 'font-lock-mode)

;; lsdyna
(autoload 'lsdyna-mode "lsdyna" "Enter ls-dyna mode." t)
(setq auto-mode-alist (cons '("\\.k\\'" . lsdyna-mode) auto-mode-alist))
(add-hook 'lsdyna-mode-hook 'font-lock-mode)

;; org-present

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


;; -setting latex font size in org eq viewing
(setq my-org-latex-preview-scale 1.5)   ; depends on the font used in emacs or just on user preference

(defun org-latex-preview-advice (orig-func &rest args)
  (let ((old-val (copy-tree org-format-latex-options)))     ; plist-put is maybe-destructive, weird. So, we have to restore old value ourselves
    (setq org-format-latex-options (plist-put org-format-latex-options
                                              :scale
                                              (* my-org-latex-preview-scale (expt text-scale-mode-step text-scale-mode-amount))))
    (apply orig-func args)
    (setq org-format-latex-options old-val)))
(advice-add 'org-latex-preview :around #'org-latex-preview-advice)

;; disable mouse clicking on laptop
(require 'disable-mouse)
(if (string-equal (system-name) "niclap")
    (global-disable-mouse-mode))


;; octave uses matlab.el
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


;; ---
;; ---   Nano emacs
;; ---

;; (add-to-list 'load-path "~/.emacs.d/nano-emacs")
;; ;; Window layout (optional)
;; ;;(require 'nano-layout)

;; ;; Theming Command line options (this will cancel warning messages)
;; (add-to-list 'command-switch-alist '("-dark"   . (lambda (args))))
;; (add-to-list 'command-switch-alist '("-light"  . (lambda (args))))
;; (add-to-list 'command-switch-alist '("-default"  . (lambda (args))))

;; (cond
;;  ((member "-default" command-line-args) t)
;;  ((member "-dark" command-line-args) (require 'nano-theme-dark))
;;  (t (require 'nano-theme-light)))

;; ;; Customize support for 'emacs -q' (Optional)
;; ;; You can enable customizations by creating the nano-custom.el file
;; ;; with e.g. `touch nano-custom.el` in the folder containing this file.
;; (let* ((this-file  (or load-file-name (buffer-file-name)))
;;        (this-dir  (file-name-directory this-file))
;;        (custom-path  (concat this-dir "nano-custom.el")))
;;   (when (and (eq nil user-init-file)
;;              (eq nil custom-file)
;;              (file-exists-p custom-path))
;;     (setq user-init-file this-file)
;;     (setq custom-file custom-path)
;;     (load custom-file)))

;; ;; Theme
;; (require 'nano-faces)
;; (nano-faces)

;; (require 'nano-theme)
;; ;; (nano-theme)

;; ;; Nano default settings (optional)
;; (require 'nano-defaults)

;; ;; Nano session saving (optional)
;; ;; (require 'nano-session)

;; ;; Nano header & mode lines (optional)
;; (require 'nano-modeline)

;; ;; Nano key bindings modification (optional)
;; ;; (require 'nano-bindings)

;; ;; Nano counsel configuration (optional)
;; ;; Needs "counsel" package to be installed (M-x: package-install)
;; ;; (require 'nano-counsel)

;; ;; Welcome message (optional)
;; (let ((inhibit-message t))
;;   (message "Welcome to GNU Emacs / N Λ N O edition")
;;   (message (format "Initialization time: %s" (emacs-init-time))))

;; ;; ;; Splash (optional)
;; ;; (add-to-list 'command-switch-alist '("-no-splash" . (lambda (args))))
;; ;; (unless (member "-no-splash" command-line-args)
;; ;;   (require 'nano-splash))

;; ;; ;; Help (optional)
;; ;; (add-to-list 'command-switch-alist '("-no-help" . (lambda (args))))
;; ;; (unless (member "-no-help" command-line-args)
;; ;;   (require 'nano-help))

;; (provide 'nano)

;; ;; ---   end Nano

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
;; --- insertion of basic html text
;; ---

(defun insert-html-basic-start()
;; "inserts the text lines that is used for images"
  (interactive)
  (insert
  "<!DOCTYPE html>
<html>
  <head>
  <meta charset=\"utf-8\">
  <title></title>
  </head>
  <body>

  </body>
</html>
"
  )
  (backward-char 19))

;; ---
;; ---
;; ---
