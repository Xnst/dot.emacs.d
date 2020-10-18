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
    ("20d5d6acdc25fafd6ded585dca7f3ea5e97c98890de88ca058bedebf6ac75a30" default)))
 '(inhibit-startup-screen t)
 '(package-selected-packages
   (quote
    (pylint flycheck 2048-game which-key try use-package pandoc-mode pandoc markdown-mode elpy)))
 '(show-paren-mode t)
 '(text-mode-hook (quote (turn-on-auto-fill text-mode-hook-identify)))
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

;; --- satter lite kortbindningar
;; ---------------
(global-set-key "\M-1" 'other-window)
(global-set-key "\C-cq" 'query-replace)
(global-set-key "\C-ca" 'mark-whole-buffer)
(global-set-key "\C-c;" 'comment-region)
(global-set-key "\C-cw" 'ispell-word)
(global-set-key "\C-ci" 'indent-region)
(global-set-key "\C-cl" 'count-lines-region)

;; --- tyst *scratch* buffer och inget pling
(setq initial-scratch-message "")
(setq ring-bell-function 'ignore)

;; --- melpa packages settings
;; ----------------
(require 'package)
(setq package-enable-at-startup nil)
(add-to-list 'package-archives
	     '("melpa" . "https://melpa.org/packages/"))

(package-initialize)

(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

;; --- the packages  

(use-package try
  :ensure t)

(use-package which-key
  :ensure t
  :config (which-key-mode))

(use-package elpy
  :ensure t
  :init
  (elpy-enable))

(use-package markdown-mode
  :ensure t
  :mode (("README\\.md\\'" . gfm-mode)
         ("\\.md\\'" . markdown-mode)
         ("\\.markdown\\'" . markdown-mode))
  :init (setq markdown-command "multimarkdown"))
(add-hook 'markdown-mode-hook 'pandoc-mode)


;; flycheck enable
(use-package flycheck
  :ensure t
  :init (global-flycheck-mode))
;; (add-hook 'after-init-hook #'global-flycheck-mode)

;; pylint enable
(use-package pylint
  :ensure t)
(autoload 'pylint "pylint")
(add-hook 'python-mode-hook 'pylint-add-menu-items)
(add-hook 'python-mode-hook 'pylint-add-key-bindings)

(use-package pandoc
  :ensure t)

(use-package pandoc-mode
  :ensure t)


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
;; ---
;; ---

