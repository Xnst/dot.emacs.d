;;; whiteboard-theme.el --- Custom theme for faces

;; Copyright (C) 2011-2019 Free Software Foundation, Inc.

;; Author: Scott Frazer <frazer.scott@gmail.com>

;; This file is part of GNU Emacs.

;; GNU Emacs is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; GNU Emacs is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs.  If not, see <https://www.gnu.org/licenses/>.

;;; Code:

(deftheme wb2
  "Face colors similar to markers on a whiteboard.")

(let ((class '((class color) (min-colors 89))))
  (custom-theme-set-faces
   'wb2
   `(Info-title-1-face ((,class (:family "helv" :weight bold :height 1.728))))
   `(Info-title-2-face ((,class (:family "helv" :weight bold :height 1.44))))
   `(Info-title-3-face ((,class (:family "helv" :weight bold :height 1.2))))
   `(Info-title-4-face ((,class (:family "helv" :weight bold))))
   `(compilation-column-number ((,class (:foreground "DarkGreen"))))
   `(compilation-error ((,class (:foreground "Red1"))))
   `(compilation-info ((,class (:weight normal :foreground "DeepSkyBlue4"))))
   `(compilation-line-number ((,class (:foreground "DarkGreen"))))
   `(cperl-array-face ((,class (:foreground "SlateBlue3"))))
   `(cperl-hash-face ((,class (:foreground "turquoise3"))))
   `(cperl-nonoverridable-face ((,class (:foreground "orchid3"))))
   `(cursor ((,class (:background "Green4"))))
   `(default ((,class (:background "oldlace" :foreground "black"))))
   `(dired-marked ((,class (:background "dodgerblue3" :foreground "white"))))
   `(flymake-errline ((,class (:background nil :underline "DarkRed"))))
   `(flymake-warnline ((,class (:background nil :underline "magenta3"))))
   `(font-lock-builtin-face ((,class (:foreground "DarkOrange3"))))
   `(font-lock-comment-delimiter-face ((,class (:foreground "gray50"))))
   `(font-lock-comment-face ((,class (:foreground "gray50"))))
   `(font-lock-constant-face ((,class (:foreground "maroon"))))
   `(font-lock-doc-face ((,class (:foreground "SaddleBrown"))))
   `(font-lock-doc-string-face ((,class (:foreground "SaddleBrown"))))
   `(font-lock-function-name-face ((,class (:foreground "firebrick4"))))
   `(font-lock-keyword-face ((,class (:foreground "DodgerBlue3"))))
   `(font-lock-preprocessor-face ((,class (:foreground "gold4"))))
   `(font-lock-reference-face ((,class (:foreground "salmon"))))
   `(font-lock-regexp-grouping-backslash ((,class (:weight bold))))
   `(font-lock-regexp-grouping-construct ((,class (:weight bold))))
   `(font-lock-string-face ((,class (:foreground "red4"))))
   `(font-lock-type-face ((,class (:foreground "magenta4"))))
   `(font-lock-variable-name-face ((,class (:foreground "DarkGreen"))))
   `(font-lock-warning-face ((,class (:foreground "DarkRed"))))
   `(fringe ((,class (:background "antique white"))))
   `(highlight ((,class (:background "SkyBlue1"))))
   `(ido-first-match ((,class (:weight normal :foreground "DarkOrange3"))))
   `(ido-only-match ((,class (:foreground "SeaGreen4"))))
   `(ido-subdir ((,class (:foreground nil :inherit font-lock-keyword-face))))
   `(info-header-node ((,class (:foreground "DeepSkyBlue1"))))
   `(info-header-xref ((,class (:foreground "SeaGreen2"))))
   `(info-menu-header ((,class (:family "helv" :weight bold))))
   `(info-node ((,class (:foreground "DeepSkyBlue1"))))
   `(info-xref ((,class (:foreground "SeaGreen4"))))
   `(isearch ((,class (:background "coral2" :foreground "white"))))
   `(isearch-lazy-highlight-face ((,class (:background "coral4" :foreground "white"))))
   `(lazy-highlight ((,class (:background "cadetblue" :foreground "white"))))
   `(match ((,class (:background "LightPink1"))))
   `(minibuffer-prompt ((,class (:foreground "DodgerBlue4"))))
   `(mode-line ((,class (:background "gray95" :foreground "black" :box (:line-width 1 :style released-button)))))
   `(mode-line-buffer-id ((,class (:weight bold :background nil :foreground "grey12"))))
   `(mode-line-inactive ((,class (:background "gray40" :foreground "dark grey" :box (:line-width 1 :color "gray40" :style nil)))))
   `(outline-1 ((,class (:foreground "DodgerBlue4" :weight bold))))
   `(outline-2 ((,class (:foreground "LightSteelBlue4" :weight bold))))
   `(outline-3 ((,class (:foreground "LightSkyBlue4" :weight bold))))
   `(outline-4 ((,class (:foreground "RoyalBlue1"))))
   `(outline-5 ((,class (:foreground "DeepSkyBlue"))))
   `(primary-selection ((,class (:background "blue3"))))
   `(realgud-overlay-arrow1  ((,class (:foreground "DarkGreen"))))
   `(realgud-overlay-arrow2  ((,class (:foreground "DarkOliveGreen"))))
   `(realgud-overlay-arrow3  ((,class (:foreground "gray60"))))
   `(realgud-bp-disabled-face      ((,class (:foreground "gray60"))))
   `(realgud-bp-line-enabled-face  ((,class (:underline "DarkRed"))))
   `(realgud-bp-line-disabled-face ((,class (:underline "gray60"))))
   `(realgud-file-name             ((,class :foreground "DarkGreen")))
   `(realgud-line-number           ((,class :foreground "blue3")))
   `(realgud-backtrace-number      ((,class :foreground "blue3" :weight bold)))

   `(region ((,class (:background "SkyBlue1"))))
   `(show-paren-match-face ((,class (:background "dodgerblue1" :foreground "white"))))
   `(show-paren-mismatch-face ((,class (:background "red1" :foreground "white"))))
   `(warning ((,class (:foreground "Yellow4"))))))

(provide-theme 'wb2)

;; Local Variables:
;; no-byte-compile: t
;; End:

;;; whiteboard-theme.el ends here
