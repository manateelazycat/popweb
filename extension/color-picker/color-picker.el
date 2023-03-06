;;; popweb-color-picker.el --- Katex.js previewer plugin

;; Filename: popweb-color-picker.el
;; Description: Katex.js previewer plugin
;; Author: Andy Stewart <lazycat.manatee@gmail.com>
;; Maintainer: Andy Stewart <lazycat.manatee@gmail.com>
;; Copyright (C) 2021, Andy Stewart, all rights reserved.
;; Created: 2021-11-15 20:04:09
;; Version: 0.1
;; Last-Updated: Fri Jan 21 01:27:21 2022 (-0500)
;;           By: Mingde (Matthew) Zeng
;; URL: https://www.github.org/manateelazycat/popweb-color-picker
;; Keywords:
;; Compatibility: GNU Emacs 29.0.50
;;
;; Features that might be required by this library:
;;
;;
;;

;;; This file is NOT part of GNU Emacs

;;; License
;;
;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
;; any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program; see the file COPYING.  If not, write to
;; the Free Software Foundation, Inc., 51 Franklin Street, Fifth
;; Floor, Boston, MA 02110-1301, USA.

;;; Commentary:
;;
;; Katex.js previewer plugin
;;

;;; Installation:
;;
;; Put popweb-color-picker.el to your load-path.
;; The load-path is usually ~/elisp/.
;; It's set in your ~/.emacs like this:
;; (add-to-list 'load-path (expand-file-name "~/elisp"))
;;
;; And the following to your ~/.emacs startup file.
;;
;; (require 'popweb-color-picker)
;;
;; No need more.

;;; Customize:
;;
;;
;;
;; All of the above can customize by:
;;      M-x customize-group RET popweb-color-picker RET
;;

;;; Change log:
;;
;; 2021/11/15
;;      * First released.
;;

;;; Acknowledgements:
;;
;;
;;

;;; TODO
;;
;;
;;

;;; Require
(require 'popweb)

;;; Code:

(setq popweb-color-picker-index-path (concat (file-name-directory load-file-name) "index.html"))
(setq popweb-color-picker-module-path (concat (file-name-directory load-file-name) "color-picker.py"))

(defun popweb-color-picker-show ()
  (interactive)
  (let* ((position (popweb-get-cursor-coordinate))
         (window-header-height (- (nth 1 (window-inside-pixel-edges)) (nth 1 (window-absolute-pixel-edges))))
         (x (car position))
         (y (- (cdr position) window-header-height))
         (x-offset (popweb-get-cursor-x-offset))
         (y-offset (popweb-get-cursor-y-offset))
         (frame-x (car (frame-position)))
         (frame-y (cdr (frame-position)))
         (frame-w (frame-outer-width))
         (frame-h (frame-outer-height))
         (color-info (or (popweb-color-picker--get-hex-color-at-point)
                         (popweb-color-picker--get-named-color-at-point)
                         (popweb-color-picker--get-rgb-or-hsl-color-at-point)
                         )))
    (popweb-call-async "call_module_method" popweb-color-picker-module-path
                       "pop_color_picker_window"
                       (list
                        "color-picker" popweb-color-picker-index-path
                        x y x-offset y-offset
                        frame-x frame-y frame-w frame-h
                        (if color-info (car color-info) "")
                        ))

    (popweb-color-picker-web-window-can-hide)

    (add-hook 'post-command-hook #'popweb-color-picker-web-window-hide-after-move)
    ))

(defvar popweb-color-picker---hex-color-regexp
  (concat
   ;; Short hex.  css-color-4 adds alpha.
   "\\(#[0-9a-fA-F]\\{3,4\\}\\b\\)"
   "\\|"
   ;; Long hex.  css-color-4 adds alpha.
   "\\(#\\(?:[0-9a-fA-F][0-9a-fA-F]\\)\\{3,4\\}\\b\\)"))

(defun popweb-color-picker--get-named-color-at-point ()
  "Return color name at point."
  (when-let* ((word (word-at-point t))
              (color (assoc (downcase word) css--color-map)))
    (cons word (bounds-of-thing-at-point 'word))))

(defun popweb-color-picker--get-rgb-or-hsl-color-at-point ()
  "Return RGB or HSL formatted color at point."
  (save-excursion
    (when-let* ((open-paren-pos (nth 1 (syntax-ppss))))
      (when (save-excursion
              (goto-char open-paren-pos)
              (looking-back "\\(?:hsl\\|rgb\\)a?" (- (point) 4)))
        (goto-char (nth 1 (syntax-ppss)))))
    (when (eq (char-before) ?\))
      (backward-sexp))
    (skip-chars-backward "rgbhslaRGBHSLA")
    (when (looking-at "\\(\\_<\\(?:hsl\\|rgb\\)a?(\\)")
      (when-let* ((start (point))
                  (end (search-forward ")" nil t)))
        (cons (buffer-substring-no-properties start end) (cons start end))))))

(defun popweb-color-picker--get-hex-color-at-point ()
  "Return hex color at point."
  (with-syntax-table (copy-syntax-table (syntax-table))
    (modify-syntax-entry ?# "w")        ; Make `#' a word constituent.
    (when-let* ((word (thing-at-point 'word t))
                (bounds (bounds-of-thing-at-point 'word)))
      (when (string-match popweb-color-picker---hex-color-regexp word)
        (cons word bounds)))))

(defvar popweb-color-picker-web-window-visible-p nil)

(defun popweb-color-picker-web-window-hide-after-move ()
  (when (and popweb-color-picker-web-window-visible-p (popweb-epc-live-p popweb-epc-process))
    (popweb-call-async "hide_web_window" "color-picker")
    (setq popweb-color-picker-web-window-visible-p nil)
    (remove-hook 'post-command-hook #'popweb-color-picker-web-window-hide-after-move)))

(defun popweb-color-picker-web-window-can-hide ()
  (run-with-timer 1 nil (lambda () (setq popweb-color-picker-web-window-visible-p t))))


(provide 'popweb-color-picker)

;;; popweb-color-picker.el ends here
