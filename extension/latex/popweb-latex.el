;;; popweb-latex.el --- Katex.js previewer plugin

;; Filename: popweb-latex.el
;; Description: Katex.js previewer plugin
;; Author: Andy Stewart <lazycat.manatee@gmail.com>
;; Maintainer: Andy Stewart <lazycat.manatee@gmail.com>
;; Copyright (C) 2021, Andy Stewart, all rights reserved.
;; Created: 2021-11-15 20:04:09
;; Version: 0.1
;; Last-Updated: Sat Nov 20 04:34:02 2021 (-0500)
;;           By: Mingde (Matthew) Zeng
;; URL: https://www.github.org/manateelazycat/popweb-latex
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
;; Put popweb-latex.el to your load-path.
;; The load-path is usually ~/elisp/.
;; It's set in your ~/.emacs like this:
;; (add-to-list 'load-path (expand-file-name "~/elisp"))
;;
;; And the following to your ~/.emacs startup file.
;;
;; (require 'popweb-latex)
;;
;; No need more.

;;; Customize:
;;
;;
;;
;; All of the above can customize by:
;;      M-x customize-group RET popweb-latex RET
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
(require 'dash)
(require 'popweb)
(require 'math-at-point)

;;; Code:

(defvar popweb-latex-current-buffer nil
  "The current buffer.")

(setq popweb-latex-index-path (concat (file-name-directory load-file-name) "index.html"))

(defun popweb-latex-preview (info)
  (let* ((position (popweb-get-cursor-coordinate))
         (x (car position))
         (y (cdr position))
         (x-offset (popweb-get-cursor-x-offset))
         (y-offset (popweb-get-cursor-y-offset))
         (width 0.1)
         (height 0.1)
         (show-window (nth 0 info))
         (latex-string (nth 1 info)))
    (popweb-call-async "pop_latex_window" "latex" x y x-offset y-offset width height popweb-latex-index-path show-window latex-string)))

(defun popweb-latex-show ()
  (interactive)
  (let* ((math-at-point (webkit-katex-render--math-at-point))
         (pos (car math-at-point))
         (latex-string (nth 1 math-at-point)))
    (if latex-string
        (if (not (eq latex-string webkit-katex-render--previous-math))
            (progn
              (popweb-start 'popweb-latex-preview (list t
                                                        (replace-regexp-in-string "\\\\" "\\\\" latex-string t t)))
              (setq webkit-katex-render--previous-math latex-string)))
      (popweb-start 'popweb-latex-preview (list nil "e^{i\\\\pi}+1=0"))))
  (add-hook 'post-command-hook #'popweb-latex-update nil t))

(defun popweb-latex-update ()
  (interactive)
  (when (popweb-epc-live-p popweb-epc-process)
    (let* ((math-at-point (webkit-katex-render--math-at-point))
           (latex-string (nth 1 math-at-point))
           (position (popweb-get-cursor-coordinate))
           (x (car position))
           (y (cdr position))
           (x-offset (popweb-get-cursor-x-offset))
           (y-offset (popweb-get-cursor-y-offset))
           (width 0.1)
           (height 0.1))
      (if position
        (if latex-string
            (if (not (eq latex-string webkit-katex-render--previous-math))
                (progn
                  (popweb-call-async "pop_latex_window" "latex"
                                     x y x-offset y-offset width height popweb-latex-index-path
                                     t
                                     (--> latex-string
                                          (replace-regexp-in-string "\\\\" "\\\\" it t t)
                                          (replace-regexp-in-string "\n" "" it t t)))
                  (setq webkit-katex-render--previous-math latex-string)))
          (popweb-latex-hide))))))

(defun popweb-latex-hide ()
  (interactive)
  (ignore-errors
    (popweb-call-async "hide_web_window" "latex")))

(defun popweb-latex-hide-after-switch-buffer ()
  (unless (equal (current-buffer) popweb-latex-current-buffer)
    (popweb-latex-hide)))

(defun popweb-latex-hide-after-lose-focus ()
  (if (frame-focus-state)
      (popweb-latex-update)
    (popweb-latex-hide)))

;;;###autoload
(define-minor-mode popweb-latex-mode
  "Toggle popweb-latex-mode"
  :group 'popweb-latex
  (if popweb-latex-mode
      (progn
        (setq popweb-latex-current-buffer (current-buffer))
        (popweb-latex-show)
        (add-hook 'buffer-list-update-hook 'popweb-latex-hide-after-switch-buffer)
        (add-function :after after-focus-change-function #'popweb-latex-hide-after-lose-focus))
    (remove-hook 'buffer-list-update-hook 'popweb-latex-hide-after-switch-buffer)
    (remove-hook 'post-command-hook #'popweb-latex-update t)
    (remove-function after-focus-change-function #'popweb-latex-hide-after-lose-focus)
    (popweb-latex-hide)))

(provide 'popweb-latex)

;;; popweb-latex.el ends here
