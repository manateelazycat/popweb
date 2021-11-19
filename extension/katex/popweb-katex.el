;;; popweb-katex.el --- Katex.js previewer plugin

;; Filename: popweb-katex.el
;; Description: Katex.js previewer plugin
;; Author: Andy Stewart <lazycat.manatee@gmail.com>
;; Maintainer: Andy Stewart <lazycat.manatee@gmail.com>
;; Copyright (C) 2021, Andy Stewart, all rights reserved.
;; Created: 2021-11-15 20:04:09
;; Version: 0.1
;; Last-Updated: 2021-11-15 20:04:09
;;           By: Andy Stewart
;; URL: https://www.github.org/manateelazycat/popweb-katex
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
;; Put popweb-katex.el to your load-path.
;; The load-path is usually ~/elisp/.
;; It's set in your ~/.emacs like this:
;; (add-to-list 'load-path (expand-file-name "~/elisp"))
;;
;; And the following to your ~/.emacs startup file.
;;
;; (require 'popweb-katex)
;;
;; No need more.

;;; Customize:
;;
;;
;;
;; All of the above can customize by:
;;      M-x customize-group RET popweb-katex RET
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

(defvar popweb-katex-current-buffer nil
  "The current buffer.")

(setq popweb-katex-index-path (concat (file-name-directory load-file-name) "index.html"))

(defun popweb-katex-preview (info)
  (let* ((position (popweb-get-cursor-coordinate))
         (x (car position))
         (y (cdr position))
         (x-offset (popweb-get-cursor-x-offset))
         (y-offset (popweb-get-cursor-y-offset))
         (width 0.1)
         (height 0.1)
         (show-window (nth 0 info))
         (latex-string (nth 1 info)))
    (popweb-call-async "pop_katex_window" x y x-offset y-offset width height popweb-katex-index-path show-window latex-string)))

(defun popweb-katex-show ()
  (interactive)
  (let* ((math-at-point (webkit-katex-render--math-at-point))
         (pos (car math-at-point))
         (latex-string (nth 1 math-at-point)))
    (if latex-string
        (if (not (eq latex-string webkit-katex-render--previous-math))
            (progn
              (popweb-start 'popweb-katex-preview (list t
                                                        (replace-regexp-in-string "\\\\" "\\\\" latex-string t t)))
              (setq webkit-katex-render--previous-math latex-string)))
      (popweb-start 'popweb-katex-preview (list nil "e^{i\\\\pi}+1=0"))))
  (add-hook 'post-command-hook #'popweb-katex-update nil t))

(defun popweb-katex-update ()
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
      (if latex-string
          (if (not (eq latex-string webkit-katex-render--previous-math))
              (progn
                (popweb-call-async "pop_katex_window"
                                   x y x-offset y-offset width height popweb-katex-index-path
                                   t
                                   (--> latex-string
                                     (replace-regexp-in-string "\\\\" "\\\\" it t t)
                                     (replace-regexp-in-string "\n" "" it t t)))
                (setq webkit-katex-render--previous-math latex-string)))
        (popweb-katex-hide)))))

(defun popweb-katex-hide ()
  (interactive)
  (ignore-errors
    (popweb-call-async "katex_hide_web_window")))

(defun popweb-katex-hide-after-switch-buffer ()
  (unless (equal (current-buffer) popweb-katex-current-buffer)
    (popweb-katex-hide)))

(defun popweb-katex-hide-after-lose-focus ()
  (if (frame-focus-state)
      (popweb-katex-update)
    (popweb-katex-hide)))

;;;###autoload
(define-minor-mode popweb-katex-mode
  "Toggle popweb-katex-mode"
  nil nil nil
  (if popweb-katex-mode
      (progn
        (setq popweb-katex-current-buffer (current-buffer))
        (popweb-katex-show)
        (add-hook 'buffer-list-update-hook 'popweb-katex-hide-after-switch-buffer)
        (add-function :after after-focus-change-function #'popweb-katex-hide-after-lose-focus))
    (remove-hook 'buffer-list-update-hook 'popweb-katex-hide-after-switch-buffer)
    (remove-hook 'post-command-hook #'popweb-katex-update t)
    (remove-function after-focus-change-function #'popweb-katex-hide-after-lose-focus)
    (popweb-katex-hide)))

(provide 'popweb-katex)

;;; popweb-katex.el ends here
