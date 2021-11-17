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
(require 'popweb)
(require 'math-at-point)

;;; Code:
(setq popweb-katex-index-path (concat (file-name-directory load-file-name) "index.html"))

(defun popweb-katex-preview (info)
  (let* ((position (popweb-get-cursor-coordinate))
         (x (car position))
         (y (cdr position))
         (x-offset (popweb-get-cursor-x-offset))
         (y-offset (popweb-get-cursor-y-offset))
         (width 0.1)
         (height 0.1)
         (index_file popweb-katex-index-path)
         (show-window (nth 0 info))
         (latex-string (nth 1 info)))
    (popweb-call-async "pop_katex_window" x y x-offset y-offset width height index_file show-window latex-string)))

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
      (progn
        (popweb-start 'popweb-katex-preview (list nil "e^{i\pi}+1=0")))))
  (add-hook 'post-command-hook #'popweb-katex-update nil t))

(defun popweb-katex-update ()
  (interactive)
  (when (popweb-epc-live-p popweb-epc-process)
    (let* ((math-at-point (webkit-katex-render--math-at-point))
           (pos (car math-at-point))
           (latex-string (nth 1 math-at-point)))
      (if latex-string
          (if (not (eq latex-string webkit-katex-render--previous-math))
              (progn
                (setq abc latex-string)
                (popweb-call-async "update_katex_content"
                                   (replace-regexp-in-string "\\\\" "\\\\" latex-string t t))
                (setq webkit-katex-render--previous-math latex-string)))
        (popweb-katex-hide)))))

(defun popweb-katex-hide ()
  (interactive)
  (popweb-call-async "katex_hide_web_window"))

;;;###autoload
(define-minor-mode popweb-katex-mode
  "Toggle popweb-katex-mode"
  nil nil nil
  (if popweb-katex-mode
      (progn
        (popweb-katex-show))
    (progn
      (remove-hook 'post-command-hook #'popweb-katex-update t)
      (popweb-katex-hide))))

(provide 'popweb-katex)

;;; popweb-katex.el ends here
