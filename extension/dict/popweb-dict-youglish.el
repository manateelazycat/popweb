;;; popweb-dict-youglish.el --- Youglish dict plugin for popweb

;; Filename: popweb-dict-youglish.el
;; Description: Youglish dict plugin for popweb
;; Author: Andy Stewart <lazycat.manatee@gmail.com>
;; Maintainer: Andy Stewart <lazycat.manatee@gmail.com>
;; Copyright (C) 2021, Andy Stewart, all rights reserved.
;; Created: 2021-11-13 23:11:15
;; Version: 0.1
;; Last-Updated: Sun Nov 28 01:43:58 2021 (-0500)
;;           By: Mingde (Matthew) Zeng
;; URL: https://www.github.org/manateelazycat/popweb-dict-youglish
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
;; Youglish dict plugin for popweb
;;

;;; Installation:
;;
;; Put popweb-dict-youglish.el to your load-path.
;; The load-path is usually ~/elisp/.
;; It's set in your ~/.emacs like this:
;; (add-to-list 'load-path (expand-file-name "~/elisp"))
;;
;; And the following to your ~/.emacs startup file.
;;
;; (require 'popweb-dict-youglish)
;;
;; No need more.

;;; Customize:
;;
;;
;;
;; All of the above can customize by:
;;      M-x customize-group RET popweb-dict-youglish RET
;;

;;; Change log:
;;
;; 2021/11/13
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
(require 'popweb-dict)

;;; Code:

(defun popweb-dict-youglish-translate (info)
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
         (width-scale 0.35)
         (height-scale 0.5)
         (word (nth 0 info))
         (url (format "https://youglish.com/pronounce/%s/english?" word))
         (js-code
          (concat
           "window.scrollTo(0, 0); "
           "document.getElementsByTagName('body')[0].style.margin = '0'; "
           "document.getElementsByTagName('header')[0].style.display = 'none'; "
           "document.getElementsByTagName('footer')[0].style.display = 'none'; "
           "document.getElementsByClassName('search')[0].style.display = 'none'; "
           "document.querySelectorAll('div .g_pr_ad_network')[1].style.display = 'none' ; "
           "Array.from(document.querySelectorAll('ins')).forEach(e => { e.style.display = 'none' }); "
           "Array.from(document.querySelectorAll('iframe:not(#player)')).forEach(e => { e.style.display = 'none' }); "
           )))
    (popweb-dict-say-word word)
    (popweb-call-async "call_module_method" popweb-dict-module-path
                       "pop_translate_window"
                       (list
                        "dict_youglish"
                        x y x-offset y-offset
                        frame-x frame-y frame-w frame-h
                        width-scale height-scale
                        url js-code))
    (popweb-dict-youglish-web-window-can-hide)))

;;;###autoload
(defun popweb-dict-youglish-input (&optional word)
  (interactive)
  (popweb-dict-youdao-web-window-hide-after-move)
  (popweb-start 'popweb-dict-youglish-translate (list (or word (popweb-dict-prompt-input "Youglish dict: "))))
  (add-hook 'post-command-hook #'popweb-dict-youglish-web-window-hide-after-move))

;;;###autoload
(defun popweb-dict-youglish-pointer ()
  (interactive)
  (popweb-dict-youdao-web-window-hide-after-move)
  (popweb-start 'popweb-dict-youglish-translate (list (popweb-dict-region-or-word)))
  (add-hook 'post-command-hook #'popweb-dict-youglish-web-window-hide-after-move))

(defvar popweb-dict-youglish-web-window-visible-p nil)

(defun popweb-dict-youglish-web-window-hide-after-move ()
  (when (and popweb-dict-youglish-web-window-visible-p (popweb-epc-live-p popweb-epc-process))
    (popweb-call-async "hide_web_window" "dict_youglish")
    (setq popweb-dict-youglish-web-window-visible-p nil)
    (remove-hook 'post-command-hook #'popweb-dict-youglish-web-window-hide-after-move)))

(defun popweb-dict-youglish-web-window-can-hide ()
  (run-with-timer 1 nil (lambda () (setq popweb-dict-youglish-web-window-visible-p t))))

(provide 'popweb-dict-youglish)

;;; popweb-dict-youglish.el ends here
