;;; popweb-dict.el --- Dict plugin  -*- lexical-binding: t -*-

;; Filename: popweb-dict.el
;; Description: Dict plugin
;; Author: Andy Stewart <lazycat.manatee@gmail.com>
;; Maintainer: Andy Stewart <lazycat.manatee@gmail.com>
;; Copyright (C) 2021, Andy Stewart, all rights reserved.
;; Created: 2021-11-21 07:32:38
;; Version: 0.1
;; Last-Updated: 2021-11-21 07:32:38
;;           By: Andy Stewart
;; URL: https://www.github.org/manateelazycat/popweb-dict
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
;; Dict plugin
;;

;;; Installation:
;;
;; Put popweb-dict.el to your load-path.
;; The load-path is usually ~/elisp/.
;; It's set in your ~/.emacs like this:
;; (add-to-list 'load-path (expand-file-name "~/elisp"))
;;
;; And the following to your ~/.emacs startup file.
;;
;; (require 'popweb-dict)
;;
;; No need more.

;;; Customize:
;;
;;
;;
;; All of the above can customize by:
;;      M-x customize-group RET popweb-dict RET
;;

;;; Change log:
;;
;; 2021/11/21
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

(setq popweb-dict-module-path (concat (file-name-directory load-file-name) "popweb-dict.py"))
(setq popweb-dict-audio-process nil)

(defcustom popweb-dict-say-word-p t
   "Whether play voice when search words.
Default value is t."
  :type 'boolean)

(defun popweb-dict-say-word (&optional word)
  (interactive)
  (unless word
    (setq word (popweb-dict-region-or-word))
    (message "Saying %s ..." word))
  (if (featurep 'cocoa)
      (call-process-shell-command
       (format "say %s" word) nil 0)
    (let ((player (or (executable-find "mpv")
                      (executable-find "mplayer")
                      (executable-find "mpg123"))))
      (if player
          (setq popweb-dict-audio-process
                (start-process
                 player
                 nil
                 player
                 (format "http://dict.youdao.com/dictvoice?type=2&audio=%s" (url-hexify-string word))))
        (message "mpv, mplayer or mpg123 is needed to play word voice")))))

(defun popweb-dict-prompt-input (prompt)
  "Prompt input object for translate."
  (read-string (format "%s(%s): " prompt (or (popweb-dict-region-or-word) ""))
               nil nil
               (popweb-dict-region-or-word)))

(defun popweb-dict-region-or-word ()
  "Return region or word around point.
If `mark-active' on, return region string.
Otherwise return word around point."
  (if mark-active
      (buffer-substring-no-properties (region-beginning)
                                      (region-end))
    (thing-at-point 'word t)))

(cl-defmacro popweb-dict-create (name url js-code &optional js-file)
  (let* ((var-visible-p (intern (format "popweb-dict-%s-web-window-visible-p" name)))
         (var-say-word-process (intern (format "popweb-dict-%s-say-word-process" name)))
         (func-hide-after-move (intern (format "popweb-dict-%s-web-window-hide-after-move" name)))
         (func-can-hide (intern (format "popweb-dict-%s-web-window-can-hide" name)))
         (func-pointer (intern (format "popweb-dict-%s-pointer" name)))
         (func-input (intern (format "popweb-dict-%s-input" name)))
         (func-translate (intern (format "popweb-dict-%s-translate" name))))
    `(progn
       (defvar ,var-visible-p nil)
       (defvar ,var-say-word-process nil)

       (defun ,func-hide-after-move ()
         (when (and ,var-visible-p (popweb-epc-live-p popweb-epc-process))
           (popweb-call-async "hide_web_window" (format "dict_%s" ,name))
           (setq ,var-visible-p nil)
           (when (and popweb-dict-say-word-p (process-live-p ,var-say-word-process))
             (kill-process ,var-say-word-process))
           (remove-hook 'post-command-hook #',func-hide-after-move)))

       (defun ,func-can-hide ()
         (run-with-timer 1 nil (lambda () (setq ,var-visible-p t))))

       (defun ,func-translate (info)
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
                (width-scale 0.3)
                (height-scale 0.5)
                (word (nth 0 info))
                (url (format ,url word))
                (js-code (format "try { %s } catch (err) { console.log(err.message) }" ,js-code))
                (js-file (format "%s" ,js-file))
                (args (nth 0 (cdr info))))
           (if popweb-dict-say-word-p (setq ,var-say-word-process (popweb-dict-say-word word)))
           (popweb-call-async "call_module_method" popweb-dict-module-path
                              "pop_translate_window"
                              (list
                               (format "dict_%s" ,name)
                               x y x-offset y-offset
                               frame-x frame-y frame-w frame-h
                               width-scale height-scale
                               url js-code
                               js-file args))
           (funcall ',func-can-hide)))

       (defun ,func-pointer ()
         (interactive)
         (,func-hide-after-move)
         (popweb-start ',func-translate (list (popweb-dict-region-or-word)))
         (add-hook 'post-command-hook #',func-hide-after-move))

       (defun ,func-input (&optional word &rest args)
         (interactive)
         (,func-hide-after-move)
         (popweb-start ',func-translate (list (or word (popweb-dict-prompt-input (format "%s dict: " (capitalize ,name)))) args))
         (add-hook 'post-command-hook #',func-hide-after-move))
       )))

(popweb-dict-create "bing"
                    "http://www.bing.com/dict/search?mkt=zh-cn&q=%s"
                    (concat
                     "window.scrollTo(0, 0); "
                     "document.getElementsByTagName('html')[0].style.visibility = 'hidden'; "
                     "document.getElementsByClassName('lf_area')[0].style.visibility = 'visible' ; "
                     "document.getElementsByTagName('header')[0].style.display = 'none'; "
                     "document.getElementsByClassName('contentPadding')[0].style.padding = '10px';"
                     ))

(popweb-dict-create "youdao"
                    "https://www.youdao.com/w/eng/%s"
                    (concat
                     "window.scrollTo(0, 0); "
                     "document.getElementsByTagName('html')[0].style.visibility = 'hidden'; "
                     "document.getElementById('results').style.visibility = 'visible'; "
                     "document.getElementById('scontainer').style.margin = '0'; "
                     "document.getElementById('scontainer').style.padding = '0'; "
                     "document.getElementById('result_navigator').style.display = 'none'; "
                     "document.getElementById('container').style.padding = '0'; "
                     "document.getElementById('container').style.paddingLeft = '10px'; "
                     "document.getElementById('container').style.margin = '0'; "
                     "document.getElementById('topImgAd').style.display = 'none'; "
                     ))

(popweb-dict-create "youglish"
                    "https://youglish.com/pronounce/%s/english?"
                    (concat
                     "window.scrollTo(0, 0); "
                     "document.getElementsByTagName('body')[0].style.margin = '0'; "
                     "document.getElementsByTagName('header')[0].style.display = 'none'; "
                     "document.getElementsByTagName('footer')[0].style.display = 'none'; "
                     "document.getElementsByClassName('search')[0].style.display = 'none'; "
                     "document.querySelectorAll('div .g_pr_ad_network')[1].style.display = 'none' ; "
                     "document.querySelectorAll('div .g_pr_ad_network')[3].style.margin = '0' ; "
                     "Array.from(document.querySelectorAll('ins')).forEach(e => { e.style.display = 'none' }); "
                     "Array.from(document.querySelectorAll('iframe:not(#player)')).forEach(e => { e.style.display = 'none' }); "
                     ))

(popweb-dict-create "dictcn"
                    "http://dict.cn/%s"
                    (concat
                     "window.scrollTo(0, 0); "
                     "document.getElementsByTagName('html')[0].style.visibility = 'hidden'; "
                     "document.getElementsByClassName('main')[0].style.visibility = 'visible' ; "
                     "document.getElementsByClassName('main')[0].style.margin = '0' ; "
                     "document.getElementById('dshared').style.display = 'none';"
                     "document.getElementById('aswift_0_host').style.display = 'none';"
                     "document.getElementById('aswift_1_host').style.display = 'none';"
                     "document.getElementById('aswift_2_host').style.display = 'none';"
                     "document.getElementById('aswift_3_host').style.display = 'none';"
                     "document.getElementById('content').style.padding = '0';"
                     "document.getElementById('content').style.margin = '0';"
                     "document.getElementById('footer').style.display = 'none';"
                     "document.getElementsByClassName('copyright')[0].style.display = 'none';"
                     "Array.from(document.querySelectorAll('iframe')).forEach(e => { e.style.display = 'none' })"
                     ))

(provide 'popweb-dict)

;;; popweb-dict.el ends here
