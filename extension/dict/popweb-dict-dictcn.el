;;; popweb-dict-dictcn.el --- Dictcn dict plugin for popweb  -*- lexical-binding: t -*-

;; Filename: popweb-dict-dictcn.el
;; Description: Dictcn dict plugin for popweb
;; Author: Andy Stewart <lazycat.manatee@gmail.com>
;; Maintainer: Andy Stewart <lazycat.manatee@gmail.com>
;; Copyright (C) 2021, Andy Stewart, all rights reserved.
;; Created: 2021-11-13 23:11:15
;; Version: 0.1
;; Last-Updated: Sun Nov 28 01:43:58 2021 (-0500)
;;           By: Mingde (Matthew) Zeng
;; URL: https://www.github.org/manateelazycat/popweb-dict-dictcn
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
;; Dictcn dict plugin for popweb
;;

;;; Installation:
;;
;; Put popweb-dict-dictcn.el to your load-path.
;; The load-path is usually ~/elisp/.
;; It's set in your ~/.emacs like this:
;; (add-to-list 'load-path (expand-file-name "~/elisp"))
;;
;; And the following to your ~/.emacs startup file.
;;
;; (require 'popweb-dict-dictcn)
;;
;; No need more.

;;; Customize:
;;
;;
;;
;; All of the above can customize by:
;;      M-x customize-group RET popweb-dict-dictcn RET
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

(provide 'popweb-dict-dictcn)

;;; popweb-dict-dictcn.el ends here
