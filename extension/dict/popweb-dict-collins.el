;;; popweb-dict-collins.el --- Collins dict plugin for popweb  -*- lexical-binding: t -*-

;; Filename: popweb-dict-collins.el
;; Description: Collins dict plugin for popweb
;; Author: Andy Stewart <lazycat.manatee@gmail.com>
;; Maintainer: Andy Stewart <lazycat.manatee@gmail.com>
;; Copyright (C) 2021, Andy Stewart, all rights reserved.
;; Created: 2021-11-13 23:11:15
;; Version: 0.1
;; Last-Updated: Sun Nov 28 01:43:58 2021 (-0500)
;;           By: Mingde (Matthew) Zeng
;; URL: https://www.github.org/manateelazycat/popweb-dict-collins
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
;; Collins dict plugin for popweb
;;

;;; Installation:
;;
;; Put popweb-dict-collins.el to your load-path.
;; The load-path is usually ~/elisp/.
;; It's set in your ~/.emacs like this:
;; (add-to-list 'load-path (expand-file-name "~/elisp"))
;;
;; And the following to your ~/.emacs startup file.
;;
;; (require 'popweb-dict-collins)
;;
;; No need more.

;;; Customize:
;;
;;
;;
;; All of the above can customize by:
;;      M-x customize-group RET popweb-dict-collins RET
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

(popweb-dict-create "collins"
                    "https://www.collinsdictionary.com/zh/dictionary/english-chinese/%s"
                    (concat
                     "window.scrollTo(0, 0);"
                     "document.getElementsByTagName('html')[0].style.visibility = 'hidden';"
                     "document.getElementsByClassName('res_cell_center')[0].style.visibility = 'visible' ;"
                     "document.getElementsByClassName('tabsNavigation')[0].style.display = 'none' ;"
                     "document.getElementsByClassName('carousel')[0].style.display = 'none' ;"
                     "document.getElementsByClassName('btmslot_a-container')[0].style.display = 'none' ;"
                     "document.getElementsByClassName('carousel-title')[1].style.display = 'none' ;"
                     "document.getElementsByClassName('cB-hook')[0].style.display = 'none' ;"
                     "document.getElementsByClassName('mpuslot_b-container')[0].style.display = 'none' ;"
                     "document.getElementById('onetrust-consent-sdk').style.display = 'none' ;"
                     "document.getElementsByClassName('res_cell_left')[0].style.width = '0' ;"
                     "document.getElementsByClassName('topslot_container')[0].style.height = '0' ;"
                     "document.querySelector('main').style.padding = '0' ;"
                     "document.querySelector('footer').style.display = 'none' ;"
                     ))

(provide 'popweb-dict-collins)

;;; popweb-dict-collins.el ends here
