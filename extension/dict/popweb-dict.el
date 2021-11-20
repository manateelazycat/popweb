;;; popweb-dict.el --- Dict plugin

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


;;; Code:

(setq popweb-dict-module-path (concat (file-name-directory load-file-name) "popweb-dict.py"))

(defun popweb-dict-say-word (word)
  (if (featurep 'cocoa)
      (call-process-shell-command
       (format "say %s" word) nil 0)
    (let ((player (or (executable-find "mpv")
                      (executable-find "mplayer")
                      (executable-find "mpg123"))))
      (if player
          (start-process
           player
           nil
           player
           (format "http://dict.youdao.com/dictvoice?type=2&audio=%s" (url-hexify-string word)))
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

(provide 'popweb-dict)

;;; popweb-dict.el ends here
