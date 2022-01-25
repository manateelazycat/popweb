;;; popweb-org-roam-link.el --- Org-Roam ID link and footnote link previewer
;; Filename: popweb-org-roam-link.el
;; Description: Org-Roam ID link and footnote link previewer
;; Author: Andy Stewart <lazycat.manatee@gmail.com>
;; Maintainer: Andy Stewart <lazycat.manatee@gmail.com>
;; Copyright (C) 2021, Andy Stewart, all rights reserved.
;; Created: 2022-01-25-23:00:00
;; Version: 0.1
;; Last-Updated: 2021-11-21 07:32:38
;;           By: Andy Stewart
;; URL: https://www.github.org/manateelazycat/popweb-dict
;; Keywords:
;; Compatibility: GNU Emacs 29.0.50
;;
;; Features that might be required by this library:
;; Package-Requires: ((org) (org-transclusion "1.2.0"))
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
;; Org-Roam ID link and footnote link previewer
;;

;;; Installation:
;;
;; Put popweb-org-roam-link.el to your load-path.
;; The load-path is usually ~/elisp/.
;; It's set in your ~/.emacs like this:
;; (add-to-list 'load-path (expand-file-name "~/elisp"))
;;
;; And the following to your ~/.emacs startup file.
;;
;; (require 'popweb-org-roam-link)
;;
;; No need more.

;;; Customize:
;;
;;

;;; Change log:
;;
;;

;;; Acknowledgements:
;;

;;; TODO
;;

;;; Require
(require 'ox)
(require 'org-transclusion)
(require 'popweb)

;;; Code:
(defvar org-roam-link-preview--previous-html nil)
(defvar popweb-org-roam-link-preview-window-visible-p nil
  "Non-nil if popweb-org-roam-link popup is at the foreground.")

(defcustom org-roam-link-popup-window-width-scale 0.8
  "The popup window's width scale of Emacs's"
  :type '(float))

(defcustom org-roam-link-popup-window-height-scale 0.5
  "The popup window's height scale of Emacs's"
  :type '(float))

(setq popweb-org-roam-link-module-path (concat (file-name-directory load-file-name) "popweb-org-roam-link.py"))

(defun link-at-point ()
  (save-excursion
    (let* ((plist)
           (link (org-element-context))
           (id (org-element-property :path link))
           (label (org-element-property :label link)))
      (cond ((and (string= "id" (org-element-property :type link)) id)
             (let*
              ((mkr (org-id-find id t))
              (content (plist-get (org-transclusion-content-org-marker mkr plist) :src-content)))
              content))
           (label
            (let*
              ((footnote (buffer-substring-no-properties
                                      (nth 1 (org-footnote-get-definition label))
                                      (nth 2 (org-footnote-get-definition label)))))
              (replace-regexp-in-string "^\\[fn:\\([0-9]+\\)\\]" "" footnote)))
           (t
            ""))
      )))

(defun get-html-from-link ()
  (if (not (string-empty-p (link-at-point)))
      (org-export-string-as (link-at-point) 'html)
    (progn
      (user-error "%s" "Nothing to preview or this kind of link is not supported yet!")
      nil)))

(defun popweb-org-roam-link-preview (info)
  (let* ((popweb-org-roam-link-index-path (concat "file:" (file-truename default-directory)))
         (position (popweb-get-cursor-coordinate))
         (x (car position))
         (y (cdr position))
         (x-offset (popweb-get-cursor-x-offset))
         (y-offset (popweb-get-cursor-y-offset))
         (frame-x (car (frame-position)))
         (frame-y (cdr (frame-position)))
         (frame-w (frame-outer-width))
         (frame-h (frame-outer-height))
         (show-window (nth 0 info))
         (html-string (nth 1 info))
         (new-html (not (string= html-string org-roam-link-preview--previous-html))))
    (popweb-call-async "call_module_method" popweb-org-roam-link-module-path
                       "pop_org_roam_link_window"
                       (list
                        "org_roam"
                        popweb-org-roam-link-index-path
                        x y x-offset y-offset
                        frame-x frame-y frame-w frame-h
                        org-roam-link-popup-window-width-scale
                        org-roam-link-popup-window-height-scale
                        show-window new-html html-string))
    (popweb-org-roam-link-preview-window-can-hide)))

(defun popweb-org-roam-link-preview-window-can-hide ()
  (run-with-timer 1 nil (lambda () (setq popweb-org-roam-link-preview-window-visible-p t))))

(defun popweb-org-roam-link-show ()
  (interactive)
  (let* ((html-string (get-html-from-link)))
    (if html-string
        (if (not (eq html-string org-roam-link-preview--previous-html))
            (progn
              (popweb-start 'popweb-org-roam-link-preview (list t html-string))
              (setq org-roam-link-preview--previous-html html-string)))
      (popweb-start 'popweb-org-roam-link-preview (list nil "Hello world"))))
  (add-hook 'post-command-hook #'popweb-org-roam-link-preview-window-hide-after-move))

(defun popweb-org-roam-link-preview-window-hide-after-move ()
  (when (and popweb-org-roam-link-preview-window-visible-p (popweb-epc-live-p popweb-epc-process))
    (setq org-roam-link-preview--previous-html nil)
    (setq popweb-org-roam-link-preview-window-visible-p nil)
    (ignore-errors
      (popweb-call-async "hide_web_window" "org_roam"))
    (remove-hook 'post-command-hook #'popweb-org-roam-link-preview-window-hide-after-move)))

(defun -posframe-tip (string)
  "Show STRING using posframe-show."
  (unless (and (require 'posframe nil t) (posframe-workable-p))
    (error "Posframe not workable"))

    (if string
        (progn
          (with-current-buffer (get-buffer-create "Org Roam Tip")
            (let ((inhibit-read-only t))
              (erase-buffer)
              (insert string)
              (goto-char (point-min))))
          (posframe-show "Org Roam Tip"
                         :left-fringe 8
                         :right-fringe 8
                         :internal-border-color (face-foreground 'default)
                         :internal-border-width 1)
          (unwind-protect
              (push (read-event) unread-command-events)
            (progn
              (posframe-delete "Org Roam Tip")
              (other-frame 0))))
      (message "Nothing to look up")))

(provide 'popweb-org-roam-link)
;;; popweb-org-roam-link.el ends here
