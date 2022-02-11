;;; popweb-org-roam-link.el --- Org-Roam ID link and footnote link previewer
;; Filename: popweb-org-roam-link.el
;; Description: Org-Roam ID link and footnote link previewer
;; Author: Andy Stewart <lazycat.manatee@gmail.com>
;; Maintainer: Andy Stewart <lazycat.manatee@gmail.com>
;; Copyright (C) 2021, Andy Stewart, all rights reserved.
;; Created: 2022-01-25-23:00:00
;; Version: 0.1
;; URL: https://www.github.com/manateelazycat/popweb-dict
;; Keywords:
;; Compatibility: GNU Emacs 29.0.50
;;
;; Features that might be required by this library:
;; Package-Requires: ((org) (org-roam "20220121.2350") (org-transclusion "20220114.9") (ivy "20211231.1730") (dash "20210826.1149"))
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
(require 'dash)
(require 'org-roam)
(require 'org-transclusion)
(require 'popweb)
(require 'ivy)

;;; Code:
(defvar popweb-org-roam-link-preview--previous-html nil)

(defvar popweb-org-roam-link-preview-window-visible-p nil
  "Non-nil if popweb-org-roam-link popup is at the foreground.")

(defcustom popweb-org-roam-link-popup-window-width-scale 0.8
  "The popup window's width scale of Emacs's"
  :type '(float))

(defcustom popweb-org-roam-link-popup-window-height-scale 0.5
  "The popup window's height scale of Emacs's"
  :type '(float))

(setq popweb-org-roam-link-module-path (concat (file-name-directory load-file-name) "popweb-org-roam-link.py"))

(defun get-org-context-at-point ()
  (save-excursion
    (let* ((plist)
           (link (org-element-context))
           (id (org-element-property :path link))
           (label (org-element-property :label link)))
      (cond ((and (string= "id" (org-element-property :type link)) id)
             (let*
              ((mkr (org-id-find id t))
               (context (org-element-context (plist-get (org-transclusion-content-org-marker mkr plist) :src-content)))
               (label-list (with-temp-buffer
                             (insert context)
                             (org-element-map (org-element-parse-buffer) 'footnote-reference
                               (lambda (reference)
                                 (org-element-property :label reference)))))
               )
              (with-temp-buffer
                (insert-file-contents (aref (org-roam-node-from-id id) 1))
                (-map (lambda (label)
                        (setq context
                              (concat context (buffer-substring-no-properties
                                               (nth 1 (org-footnote-get-definition label))
                                               (nth 2 (org-footnote-get-definition label))))))
                      label-list))
              context))
           (label
            (let*
              ((footnote (buffer-substring-no-properties
                                      (nth 1 (org-footnote-get-definition label))
                                      (nth 2 (org-footnote-get-definition label)))))
              (replace-regexp-in-string "^\\[fn:\\([0-9]+\\)\\]" "" footnote)))
           (t
            "")))))

(defun get-org-context-from-org-id-link (id)
  (let*
      ((plist)
       (mkr (org-id-find id t))
       (context (org-element-context (plist-get (org-transclusion-content-org-marker mkr plist) :src-content)))
       (label-list (with-temp-buffer
                     (insert context)
                     (org-element-map (org-element-parse-buffer) 'footnote-reference
                       (lambda (reference)
                         (org-element-property :label reference))))))
    (with-temp-buffer
      (insert-file-contents (aref (org-roam-node-from-id id) 1))
      (-map (lambda (label)
              (setq context
                    (concat context (buffer-substring-no-properties
                                     (nth 1 (org-footnote-get-definition label))
                                     (nth 2 (org-footnote-get-definition label))))))
            label-list))
    context))

(defun get-org-context-from-footnote (label)
  (let*
      ((footnote (buffer-substring-no-properties
                  (nth 1 (org-footnote-get-definition label))
                  (nth 2 (org-footnote-get-definition label)))))
    (replace-regexp-in-string "^\\[fn:\\([0-9]+\\)\\]" "" footnote)))

(defun find-org-id-links ()
  (let* ((link-list (org-element-map (org-element-parse-buffer) 'link
                      (lambda (link)
                        (when (string= (org-element-property :type link) "id")
                          (list (concat "↑ "
                                         (if (and (org-element-property :contents-begin link)
                                                  (org-element-property :contents-end link))
                                             (buffer-substring-no-properties
                                              (org-element-property :contents-begin link)
                                              (org-element-property :contents-end link))
                                           (org-element-property :path link)))
                                "ID link"
                                (org-element-property :begin link)
                                (org-element-property :end link)
                                (org-element-property :path link)
                                ))))))
    link-list))

(defun find-footnotes ()
  (let* ((footnote-list (org-element-map (org-element-parse-buffer) 'footnote-reference
                         (lambda (reference)
                           (list (concat "↓ "
                                         (org-element-property :label reference))
                                 "Footnote"
                                 (org-element-property :begin reference)
                                 (org-element-property :end reference)
                                 (org-element-property :label reference)
                                 )))))
    footnote-list))

(defun get-html-from-org-context (context)
  (if (not (string-empty-p context))
      (org-export-string-as context 'html)
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
         (new-html (not (string= html-string popweb-org-roam-link-preview--previous-html))))
    (popweb-call-async "call_module_method" popweb-org-roam-link-module-path
                       "pop_org_roam_link_window"
                       (list
                        "org_roam"
                        popweb-org-roam-link-index-path
                        x y x-offset y-offset
                        frame-x frame-y frame-w frame-h
                        popweb-org-roam-link-popup-window-width-scale
                        popweb-org-roam-link-popup-window-height-scale
                        show-window new-html html-string))
    (popweb-org-roam-link-preview-window-can-hide)))

(defun popweb-org-roam-link-preview-window-can-hide ()
  (run-with-timer 1 nil (lambda () (setq popweb-org-roam-link-preview-window-visible-p t))))

(defun popweb-org-roam-link-show (&optional context)
  (interactive)
  (let* ((context-string (or context (get-org-context-at-point)))
         (html-string (get-html-from-org-context context-string)))
    (if html-string
        (if (not (eq html-string popweb-org-roam-link-preview--previous-html))
            (progn
              (setq popweb-org-roam-link-preview--previous-html html-string)
              (if popweb-org-roam-link-preview-window-visible-p
                  (progn
                    (setq popweb-org-roam-link-preview-window-visible-p nil)
                    (ignore-errors
                      (popweb-call-async "hide_web_window" "org_roam"))))
              (popweb-start 'popweb-org-roam-link-preview (list t html-string))))
      (popweb-start 'popweb-org-roam-link-preview (list nil "Hello world"))))
  (add-hook 'post-command-hook #'popweb-org-roam-link-preview-window-hide-after-move))

(defun popweb-org-roam-link-preview-select ()
  (interactive)
  (ivy-read "Select a link to preview: " (append (find-org-id-links) (find-footnotes))
            :action (lambda (link) (cond ((string= "ID link" (elt link 1))
                                     (popweb-org-roam-link-show (get-org-context-from-org-id-link (elt link 4))))
                                    ((string= "Footnote" (elt link 1))
                                     (popweb-org-roam-link-show (get-org-context-from-footnote (elt link 4))))))))

(defun popweb-org-roam-link-preview-window-hide-after-move ()
  (when (and popweb-org-roam-link-preview-window-visible-p (popweb-epc-live-p popweb-epc-process))
    (setq org-roam-link-preview--previous-html nil)
    (setq popweb-org-roam-link-preview-window-visible-p nil)
    (ignore-errors
      (popweb-call-async "hide_web_window" "org_roam"))
    (remove-hook 'post-command-hook #'popweb-org-roam-link-preview-window-hide-after-move)))

(provide 'popweb-org-roam-link)
;;; popweb-org-roam-link.el ends here
