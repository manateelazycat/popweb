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
;; Package-Requires: ((org) (org-roam "20220121.2350") (org-transclusion "20220114.9") (ivy "20211231.1730") (s "1.12.0") (dash "20210826.1149") (ox-hugo))
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
(require 'ox-hugo)
(require 's)
(require 'dash)
(require 'org-roam)
(require 'org-transclusion)
(require 'popweb)
(require 'ivy)

;;; Code:
(defvar popweb-org-roam-link-preview--previous-html nil)

(defvar popweb-org-roam-link-preview-window-visible-p nil
  "Non-nil if popweb-org-roam-link popup is at the foreground.")

(defvar org-roam-node-ivy-read-result nil)

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
              (
               (mkr (or (ignore-errors (org-id-find id t))
                        (with-current-buffer (find-file-noselect (aref (org-roam-node-from-id id) 1))
                          (goto-char (aref (org-roam-node-from-id id) 8))
                          (point-marker))))
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
       (mkr (or (ignore-errors (org-id-find id t))
                (with-current-buffer (find-file-noselect (aref (org-roam-node-from-id id) 1))
                  (goto-char (aref (org-roam-node-from-id id) 8))
                  (point-marker))))
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
      (let ((org-export-before-processing-hook '(org-blackfriday--reset-org-blackfriday--code-block-num-backticks))) (org-export-string-as context 'html))
    (progn
      (user-error "%s" "Nothing to preview or this kind of link is not supported yet!")
      nil)))

(defun popweb-org-roam-link-preview (info)
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
         (show-window (nth 0 info))
         (html-string (nth 1 info))
         (ivy-action-x (nth 2 info))
         (new-html (not (string= html-string popweb-org-roam-link-preview--previous-html))))
    (cond ((and ivy-action-x (listp ivy-action-x))
           (setq popweb-org-roam-link-index-path (concat "file:" (file-name-directory (org-roam-node-file (cdr ivy-action-x))))))
          (t
           (setq popweb-org-roam-link-index-path (concat "file:" (file-truename default-directory)))))
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

(defun popweb-org-roam-link-show (&optional context ivy-action-x)
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
              (popweb-start 'popweb-org-roam-link-preview (list t html-string ivy-action-x))))
      (popweb-start 'popweb-org-roam-link-preview (list nil "Hello world" ivy-action-x))))
  (add-hook 'post-command-hook #'popweb-org-roam-link-preview-window-hide-after-move))

(defun org-roam-node--ivy-read-1 (&optional prompt initial-input filter-fn sort-fn require-match action caller)
  (ivy-read prompt (org-roam-node-read--completions filter-fn sort-fn)
            :require-match require-match
            :initial-input initial-input
            :action action
            :history 'org-roam-node-history
            :caller caller)
  org-roam-node-ivy-read-result)

(defun popweb-org-roam-node-preview-select-action (x)
  (cond ((and x (listp x))
         (let ((node (cdr x)))
           (setq org-roam-node-ivy-read-result node)
           (popweb-org-roam-link-show (get-org-context-from-org-id-link (org-roam-node-id node)) x)))
        ((stringp x)
         (setq org-roam-node-ivy-read-result (org-roam-node-create :title x)))))

(defun popweb-org-roam-link-preview-select ()
  (interactive)
  (ivy-read "Select a link to preview: " (append (find-org-id-links) (find-footnotes))
            :action (lambda (link) (cond ((string= "ID link" (elt link 1))
                                     (popweb-org-roam-link-show (get-org-context-from-org-id-link (elt link 4))))
                                    ((string= "Footnote" (elt link 1))
                                     (popweb-org-roam-link-show (get-org-context-from-footnote (elt link 4))))))))

(defun popweb-org-roam-node-preview-select (&optional initial-input filter-fn sort-fn require-match prompt)
  (interactive)
  (org-roam-node--ivy-read-1 (or prompt "Select a node to preview: ") initial-input filter-fn sort-fn require-match
                             #'popweb-org-roam-node-preview-select-action
                             'popweb-org-roam-node-preview-select))

(defun popweb-org-roam-node-backlinks-preview ()
  (interactive)
  (let* ((id (ignore-errors
                    (save-match-data
                      (org-roam-node-id (org-roam-node-at-point)))))
         (node (if id (org-roam-node-from-id id) (org-roam-node-at-point t)))
         (backlinks (--filter (->> (org-roam-backlink-source-node it)
                                        (org-roam-node-file)
                                        (s-contains? "private/") (not))
                              (org-roam-backlinks-get node)))
         (content-and-footnote-string-list
          (-map (lambda (backlink)
                  (let* ((source-node (org-roam-backlink-source-node backlink))
                         (source-file (org-roam-node-file source-node))
                         (properties (org-roam-backlink-properties backlink))
                         (outline (if-let ((outline (plist-get properties :outline)))
                                      (mapconcat #'org-link-display-format outline " > ")))
                         (point (org-roam-backlink-point backlink))
                         (text (org-roam-preview-get-contents
                                                    source-file
                                                    point))
                         (content (format "%s [[id:%s][%s]]\n%s\n%s"
                                            (s-repeat (+ (org-roam-node-level node) 2) "*")
                                            (org-roam-node-id source-node)
                                            (org-roam-node-title source-node)
                                            (if outline (format "%s (/%s/)"
                                                                (s-repeat (+ (org-roam-node-level node) 3) "*") outline) "")
                                            text))
                         (label-list (with-temp-buffer
                                       (insert-file-contents source-file)
                                       (org-element-map (org-element-parse-buffer) 'footnote-reference
                                         (lambda (reference)
                                           (org-element-property :label reference)))))
                         (footnote-list
                          (with-temp-buffer
                            (insert-file-contents source-file)
                            (-map (lambda (label) (buffer-substring-no-properties
                                                   (nth 1 (org-footnote-get-definition label))
                                                   (nth 2 (org-footnote-get-definition label))))
                                  label-list)))
                         (footnote-string-list (string-join footnote-list "\n"))
                         (content-and-footnote-string (format "%s\n%s" content footnote-string-list)))
                    content-and-footnote-string)
                  ) backlinks)))
    (popweb-org-roam-link-show (string-join content-and-footnote-string-list "\n"))))

(defun popweb-org-roam-link-preview-window-hide-after-move ()
  (when (and popweb-org-roam-link-preview-window-visible-p (popweb-epc-live-p popweb-epc-process))
    (setq org-roam-link-preview--previous-html nil)
    (setq popweb-org-roam-link-preview-window-visible-p nil)
    (ignore-errors
      (popweb-call-async "hide_web_window" "org_roam"))
    (remove-hook 'post-command-hook #'popweb-org-roam-link-preview-window-hide-after-move)))

(provide 'popweb-org-roam-link)
;;; popweb-org-roam-link.el ends here
