;;; popweb-anki-review.el --- Anki review popup
;; Filename: popweb-anki-review.el
;; Description: Anki note review
;; Author: Andy Stewart <lazycat.manatee@gmail.com>
;; Maintainer: Andy Stewart <lazycat.manatee@gmail.com>
;; Copyright (C) 2021, Andy Stewart, all rights reserved.
;; Created: 2022-01-25-23:00:00
;; Version: 0.1
;; URL: https://www.github.com/manateelazycat/popweb
;; Keywords:
;; Compatibility: GNU Emacs 29.0.50
;;
;; Features that might be required by this library:
;; Package-Requires:
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
;; Put popweb-anki-review.el to your load-path.
;; The load-path is usually ~/elisp/.
;; It's set in your ~/.emacs like this:
;; (add-to-list 'load-path (expand-file-name "~/elisp"))
;;
;; And the following to your ~/.emacs startup file.
;;
;; (require 'popweb-anki-review)
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
(require 'popweb)

;;; Code:
(defvar popweb-anki-review-preview--previous-query nil)

(defvar popweb-anki-review-preview-window-visible-p nil
  "Non-nil if popweb-anki-review popup is at the foreground.")

(defvar popweb-anki-review-tooltip-file (expand-file-name "tooltip.js" (if load-file-name
                                                                        (file-name-directory load-file-name)
                                                                      default-directory)))

(defcustom popweb-anki-review-popup-window-width-scale 0.8
  "The popup window's width scale of Emacs's."
  :type '(float))

(defcustom popweb-anki-review-popup-window-height-scale 0.5
  "The popup window's height scale of Emacs's."
  :type '(float))

(defcustom popweb-anki-review-media-directory nil
  "Anki media directory."
  :type '(string))

(defcustom popweb-anki-review-callback nil
  "Customize callback function."
  :type '(string))

(defcustom popweb-anki-review-eww-sentence-abbrevs '("i.e." "etc." "U.S.")
  "Prevent to incorrectly determine sentence end."
  :type '(repeat string)
  :group 'popweb-anki-review)

(defcustom popweb-anki-review-eww-sentence-ends (rx (or
                                                     (and "." (or " " eol))
                                                     (and "?" (or " " eol))
                                                     (and "!" (or " " eol))
                                                     (and ";" (or " " eol))
                                                     "\n\n"))
  "A regexp used to determine where is the end of a sentence in eww."
  :type 'string
  :group 'popweb-anki-review)

(setq popweb-anki-review-index-path (concat (file-name-directory load-file-name) "index.html"))
(setq popweb-anki-review-module-path (concat (file-name-directory load-file-name) "popweb-anki-review.py"))

(defun popweb-anki-review--sentence ()
  (let (sentence)
    (cond
     ((derived-mode-p 'eww-mode)
      (setq sentence (popweb-anki-review--eww-sentence)))
     (t
      (setq sentence (thing-at-point 'sentence t))))
    sentence))

(defun popweb-anki-review--string-ends-with-any (str patterns)
  (cl-dolist (p patterns)
    (when (string-suffix-p p str t)
      (cl-return t))))

(defun popweb-anki-review--eww-sentence ()
  (let ((sentence-ends popweb-anki-review-eww-sentence-ends)
        (point (point))
        (stop nil)
        start end)
    (save-excursion
      (while (not stop)
        (setq end (search-forward-regexp sentence-ends nil t))
        ;; (message "end: %s" end)
        (if (not end)
            (setq end (point-max)
                  stop t)
          (unless (popweb-anki-review--string-ends-with-any (buffer-substring-no-properties point (- end 1)) popweb-anki-review-eww-sentence-abbrevs)
            (setq stop t))))

      (setq stop nil)
      (goto-char point)
      (while (not stop)
        (setq start (search-backward-regexp sentence-ends nil t))
        ;; (message "start: %s" start)
        (if (not start)
            (setq start (point-min)
                  stop t)
          (unless (popweb-anki-review--string-ends-with-any (buffer-substring-no-properties (point-at-bol) (1+ start)) popweb-anki-review-eww-sentence-abbrevs)
            (setq stop t)
            (setq start (1+ start))))))
    (string-trim (buffer-substring-no-properties start end))))

(defun popweb-anki-review--sentence ()
  (let (sentence)
    (cond
     ((derived-mode-p 'eww-mode)
      (setq sentence (popweb-anki-review--eww-sentence)))
     (t
      (setq sentence (thing-at-point 'sentence t))))
    sentence))

(defun popweb-anki-review-region-or-sentence ()
  "Return region or sentence around point.
If `mark-active' on, return region string.
Otherwise return sentence around point."
  (if mark-active
      (buffer-substring-no-properties (region-beginning)
                                      (region-end))
    (popweb-anki-review--sentence)))

(defun popweb-anki-review-preview (info)
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
         (emacs-query (nth 1 info))
         (script-file popweb-anki-review-tooltip-file)
         (media-directory popweb-anki-review-media-directory)
         (callback popweb-anki-review-callback)
         (new-query-p (not (string= emacs-query popweb-anki-review-preview--previous-query))))
    (popweb-call-sync "call_module_method" popweb-anki-review-module-path
                       "pop_anki_review_window"
                       (list
                        popweb-anki-review-module-path
                        "anki_review"
                        popweb-anki-review-index-path
                        x y x-offset y-offset
                        frame-x frame-y frame-w frame-h
                        popweb-anki-review-popup-window-width-scale
                        popweb-anki-review-popup-window-height-scale
                        show-window script-file media-directory
                        new-query-p emacs-query
                        callback))
    (popweb-anki-review-preview-window-can-hide)))

(defun popweb-anki-review-preview-window-can-hide ()
  (run-with-timer 1 nil (lambda () (setq popweb-anki-review-preview-window-visible-p t))))

(defun popweb-anki-review-show (&optional arg)
  (interactive "P")

  (setq popweb-anki-review-sentence (if (or mark-active arg) (or (popweb-anki-review-region-or-sentence) "") ""))

  (while (string-equal popweb-anki-review-sentence "")
    (setq popweb-anki-review-sentence (read-string "Please input the word or sentence to review: "
                                                   nil nil "" nil)))

  (setq popweb-anki-review-sentence (replace-regexp-in-string "[\t\n\r]+" " " popweb-anki-review-sentence))

  (message "[popweb-anki-review] to review: '%s'" popweb-anki-review-sentence)

  (if popweb-anki-review-sentence
    (progn
      (setq popweb-anki-review-preview--previous-query popweb-anki-review-sentence)
      (if popweb-anki-review-preview-window-visible-p
          (progn
            (setq popweb-anki-review-preview-window-visible-p nil)
            (ignore-errors
                      (popweb-call-async "hide_web_window" "anki_review"))))
              (popweb-start 'popweb-anki-review-preview (list t popweb-anki-review-sentence)))
      (popweb-start 'popweb-anki-review-preview (list nil "Hello world")))
  (add-hook 'post-command-hook #'popweb-anki-review-preview-window-hide-after-move))

(defun popweb-anki-review-preview-window-hide-after-move ()
  (when (and popweb-anki-review-preview-window-visible-p (popweb-epc-live-p popweb-epc-process))
    (setq popweb-anki-review-preview--previous-query nil)
    (setq popweb-anki-review-preview-window-visible-p nil)
    (ignore-errors
      (popweb-call-async "hide_web_window" "anki_review"))
    (remove-hook 'post-command-hook #'popweb-anki-review-preview-window-hide-after-move)))

(provide 'popweb-anki-review)
;;; popweb-anki-review.el ends here
