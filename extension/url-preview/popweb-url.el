;;; popweb-url.el --- Plugin view url for popweb     -*- lexical-binding: t; -*-

;; Copyright (C) 2022  Qiqi Jin

;; Author: Qiqi Jin;;; Require <ginqi7@gmail.com>
;; Keywords: lisp

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;;

;;; Code:

(require 'popweb)

(setq popweb-url-module-path
      (concat (file-name-directory load-file-name) "popweb-url.py"))

(defvar popweb-url-web-window-visible-p nil)

(defvar popweb-url-regexp
  "\\b\\(\\(www\\.\\|\\(s?https?\\|ftp\\|file\\|gopher\\|gemini\\|nntp\\|news\\|telnet\\|wais\\|mailto\\|info\\):\\)\\(//[-a-z0-9_.]+:[0-9]*\\)?\\(?:[-a-z0-9_=#$@~%&*+\\/[:word:]!?:;.,]+([-a-z0-9_=#$@~%&*+\\/[:word:]!?:;.,]+)\\(?:[-a-z0-9_=#$@~%&*+\\/[:word:]!?:;.,]+[-a-z0-9_=#$@~%&*+\\/[:word:]]\\)?\\|[-a-z0-9_=#$@~%&*+\\/[:word:]!?:;.,]+[-a-z0-9_=#$@~%&*+\\/[:word:]]\\)\\)"
  "Regexp check for url.")

(defcustom popweb-url-web-window-width-scale 0.8
  "The popup window's width scale of Emacs's."
  :type '(float))

(defcustom popweb-url-web-window-height-scale 0.45
  "The popup window's height scale of Emacs's."
  :type '(float))

(defcustom popweb-url-web-window-width-absolute 480
  "The popup windows's absolute width in pixels."
  :type '(int))

(defcustom popweb-url-web-window-height-absolute 270
  "The popup windows's absolute height in pixels."
  :type '(int))

(defcustom popweb-url-web-window-size-use-absolute t
  "Wether to use absolute or relative size for the popup window."
  :type '(bool))

(defun popweb-url-local-file-url-completion (url)
  "If URL is local file, complete it's absolute path."
  (let ((current-path
         (when (buffer-file-name)
           (file-name-directory (buffer-file-name)))))
    (replace-regexp-in-string "^file:"
                              (concat "file:" current-path)
                              url)))
(defun popweb-get-url()
  "Get url.
1. If current point on a url, get the url.
2. If current poin befor a url, get the url.
3. Get the first url.
4. return nil"
  (let* ((url (thing-at-point 'url))
         (url
          (if url url
            (popweb-current-line-url
             (- (point) (line-beginning-position)))))
         (url (if url url (popweb-current-line-url 0))))
    (when url (popweb-url-local-file-url-completion url))))

(defun popweb-current-line-url (start)
  "Return current line url after START."
  (let* ((current-line (thing-at-point 'line t)))
    (when (string-match popweb-url-regexp current-line start)
      (match-string 0 current-line))))

(defun popweb-url-prompt-input (prompt)
  "PROMPT input url for preview."
  (read-string
   (format "%s(%s): " prompt (or (popweb-get-url) ""))
   nil nil
   (popweb-get-url)))

(defun popweb-url-web-window-hide-after-move ()
  "Hide popweb window after move."
  (when (and popweb-url-web-window-visible-p
             (popweb-epc-live-p popweb-epc-process))
    (popweb-call-async "hide_web_window" "url-preview")
    (setq popweb-url-web-window-visible-p nil)
    (remove-hook 'post-command-hook #'popweb-dict-bing-web-window-hide-after-move)))

(defun popweb-url (info)
  "Pop window to show current url preview INFO."
  (let* ((position (popweb-get-cursor-coordinate))
         (window-header-height
          (-
           (nth 1 (window-inside-pixel-edges))
           (nth 1 (window-absolute-pixel-edges))))
         (x (car position))
         (y (- (cdr position) window-header-height))
         (x-offset (popweb-get-cursor-x-offset))
         (y-offset (popweb-get-cursor-y-offset))
         (frame-x (car (frame-position)))
         (frame-y (cdr (frame-position)))
         (frame-w (frame-outer-width))
         (frame-h (frame-outer-height))
         (url (nth 0 info)))
    (popweb-call-async "call_module_method" popweb-url-module-path
                       "pop_url_window"
                       (list
                        "url-preview"
                        x y x-offset y-offset
                        frame-x frame-y frame-w frame-h
                        popweb-url-web-window-width-scale
                        popweb-url-web-window-height-scale
                        popweb-url-web-window-width-absolute
                        popweb-url-web-window-height-absolute
                        popweb-url-web-window-size-use-absolute
                        url))
    (popweb-url-web-window-can-hide)))

(defun popweb-url-input (&optional url)
  "Input URL for preview."
  (interactive)
  (popweb-start 'popweb-url
                (list
                 (or url (popweb-url-prompt-input "preview url: "))))
  (add-hook 'post-command-hook #'popweb-url-web-window-hide-after-move))

(defun popweb-url-preview-pointer()
  "Preview current pointer url."
  (interactive)
  (popweb-start 'popweb-url (list (popweb-get-url)))
  (add-hook 'post-command-hook #'popweb-url-web-window-hide-after-move))

(defun popweb-url-web-window-can-hide ()
  "Make web window can hide on idle time."
  (run-with-timer 1 nil
                  (lambda ()
                    (setq popweb-url-web-window-visible-p t))))
(provide 'popweb-url)
;;; popweb-url.el ends here
