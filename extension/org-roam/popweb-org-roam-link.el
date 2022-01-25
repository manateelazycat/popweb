;;; popweb-org-roam-link.el --- Org-Roam ID link and footnote link previewer
;;
;; Author: c <c@MacBook-Pro.local>
;; Copyright © 2022, c, all rights reserved.
;; Created: 25 January 2022
;;
;;; Commentary:
;;
;;
;;
;;; Code:

(require 'ox)
(require 'org-transclusion)
(require 'popweb)


(defvar org-roam-link-preview--previous-html nil)
(defvar popweb-org-roam-link-preview-window-visible-p nil
  "Non-nil if popweb-org-roam-link popup is at the foreground.")

(setq popweb-org-roam-link-module-path (concat (file-name-directory load-file-name) "popweb-org-roam-link.py"))
;; (defcustom popweb-org-roam-link-index-path (format "file:%s" (plist-get (cdr (car org-publish-project-alist)) :publishing-directory))
;;   "Default org publishing-directory"
;;   :type '(string))

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
  (let* ((popweb-org-roam-link-index-path (format "file:%s" (file-truename (plist-get (cdr (car org-publish-project-alist)) :base-directory))))
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
