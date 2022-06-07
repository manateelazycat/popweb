;;; Require
(require 'popweb)

(setq popweb-url-module-path (concat (file-name-directory load-file-name) "popweb-url.py"))

(defvar popweb-url-web-window-visible-p nil)


(defun popweb-url-local-file-url-completion (current-line)
  "If url is local file, complete it's absolute path"
  (let ((match-result (match-string 0 current-line))
        (current-path (when (buffer-file-name)
                        (file-name-directory (buffer-file-name)))))
        (replace-regexp-in-string "^file:" (concat "file:" current-path) match-result)))



(defun popweb-current-line-url ()
  "Return current line url."
  (let ((current-line (thing-at-point 'line t)))
    (when (string-match gnus-button-url-regexp current-line)
      (popweb-url-local-file-url-completion current-line))))


(defun popweb-url-prompt-input (prompt)
  "Prompt input url for preview ."
  (read-string (format "%s(%s): " prompt (or (popweb-current-line-url) ""))
               nil nil
               (popweb-current-line-url)))

(defun popweb-url-web-window-hide-after-move ()
  "Hide popweb window after move"
  (when (and popweb-url-web-window-visible-p (popweb-epc-live-p popweb-epc-process))
    (popweb-call-async "hide_web_window" "url-preview")
    (setq popweb-url-web-window-visible-p nil)
    (remove-hook 'post-command-hook #'popweb-dict-bing-web-window-hide-after-move)))

(defun popweb-url (info)
  "Pop window to show current url preview"
  (let* ((position (popweb-get-cursor-coordinate))
         (x (car position))
         (y (cdr position))
         (x-offset (popweb-get-cursor-x-offset))
         (y-offset (popweb-get-cursor-y-offset))
         (frame-x (car (frame-position)))
         (frame-y (cdr (frame-position)))
         (frame-w (frame-outer-width))
         (frame-h (frame-outer-height))
         (width-scale 0.35)
         (height-scale 0.5)
         (url (nth 0 info))
         )
    (popweb-call-async "call_module_method" popweb-url-module-path
                       "pop_url_window"
                       (list
                        "url-preview"
                        x y x-offset y-offset
                        frame-x frame-y frame-w frame-h
                        width-scale height-scale
                        url))
    (popweb-url-web-window-can-hide)))


(defun popweb-url-input (&optional url)
  "Input url for preview"
  (interactive)
  (popweb-start 'popweb-url (list (or url (popweb-url-prompt-input "preview url: "))))
  (add-hook 'post-command-hook #'popweb-url-web-window-hide-after-move))

(defun popweb-url-preview-pointer()
  "Input url for preview"
  (interactive)
  (popweb-start 'popweb-url (list (popweb-current-line-url)))
  (add-hook 'post-command-hook #'popweb-url-web-window-hide-after-move))


(defun popweb-url-web-window-can-hide ()
  (run-with-timer 1 nil (lambda () (setq popweb-url-web-window-visible-p t))))


(provide 'popweb-url)
