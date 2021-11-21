;; The code is extracted from https://github.com/fuxialexander/emacs-webkit-katex-render

(defvar webkit-katex-render--previous-math nil)
(defvar webkit-katex-render--org-math-preprocess-function
  'webkit-katex-render--org-math-preprocess)

(defun webkit-katex-render--tex-math-preprocess (math type)
  "Preprocess current MATH environment with TYPE."
  (cond ((eq type 'sw-on)
         (setq math (substring math 2 -2)))
        ((eq type 'sw-toggle)
         (if (string-prefix-p "$$" math)
             (setq math (substring math 2 -2))
           (setq math (substring math 1 -1))))
        ((eq type 'env-on)
         (setq math (replace-regexp-in-string
                     "\\\\"
                     "\\"
                     math t t))
         (setq math
               (replace-regexp-in-string
                "begin{equation}\\|begin{align}\\|begin{align\\*}"
                "begin{aligned}"
                math))
         (setq math
               (replace-regexp-in-string
                "end{equation}\\|end{align}\\|end{align\\*}"
                "end{aligned}"
                math)))
        (t math))
  (setq math
        (replace-regexp-in-string
         "symbf"
         "mathbf"
         math))
  math)

(defun webkit-katex-render--tex-math-at-point ()
  "Mark current math environment."
  (if (texmathp)
      (let* ((string (car texmathp-why))
             (pos (cdr texmathp-why))
             (reason (assoc string texmathp-tex-commands1))
             (type (cadr reason)))
        (cond
         ((eq type 'env-on) ;; environments equation, align, etc.
          (progn
            (let ((cur (point))
                  (count 1)
                  end)
              ;; Only change point and mark after beginning and end were found.
              ;; Point should not end up in the middle of nowhere if the search fails.
              (save-excursion
                (goto-char pos)
                (forward-char 1)
                (dotimes (c count) (LaTeX-find-matching-end))
                (setq end (line-beginning-position 2))
                (goto-char pos)
                (forward-char 1)
                (dotimes (c count) (LaTeX-find-matching-begin))
                (setq beg (point))
                (list end
                      (webkit-katex-render--tex-math-preprocess
                       (buffer-substring-no-properties beg end) type))))))
         ;; ((eq type 'arg-on) ;; \ensuremath etc.
         ;;  (goto-char pos)
         ;;  (set-mark (point))
         ;;  (forward-sexp 2)
         ;;  (exchange-point-and-mark))
         ((eq type 'sw-toggle) ;; $ and $$
          (save-excursion
            (let ((end (scan-sexps pos 1)))
              (list end (webkit-katex-render--tex-math-preprocess
                         (buffer-substring-no-properties pos end) type)))))
         ((eq type 'sw-on) ;; \( and \[
          (save-excursion
            (let ((end (re-search-forward texmathp-onoff-regexp)))
              (list end
                    (webkit-katex-render--tex-math-preprocess
                     (buffer-substring-no-properties pos end)
                     type)))))))
    nil))

(defun webkit-katex-render--org-math-preprocess (math type)
  (if (eq type 'latex-fragment)
      (if (and (string-prefix-p "$" math)
               (not (string-prefix-p "$$" math)))
          (setq math (substring math 1 -1))
        (setq math (substring math 2 -2)))
    (if (eq type 'latex-environment)
        (progn
          (setq math
                (replace-regexp-in-string
                 "begin{equation}\\|begin{align}\\|begin{align\\*}"
                 "begin{aligned}"
                 math))
          (setq math
                (replace-regexp-in-string
                 "end{equation}\\|end{align}\\|end{align\\*}"
                 "end{aligned}"
                 math)))))
  math)

(defun webkit-katex-render--org-math-at-point ()
  (if (and (org-in-src-block-p t)
           (string= (upcase (cadr (nth 1 (org-element-at-point)))) "LATEX"))
      (webkit-katex-render--tex-math-at-point)
    (if (org-inside-LaTeX-fragment-p)
        (let (beg end)
          (let ((datum (org-element-context)))
            (when (memq (org-element-type datum)
                        '(latex-environment latex-fragment))
              (setq beg (org-element-property :begin datum))
              (setq end (org-element-property :end datum))
              (save-excursion
                (goto-char beg)
                (let* ((context (org-element-context))
                       (type (org-element-type context)))
                  (when (memq type '(latex-environment latex-fragment))
                    (let ((value (org-element-property :value context))
                          (beg (org-element-property :begin context))
                          (end (save-excursion
                                 (goto-char (org-element-property :end context))
                                 (skip-chars-backward " \r\t\n")
                                 (point))))
                      (goto-char end)
                      (list end (funcall webkit-katex-render--org-math-preprocess-function
                                         value type)))))))))

      nil)))

(defun webkit-katex-render--math-at-point ()
  "Return recognized math at point."
  (interactive)
  (condition-case err
      (or (and (equal major-mode 'latex-mode)
               (webkit-katex-render--tex-math-at-point))
          (and (equal major-mode 'org-mode)
               (webkit-katex-render--org-math-at-point)))
    (error
     (message "[Error] webkit-katex-render--math-at-point, %s" (error-message-string err))
     nil)))

(provide 'math-at-point)
