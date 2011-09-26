(defvar *succor-directory* (expand-file-name "~/.succor/"))
(defvar *succor-file-extension* ".org")

(defadvice gtags-find-tag (around gtags-find-tag-after-hook)
  "Add hook."
  (let ((name (gtags-current-token))
        (line (buffer-substring (line-beginning-position) (line-end-position))))
    ad-do-it
    (run-hook-with-args 'gtags-find-tag-after-hook name)))

(defadvice gtags-pop-stack (around gtags-pop-stack-after-hook)
  "Add hook"
  (let ((name (gtags-current-token))
        (line (buffer-substring (line-beginning-position) (line-end-position))))
    ad-do-it
    (run-hook-with-args 'gtags-pop-stack-after-hook name)))

(ad-activate-regexp "gtags-find-tag-after-hook")
(ad-activate-regexp "gtags-pop-stack-after-hook")
;;(ad-disable-regexp  "gtags-find-tag-after-hook")

(defun succor-find-tag (args)
  "gtags-find-tagで検索した関数のメモにジャンプする．メモに関数がまだ記録されていない場合は見出しを作成する"
  (let* ((tag-name args)
         (source-buffer (buffer-name gtags-current-buffer))
         (line (buffer-substring (line-beginning-position) (line-end-position)))
         (path (concat *succor-directory*
                       (if (string-match "\*.*\* (.*)\\(.*\\)<.*>" source-buffer)
                           (match-string 1 source-bufer)
                         source-buffer)
                       *succor-file-extension*))
         (note-buffer (find-file-noselect path))
         (link (org-store-link nil)))
    (with-current-buffer note-buffer
      (goto-char (point-min))
      (when (equal (re-search-forward tag-name nil t) nil)
        (goto-char (point-max))
        (insert (concat "* " tag-name "\n"))
        (org-entry-put (point) "LINK" link)
        (org-entry-put (point) "MTIME" (format-time-string "<%Y-%m-%d %a %H:%M:%S>" (current-time)))))
    (display-buffer note-buffer)))

(add-hook 'gtags-find-tag-after-hook 'succor-find-tag)
(add-hook 'gtags-pop-stack-after-hook 'succor-find-tag)


;; (defun org-code-reading-lookup (&optional change-buffer?)
;;   (interactive)
;;   (let* ((func-name (which-function))
;;         (path (concat "~/code-reading/" (buffer-name (current-buffer)) ".org"))
;;         (buf (find-file-noselect path)))
;;     (if (equal change-buffer? nil)
;;         (progn (pop-to-buffer buf)
;;                (goto-char (point-min))
;;                (re-search-forward func-name nil t) nil)
;;       (progn
;;         (with-current-buffer buf
;;           (re-search-forward func-name nil t))
;;         (display-buffer buf)))))

(defun succor-capture-get-prefix (lang)
  (concat "[" lang "]"
          "[" (file-name-nondirectory (buffer-file-name)) "]"))

(defun succor-capture ()
  (interactive)
  (if (equal which-function-mode nil)
      (which-function-mode t))
  (let* ((prefix (succor-capture-get-prefix (substring (symbol-name major-mode) 0 -5)))
         (tag-name (which-function))
         (path (concat *succor-directory* (buffer-name (current-buffer)) *succor-file-extension*))
         (org-capture-templates
          `(("r" "CodeReading" entry (file+headline ,path ,tag-name)  "* %(identity prefix)%?\n   \n   %a\n   %t"))))
    (org-capture nil "r")))

(global-set-key (kbd "C-c C-r")'succor-capture)

