(defvar succor-mode nil)
(defvar succor-mode-map nil)
(defvar *succor-directory* (expand-file-name "~/.succor/"))
(defvar *succor-current-project* nil)
(defvar *succor-work-directory* nil)
(defvar *succor-file-extension* ".org")
(defvar *succor-note-window* nil)

(if (not (assq 'succor-mode minor-mode-alist))
     (setq minot-mode-alist
           (cons '(succor-mode "Succor-mode")
                 minor-mode-alist)))

(defun succor-mode (&optional arg)
  "succor-minor-mode"
  (interactive)
  (cond
   ((< (prefix-numeric-value arg) 0)
    (setq succor-mode nil)
    (succor-deactivate-advice)
    )
   (arg
    (setq succor-mode t)
    (succor-activate-advice)
    )
   (t
    (if succor-mode
        (succor-deactivate-advice)
      (succor-initialize))
    (setq succor-mode (not succor-mode))))
  (if succor-mode
      nil))

(defun succor-initialize ()
  (let* ((rootpath (gtags-get-rootpath))
         (*succor-current-project*
          (progn (string-match "^/.*/\\(.*\\)/$" rootpath)
                 (match-string 1  rootpath))))
    (setq *succor-work-directory*
          (concat *succor-directory* *succor-current-project* "/"))
    (unless (file-exists-p *succor-work-directory*)
        (make-directory *succor-work-directory*))
    (ad-activate-regexp "gtags-find-tag-after-hook")
    (ad-activate-regexp "gtags-pop-stack-after-hook")))

(defun succor-deactivate-advice ()
  (ad-deactivate-regexp "gtags-find-tag-after-hook")
  (ad-deactivate-regexp "gtags-pop-stack-after-hook"))

(defun succor-define-mode-map ()
  "キーマップ `succor-define-mode-map' を定義する。"
  (unless (keymapp succor-mode-map)
    (setq succor-mode-map (make-sparse-keymap))
    (setq minor-mode-map-alist
          (cons (cons 'succor-mode succor-mode-map)
                minor-mode-map-alist))))


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

;;(ad-disable-regexp  "gtags-find-tag-after-hook")

(defun succor-pop-stack (args)
  "gtags-pop-stackで戻った関数のメモにジャンプする．メモに関数がまだ記録されていない場合は見出しを作成する"
  (if (equal which-function-mode nil)
      (which-function-mode t))
  (let* ((tag-name args)
         (source-buffer (buffer-name gtags-current-buffer))
         (line (which-function))
         (path (concat *succor-work-directory*
                       (if (string-match "\*.*\* (.*)\\(.*\\)<.*>" source-buffer)
                           (match-string 1 source-bufer)
                         source-buffer)
                       *succor-file-extension*))
         (buf (current-buffer))
         (note-buffer (find-file-noselect path))
         (link (org-store-link nil))
         (win (selected-window)))
;;    (save-selected-window
    (select-window *succor-note-window*)
    (set-window-buffer (selected-window) note-buffer)
    ;; (switch-to-buffer-other-window note-buffer)
    ;; (with-current-buffer note-buffer
    (goto-char (point-min))
    (when (equal (re-search-forward line nil t) nil)
      (goto-char (point-max))
      (save-excursion
        (insert (concat "* " tag-name "\n"))
        (org-entry-put (point) "LINK" link)
        (org-entry-put (point) "TIME" (format-time-string "<%Y-%m-%d %a %H:%M:%S>" (current-time)))))
    (recenter 0)
    (select-window win)))

(defun succor-find-tag (args)
  "gtags-find-tagで検索した関数のメモにジャンプする．メモに関数がまだ記録されていない場合は見出しを作成する"
  (if (equal which-function-mode nil)
      (which-function-mode t))
  (let* ((tag-name args)
         (source-buffer (buffer-name gtags-current-buffer))
         (line (buffer-substring (line-beginning-position) (line-end-position)))
         (path (concat *succor-work-directory*
                       (if (string-match "\*.*\* (.*)\\(.*\\)<.*>" source-buffer)
                           (match-string 1 source-bufer)
                         source-buffer)
                       *succor-file-extension*))
         (buf (current-buffer))
         (note-buffer (find-file-noselect path))
         (link (org-store-link nil)))
    (save-selected-window
      (switch-to-buffer-other-window note-buffer)
      (setq *succor-note-window* (selected-window))
      ;; (with-current-buffer note-buffer
      (goto-char (point-min))
      (when (equal (re-search-forward (concat tag-name "$") nil t) nil)
        (goto-char (point-max))
        (save-excursion
          (insert (concat "* " tag-name "\n"))
          (org-entry-put (point) "LINK" link)
          (org-entry-put (point) "TIME" (format-time-string "<%Y-%m-%d %a %H:%M:%S>" (current-time)))))
      (recenter 0))))

(add-hook 'gtags-find-tag-after-hook 'succor-find-tag)
(add-hook 'gtags-pop-stack-after-hook 'succor-pop-stack)


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

(defvar succor-link nil)
(defvar succor-line-num nil)
(defun succor-capture-get-prefix (lang)
  (concat "[" lang "]"
          "[" (file-name-nondirectory (buffer-file-name)) "]"))


(defun succor-capture ()
  (interactive)
  (add-hook 'org-capture-mode-hook 'succor-insert-properties)
  (if (equal which-function-mode nil)
      (which-function-mode t))
  (let* ((prefix (succor-capture-get-prefix (substring (symbol-name major-mode) 0 -5)))
         (tag-name (or (which-function) ""))
         (path (concat *succor-work-directory* (buffer-name (current-buffer)) *succor-file-extension*))
         (org-capture-templates
          (if (string= "" tag-name)
              `(("r" "CodeReading" entry (file ,path ,tag-name)  "* %(identity prefix)%?\n   \n"))
            `(("r" "CodeReading" entry (file+headline ,path ,tag-name)  "* %(identity prefix)%?\n   \n")))))
    (setq succor-line-num (count-lines (point-min) (point)))
    (setq succor-link (org-store-link nil))
    (org-capture nil "r"))
  (remove-hook 'org-capture-mode-hook 'succor-insert-properties))


(defun succor-insert-properties ()
  (org-entry-put (point) "LINK" succor-link)
  (org-entry-put (point) "LINE" (number-to-string succor-line-num))
  (org-entry-put (point) "TIME" (format-time-string "<%Y-%m-%d %a %H:%M:%S>" (current-time))))



(succor-define-mode-map)
(define-key succor-mode-map "\C-c\C-r" 'succor-capture)
(provide 'succor)