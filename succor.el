(defvar succor-mode nil)
(defvar succor-mode-map nil)

(defvar *succor-directory*
  (expand-file-name "~/.succor/"))

(defvar *succor-current-project* nil)
(defvar *succor-work-directory* nil)
(defvar *succor-file-extension*
  ".org")
(defvar *succor-note-window* nil)
(defvar succor-gtags-enable t)
(defvar succor-imenu-enable t)

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
    (succor-deactivate-advice))
   (arg
    (setq succor-mode t)
    (succor-initiaize))
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
    (when succor-gtags-enable
      (ad-activate-regexp "gtags-find-tag-after-hook")
      (ad-activate-regexp "gtags-pop-stack-after-hook"))
    (when succor-imenu-enable
      (ad-activate-regexp "succor-imenu-after-jump-hook"))))

(defun succor-deactivate-advice ()
  (ad-deactivate-regexp "gtags-find-tag-after-hook")
  (ad-deactivate-regexp "gtags-pop-stack-after-hook")
  (ad-deactivate-regexp "succor-imenu-after-jump-hook"))

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
        (cur-buf (current-buffer))
        (line (buffer-substring (line-beginning-position) (line-end-position)))
        (ret     ad-do-it))
    (if (equal cur-buf ret)
        (message "tag not found")
      (run-hook-with-args 'gtags-find-tag-after-hook name))))

(defadvice gtags-pop-stack (around gtags-pop-stack-after-hook)
  "Add hook"
  (let ((name (gtags-current-token))
        (line (buffer-substring (line-beginning-position) (line-end-position))))
    ad-do-it
    (run-hook-with-args 'gtags-pop-stack-after-hook name)))

(defadvice imenu (after succor-imenu-after-jump-hook)
  (run-hooks 'succor-imenu-after-jump-hook))
                        

(defun succor-pop-stack (args)
  "gtags-pop-stackで戻った関数のメモにジャンプする．メモに関数がまだ記録されていない場合は見出しを作成する"
  (if (equal which-function-mode nil)
      (which-function-mode t))
  (let* ((tag-name args)
         (source-buffer (buffer-name gtags-current-buffer))
         (line (which-function))
         (dir (if (string-match (concat (gtags-get-rootpath)
                                        "\\(.*\\)"
                                        source-buffer)
                                (buffer-file-name (current-buffer)))
                  (match-string 1 (buffer-file-name (current-buffer)))))
         (path (concat *succor-work-directory*
                       dir
                       (if (string-match "\*.*\* (.*)\\(.*\\)<.*>" source-buffer)
                           (match-string 1 source-bufer)
                         source-buffer)
                       *succor-file-extension*))
         (buf (current-buffer))
         (note-buffer (progn (unless (file-exists-p (concat *succor-work-directory* dir))
                               (make-directory (concat *succor-work-directory* dir) t))
                             (find-file-noselect path)))
         (link (org-store-link nil))
         (win (selected-window)))
    (select-window *succor-note-window*)
    (set-window-buffer (selected-window) note-buffer)
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
         (dir (if (string-match (concat (gtags-get-rootpath)
                                        "\\(.*\\)"
                                        source-buffer)
                                (buffer-file-name (current-buffer)))
                  (match-string 1 (buffer-file-name (current-buffer)))))
         (path (concat *succor-work-directory*
                       dir
                       (if (string-match "\*.*\* (.*)\\(.*\\)<.*>" source-buffer)
                           (match-string 1 source-bufer)
                         source-buffer)
                       *succor-file-extension*))
         (buf (current-buffer))
         (note-buffer (progn (unless (file-exists-p (concat *succor-work-directory* dir))
                               (make-directory (concat *succor-work-directory* dir) t))
                             (find-file-noselect path)))
         (link (org-store-link nil)))
    (save-selected-window
      (switch-to-buffer-other-window note-buffer)
      (setq *succor-note-window* (selected-window))
      (goto-char (point-min))
      (when (equal (re-search-forward (concat tag-name "$") nil t) nil)
        (goto-char (point-max))
        (save-excursion
          (insert (concat "* " tag-name "\n"))
          (org-entry-put (point) "LINK" link)
          (org-entry-put (point) "TIME" (format-time-string "<%Y-%m-%d %a %H:%M:%S>" (current-time)))))
      (recenter 0))))

(defun succor-imenu-jamp ()
  "imenuでジャンプした関数のメモにジャンプする．メモに関数がまだ記録されていない場合は見出しを作成する"
  (if (equal which-function-mode nil)
      (which-function-mode t))
  (let* ((tag-name (which-function))
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
      (goto-char (point-min))
      (when (equal (re-search-forward (concat tag-name "$") nil t) nil)
        (goto-char (point-max))
        (save-excursion
          (insert (concat "* " tag-name "\n"))
          (org-entry-put (point) "LINK" link)
          (org-entry-put (point) "TIME" (format-time-string "<%Y-%m-%d %a %H:%M:%S>" (current-time)))))
      (recenter 0))))

(defun succor-lookup ()
  "現在の関数のノートを参照する"
  (interactive)
  (if (equal which-function-mode nil)
      (which-function-mode t))
  (let* ((tag-name (which-function))
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
    (succor-lookup-tag note-buffer tag-name link)))

(defun succor-lookup-tag (buffer tag link)
  (save-selected-window
    (switch-to-buffer-other-window buffer)
    (setq *succor-note-window* (selected-window))
    (goto-char (point-min))
    (when (equal (re-search-forward (concat tag "$") nil t) nil)
      (goto-char (point-max))
      (save-excursion
        (insert (concat "* " tag "\n"))
        (org-entry-put (point) "LINK" link)
        (org-entry-put (point) "TIME" (format-time-string "<%Y-%m-%d %a %H:%M:%S>" (current-time)))))
    (recenter 0)))

(add-hook 'gtags-find-tag-after-hook 'succor-find-tag)
(add-hook 'gtags-pop-stack-after-hook 'succor-pop-stack)
(add-hook 'succor-imenu-after-jump-hook 'succor-imenu-jamp)

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