
(require 'gtags)

(defvar succor-mode nil)
(defvar succor-mode-map nil)

(defvar *succor-directory* (expand-file-name "~/.succor/")
  "ノートを保存するディレクトリ")

(defvar *succor-current-project* nil
  "現在読んでいるプロジェクト")

(defvar *succor-work-directory* nil
  "")

(defvar *succor-file-extension*
  ".org"
  "ノートファイルの拡張子")

(defvar succor-mark-ring nil)
(defvar *succor-note-window* nil)
(defvar succor-gtags-enable nil)
(defvar succor-imenu-enable nil)

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
        (succor-exit)
      (succor-initialize))
    (setq succor-mode (not succor-mode))))
  (if succor-mode
      nil))

(defun succor-get-current-project ()
  (let* ((rootpath (gtags-get-rootpath))
         (*succor-current-project* (first (reverse (split-string rootpath "/" t)))))
	*succor-current-project*))
  
(defun succor-initialize ()
  (let* ((rootpath (gtags-get-rootpath))
         (*succor-current-project* (succor-get-current-project)))
    (setq *succor-work-directory*
          (concat *succor-directory* *succor-current-project* "/"))
    (catch 'succor-start-p
      (unless (file-exists-p *succor-work-directory*)
        (if (y-or-n-p (format "No such directory %s.\nCreate new directory?" *succor-work-directory*))
            (make-directory *succor-work-directory* t)
          (progn (succor-mode -1)
                 (throw 'succor-start-p t))))
      (succor-activate-advice)
      (message "succor-mode enabled"))))

(defun succor-exit ()
  (succor-deactivate-advice)
  (message "succor-mode disabled"))

(defun succor-activate-advice ()
  (when succor-gtags-enable
    (ad-activate-regexp "gtags-select-it-after-hook")
    (ad-activate-regexp "gtags-pop-stack-after-hook"))
  (when succor-imenu-enable
    (ad-activate-regexp "succor-after-set-mark-comamnd")
    (ad-activate-regexp "succor-imenu-after-jump-hook")))

(defun succor-deactivate-advice ()
  (ad-deactivate-regexp "gtags-select-it-after-hook")
  (ad-deactivate-regexp "gtags-pop-stack-after-hook")
  (ad-deactivate-regexp "succor-after-set-mark-comamnd")
  (ad-deactivate-regexp "succor-imenu-after-jump-hook"))

(defun succor-define-mode-map ()
  "キーマップ `succor-define-mode-map' を定義する。"
  (unless (keymapp succor-mode-map)
    (setq succor-mode-map (make-sparse-keymap))
    (setq minor-mode-map-alist
          (cons (cons 'succor-mode succor-mode-map)
                minor-mode-map-alist))))

(defun succor-mark ()
  "カーソル位置をsuccor-mark-ringに追加する"
  (let ((marker (cons (current-buffer) (point))))
    (setq succor-mark-ring (cons marker succor-mark-ring))))

(defun succor-pop-stack ()
  "ジャンプ前の位置に戻る"
  (interactive)
  (let ((buf (caar succor-mark-ring))
        (pos (cdar succor-mark-ring)))
    (setq succor-mark-ring (cdr succor-mark-ring))
    (switch-to-buffer buf)
    (goto-char pos)
    (which-function-mode t)
    (let* ((tag-name (or (which-function) ""))
           (note-buffer (find-file-noselect (succor-get-note-path)))
           (link (org-store-link nil))
           (win (selected-window)))
      (select-window *succor-note-window*)
      (set-window-buffer (selected-window) note-buffer)
      (goto-char (point-min))
      (when (equal (re-search-forward tag-name nil t) nil)
        (goto-char (point-max))
        (succor-insert-note tag-name link))
      (recenter 0)
      (select-window win))))

;;; Advice
(defadvice gtags-select-it (around gtags-select-it-after-hook)
  "Add hook."
  (succor-mark)
  (let ((name (gtags-current-token))
        (cur-buf (current-buffer))
        (line (buffer-substring (line-beginning-position) (line-end-position)))
        (ret     ad-do-it))
    (if (equal cur-buf ret)
        (message "tag not found")
      (run-hook-with-args 'gtags-select-it-after-hook name))))

(defadvice gtags-pop-stack (around gtags-pop-stack-after-hook)
  "Add hook"
  (let ((name (gtags-current-token))
        (line (buffer-substring (line-beginning-position) (line-end-position))))
    ad-do-it
    (run-hook-with-args 'gtags-pop-stack-after-hook name)))

(defadvice set-mark-command (after succor-after-set-mark-comamnd)
  (if arg
      (let ((name (gtags-current-token))
            (line (buffer-substring (line-beginning-position)
                                    (line-end-position))))
        (succor-pop-note name))))

(defadvice imenu (around succor-imenu-after-jump-hook)
  (succor-mark)
  ad-do-it
  (run-hooks 'succor-imenu-after-jump-hook))
                        
;;; Jump note with gtags, imenu,
(defun succor-pop-note (args)
  "gtags-pop-stackで戻った関数のメモにジャンプする．メモに関数がまだ記録されていない場合は見出しを作成する"
  (if (equal which-function-mode nil)
      (which-function-mode t))
  (let* ((tag-name (or (which-function) ""))
         (line (which-function))
         (note-buffer (find-file-noselect (succor-get-note-path)))
         (link (org-store-link nil))
         (win (selected-window)))
    (select-window *succor-note-window*)
    (set-window-buffer (selected-window) note-buffer)
    (goto-char (point-min))
    (when (equal (re-search-forward line nil t) nil)
      (goto-char (point-max))
      (succor-insert-note tag-name link))
    (recenter 0)
    (select-window win)))


(defun succor-find-note (args)
  "gtags-find-tagで検索した関数のメモにジャンプする．メモに関数がまだ記録されていない場合は見出しを作成する"
  (if (equal which-function-mode nil)
      (which-function-mode t))
  (let* ((tag-name args)
         (line (buffer-substring (line-beginning-position) (line-end-position)))
         (note-buffer (find-file-noselect (succor-get-note-path)))
         (link (org-store-link nil)))
    (save-selected-window
      (switch-to-buffer-other-window note-buffer)
      (setq *succor-note-window* (selected-window))
      (goto-char (point-min))
      (when (equal (re-search-forward (concat tag-name "$") nil t) nil)
        (goto-char (point-max))
        (succor-insert-note tag-name link))
      (recenter 0))))

(defun succor-imenu-jamp ()
  "imenuでジャンプした関数のメモにジャンプする．メモに関数がまだ記録されていない場合は見出しを作成する"
  (if (equal which-function-mode nil)
      (which-function-mode t))
  (let* ((tag-name (which-function))
         (note-buffer (find-file-noselect (succor-get-note-path)))
         (link (org-store-link nil)))
    (save-selected-window
      (switch-to-buffer-other-window note-buffer)
      (setq *succor-note-window* (selected-window))
      (goto-char (point-min))
      (when (equal (re-search-forward (concat tag-name "$") nil t) nil)
        (goto-char (point-max))
        (succor-insert-note tag-name link))
      (recenter 0))))

(defun succor-lookup ()
  "現在の関数のノートを参照する"
  (interactive)
  (succor-mark)
  (which-function-mode t)
  (let* ((tag-name (which-function))
         (note-buffer (find-file-noselect (succor-get-note-path)))
         (link (org-store-link nil)))
    (succor-lookup-tag note-buffer tag-name link)))

(defun succor-lookup-tag (buffer tag link)
  (save-selected-window
    (switch-to-buffer-other-window buffer)
    (setq *succor-note-window* (selected-window))
    (goto-char (point-min))
    (when (equal (re-search-forward (concat tag "$") nil t) nil)
      (goto-char (point-max))
      (succor-insert-note tag link))
    (recenter 0)))

(defun succor-get-note-path ()
  (let* ((source-buffer (car (reverse (split-string (buffer-file-name (current-buffer)) "/"))))
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
                       *succor-file-extension*)))
    (unless (file-exists-p (file-name-directory path))
        (make-directory (file-name-directory path) t))
    path))

(defun succor-insert-note (tag-name link)
  (save-excursion
    (insert (concat "* " tag-name "\n"))
    (org-entry-put (point) "LINK" link)
    (org-entry-put (point) "TIME" (format-time-string "<%Y-%m-%d %a %H:%M:%S>" (current-time)))))

(add-hook 'gtags-select-it-after-hook 'succor-find-note)
(add-hook 'gtags-pop-stack-after-hook 'succor-pop-note)
(add-hook 'succor-imenu-after-jump-hook 'succor-imenu-jamp)


;;; Capture note
;; Ported by http://d.hatena.ne.jp/rubikitch/20090121/1232468026
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
         (path (succor-get-note-path))
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
(define-key succor-mode-map "\C-c\C-l" 'succor-lookup)
(define-key succor-mode-map "\C-c\C-t" 'succor-pop-stack)
(provide 'succor)
