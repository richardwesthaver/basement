;; summary
(defun org-dblock-write:summary (params)
  "Generate a file or heading summary section.")

(defun org-summary ()
  "Insert or update a summary section.")

(defun org-inbox-configure-dblock ()
  "Configure the current org-inbox-dblock at point."
  (interactive)
  (with-demoted-errors "Error: %S"
    (let* ((beginning (org-beginning-of-dblock))
	   (parameters (org-prepare-dblock)))
      (org-inbox-show-config (current-buffer) beginning parameters))))

;; org-inbox-dashboard?
(defun org-inbox-show-config (&optional buffer position parameters)
  (interactive)
  (switch-to-buffer org-inbox-config-buffer-name)
  (erase-buffer)
  (remove-overlays)
  (widget-insert "\n\n")
  (widget-create 'push-button
		 :notify (lambda(_widget &rest _ignore)
			   (with-current-buffer buffer
			     (goto-char position)
			     )
			   (kill-buffer)
			   (org-ctrl-c-ctrl-c))
		 (propertize "Apply" 'face 'font-lock-comment-face))
  (widget-insert " ")
  (widget-create 'push-button
		 :notify (lambda (_widget &rest _ignore)
			   (kill-buffer))
		 (propertize "Cancel" 'face 'font-lock-string-face))
  (use-local-map widget-keymap)
  (widget-setup))
