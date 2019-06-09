;; These are really a nonsense with my keyboard setup.
(global-unset-key (kbd "C-f")); forward-char
(global-unset-key (kbd "C-b")); backward-char
(global-unset-key (kbd "C-n")); next-line
(global-unset-key (kbd "C-p")); previous-line
(global-unset-key (kbd "M-c")); upcase-char

(global-set-key (kbd "C-x k") 'kill-current-buffer)
(global-set-key (kbd "M-x") 'helm-M-x)
(global-set-key (kbd "C-x C-f") 'helm-find-files)
(global-set-key (kbd "C-x b") 'helm-buffers-list)
(global-set-key (kbd "C-x C-b") 'switch-to-buffer)

(define-key projectile-mode-map (kbd "C-c p") 'projectile-command-map)
(global-set-key (kbd "M-w") 'easy-kill)
