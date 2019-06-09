(require 'package)
(let* ((no-ssl (and (memq system-type '(windows-nt ms-dos))
                    (not (gnutls-available-p))))
       (proto (if no-ssl "http" "https")))
  (when no-ssl
    (warn "\
Your version of Emacs does not support SSL connections,
which is unsafe because it allows man-in-the-middle attacks.
There are two things you can do about this warning:
1. Install an Emacs version that does support SSL and be safe.
2. Remove this warning from your init file so you won't see it again."))
  ;; Comment/uncomment these two lines to enable/disable MELPA and MELPA Stable as desired
  (add-to-list 'package-archives (cons "melpa" (concat proto "://melpa.org/packages/")) t)
  ;;(add-to-list 'package-archives (cons "melpa-stable" (concat proto "://stable.melpa.org/packages/")) t)
  (when (< emacs-major-version 24)
    ;; For important compatibility libraries like cl-lib
    (add-to-list 'package-archives (cons "gnu" (concat proto "://elpa.gnu.org/packages/")))))

;; Added by Package.el.  This must come before configurations of
;; installed packages.  Don't delete this line.  If you don't want it,
;; just comment it out by adding a semicolon to the start of the line.
;; You may delete these explanatory comments.
(package-initialize)

(unless package-archive-contents
  (package-refresh-contents))
(package-install-selected-packages)

(unless (display-graphic-p)
  (require 'terminal-focus-reporting)
  (terminal-focus-reporting-mode))

(setq custom-file (concat user-emacs-directory "/custom.el"))
(load-file custom-file)

(defun server-shutdown ()
  "Save buffers, Quit, and Shutdown (kill) server"
  (interactive)
  (save-some-buffers)
  (kill-emacs))

(defun save-current-buffer-if-needed ()
  (interactive)
  (when (and (buffer-file-name) (buffer-modified-p))
    (save-buffer)))

(add-hook 'focus-out-hook 'save-current-buffer-if-needed)
(add-hook 'focus-in-hook 'diff-hl-update)
(add-hook 'prog-mode-hook 'fci-mode); set long line ruler
(add-hook 'git-commit-mode-hook 'fci-mode)

(global-diff-hl-mode)
(global-display-line-numbers-mode)
(global-hl-line-mode)
(global-undo-tree-mode)
(global-total-lines-mode)
(global-anzu-mode); show total matches in find/replace
(global-git-commit-mode)
(dtrt-indent-global-mode); figure out file indentation
(smart-tabs-mode); indent with tabs, align with spaces
(menu-bar-mode -1)
(tool-bar-mode -1)
(scroll-bar-mode -1)
(horizontal-scroll-bar-mode -1)
(diff-hl-margin-mode); use margin for diff signs
(diff-hl-flydiff-mode); on-the-fly highlighting (without having to save file)
(column-number-mode)

(add-to-list 'default-frame-alist
             '(font . "Terminus-10"))

(setq-default global-mode-string '(:eval (format "  %dL" total-lines)))
(setq-default tab-width 4)
(setq-default c-basic-offset 4)
(setq-default auto-save-default nil)
(setq-default make-backup-files nil)
(setq-default require-final-newline t)
(setq-default projectile-switch-project-action 'helm-projectile)
(setq-default show-trailing-whitespace t)
(setq-default scroll-conservatively 101)
(setq-default electric-pair-mode t)
(setq-default echo-keystrokes 0.01)
(setq-default git-commit-summary-max-length 50)
(setq-default column-number-indicator-zero-based nil)
(setq-default fci-rule-column 80)
(setq-default create-lockfiles nil)
(setq-default org-log-done 'note)
(setq-default org-startup-indented t)
(setq-default find-file-visit-truename t)

;(whole-line-or-region-global-mode t)
(doom-modeline-mode)
(helm-mode)
(projectile-mode)
(helm-projectile-on)
(line-number-mode -1)
(setq linum-format "%4d ")
(show-paren-mode)
(xclip-mode)
(global-auto-revert-mode)

(load-file (concat user-emacs-directory "keys.el"))

(defun inhibit-save-message (f &rest args)
  (let ((inhibit-message t))
    (funcall f)))

(advice-add 'save-buffer :around #'inhibit-save-message)
