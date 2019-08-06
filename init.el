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

(setq custom-file (concat user-emacs-directory "/custom.el"))
(load-file custom-file)

(unless package-archive-contents
  (package-refresh-contents))
(package-install-selected-packages)

(unless (display-graphic-p)
  (require 'terminal-focus-reporting)
  (terminal-focus-reporting-mode))

(defun server-shutdown ()
  "Save buffers, Quit, and Shutdown (kill) server"
  (interactive)
  (save-some-buffers)
  (kill-emacs))

(defun save-current-buffer-if-needed ()
  (interactive)
  (when (and (buffer-file-name) (buffer-modified-p))
    (save-buffer)))

(defun fix-C-S-backspace ()
  (interactive)
  (define-key input-decode-map "\e[27;6;15~" [C-S-backspace])); for urxvt only

(add-hook 'tty-setup-hook 'fix-C-S-backspace)

(add-hook 'focus-out-hook 'save-current-buffer-if-needed)
(add-hook 'focus-in-hook 'diff-hl-update)
(add-hook 'prog-mode-hook 'fci-mode); set long line ruler
(add-hook 'git-commit-mode-hook 'fci-mode)
(add-hook 'prog-mode-hook (lambda () (setq indent-tabs-mode nil)))

(setq org-agenda-file-regexp "\\`[^.].*\\.org\\'")
(setq org-agenda-files (apply 'append
                              (mapcar
                              (lambda (directory)
                                (directory-files-recursively
                                 directory org-agenda-file-regexp))
                              '("~/doc/notes/"))))
(setq org-pomodoro-length 30)
(setq org-pomodoro-long-break-length 30)
(setq org-pomodoro-manual-break t)
(setq org-pomodoro-expiry-time 60)
(setq org-pomodoro-format "Pomodoro - %s")
(setq org-pomodoro-short-break-format "Short - %s")
(setq org-pomodoro-long-break-format "Long - %s")
(setq org-pomodoro-overtime-format "Overtime - %s")

(global-diff-hl-mode)
(global-display-line-numbers-mode)
(global-hl-line-mode)
(global-undo-tree-mode)
(global-total-lines-mode)
(global-anzu-mode); show total matches in find/replace
(global-git-commit-mode)
(global-whitespace-mode)
(global-whitespace-cleanup-mode)
(dtrt-indent-global-mode); figure out file indentation
(smart-tabs-insinuate 'c 'c++)
(menu-bar-mode -1)
(tool-bar-mode -1)
(scroll-bar-mode -1)
(horizontal-scroll-bar-mode -1)
(diff-hl-margin-mode); use margin for diff signs
(diff-hl-flydiff-mode); on-the-fly highlighting (without having to save file)
(column-number-mode)
(electric-pair-mode)

(add-to-list 'default-frame-alist
             '(font . "Terminus-10"))

(setq-default global-mode-string '(:eval (format "  %dL" total-lines)))
(setq-default tab-width 4)
(setq-default indent-tabs-mode nil)
(setq-default c-basic-offset 4)
(setq-default auto-save-default nil)
(setq-default make-backup-files nil)
(setq-default require-final-newline t)
(setq-default projectile-switch-project-action 'helm-projectile)
(setq-default show-trailing-whitespace t)
(setq-default scroll-conservatively 101)
(setq-default echo-keystrokes 0.01)
(setq-default git-commit-summary-max-length 50)
(setq-default column-number-indicator-zero-based nil)
(setq-default fci-rule-column 80)
(setq-default create-lockfiles nil)
(setq-default org-log-done 'note)
(setq-default org-startup-indented t)
(setq-default find-file-visit-truename t)
(setq whitespace-style '(face trailing))
(setq-default org-display-custom-times t)
(setq-default org-time-stamp-custom-formats '("<%a %d.%m.%Y>" . "<%a %d.%m.%Y %H:%M>"))

(setq org-log-note-headings '((done . "CLOSING NOTE")
                              (state . "State %-12s from %-12S %t")
                              (note . "Note taken on %t")
                              (reschedule . "Rescheduled from %S on %t")
                              (delschedule . "Not scheduled, was %S on %t")
                              (redeadline . "New deadline from %S on %t")
                              (deldeadline . "Removed deadline, was %S on %t")
                              (refile . "Refiled on %t")
                              (clock-out . "")))

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

(require 'cl-lib)
(add-hook
 'tty-setup-hook
 '(lambda ()
    (cl-case (assoc-default 'terminal-initted (terminal-parameters))
      (terminal-init-rxvt
       (when (getenv "COLORTERM" (selected-frame))
         (urxvt-keybinder-setup))))))
