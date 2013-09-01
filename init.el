; Variables ----------------------------------------------------------

;; Basic settings
(setq mac-command-modifier 'meta
      ring-bell-function 'ignore
      vc-handled-backends nil
      backup-inhibited t
      inhibit-startup-screen t
      mac-allow-anti-aliasing t
      python-indent-offset 4
      python-indent-guess-indent-offset nil
      confirm-kill-emacs 'yes-or-no-p)

(setq-default truncate-lines t
	      show-trailing-whitespace t)

;; Custom frame title
(setq frame-title-format (list
			  '(:eval
			    (if buffer-file-name
				(replace-regexp-in-string
				 (getenv "HOME") "~" buffer-file-name)
			      (concat "Buffer: " (buffer-name))))
			  (format " - GNU Emacs %d.%d" emacs-major-version emacs-minor-version)))

; Commands -----------------------------------------------------------

;; Use UTF-8
(prefer-coding-system 'utf-8-unix)

;; Display the time in the modeline
(display-time)

;; Delete region if selected when typing
(delete-selection-mode)

;; Let clients use this as a server
(server-start)

;; Slim the frame down
(menu-bar-mode -1)
(tool-bar-mode -1)
(scroll-bar-mode -1)
(blink-cursor-mode -1)

;; Show columns and line numbers
(column-number-mode 1)
(line-number-mode 1)

;; Highlight regions and parens
(transient-mark-mode 1)
(show-paren-mode 1)

; Frame --------------------------------------------------------------

;; Set font
(cond ((string= system-name "Erics-MacBook-Pro.local")
       (set-frame-font "Source Code Pro 12" :frames t))
      (t (set-frame-font "Source Code Pro 16" :frames t)))

;; Resize the frame once everything is loaded
(add-hook 'emacs-startup-hook
	  '(lambda ()
	     (set-frame-size (selected-frame) 80 35)))

;; https://github.com/sellout/emacs-color-theme-solarized
(add-to-list 'custom-theme-load-path "~/.emacs.d/solarized/")
(load-theme 'solarized-dark t)

; Functions ----------------------------------------------------------

(defun delete-file-and-buffer ()
  (interactive)
  (delete-file buffer-file-name)
  (kill-buffer (buffer-name)))

; Hooks --------------------------------------------------------------

(add-hook 'after-save-hook
	  'executable-make-buffer-file-executable-if-script-p)

; Packages -----------------------------------------------------------

(add-to-list 'load-path "~/.emacs.d/lib/")

;; Installed from Homebrew
(require 'magit)

(require 'ido)
(ido-mode 1)

(require 'yaml-mode)
(add-to-list 'auto-mode-alist '("\\.yml$" . yaml-mode))

; Keybindings --------------------------------------------------------

(global-set-key (kbd "\C-x SPC") 'just-one-space)
(global-set-key (kbd "M-+") 'text-scale-increase)
(global-set-key (kbd "M--") 'text-scale-decrease)
(global-set-key (kbd "RET") 'newline-and-indent)
(global-set-key (kbd "\C-c \C-x r") 'replace-string)
(global-set-key (kbd "\C-x m") 'magit-status)

; Custom -------------------------------------------------------------

(setq custom-file "~/.emacs.d/custom.el")
(load custom-file)
