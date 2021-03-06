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
      user-full-name "Eric Davis"
      user-mail-address "eric@davising.com"
      opml-owner-id "http://ericdavis.org/"
      show-trailing-whitespace t
      delete-by-moving-to-trash t
      trash-directory "~/.emacs-trash"
      confirm-kill-emacs 'yes-or-no-p)

(setenv "PATH" (format "%s:/usr/local/bin" (getenv "PATH")))

(setq-default truncate-lines t)

;; Custom frame title
(setq frame-title-format (list
			  '(:eval
			    (if buffer-file-name
				(replace-regexp-in-string (getenv "HOME") "~" buffer-file-name)
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

; Cask ---------------------------------------------------------------

(require 'cask)
(cask-initialize)

; Frame --------------------------------------------------------------

;; Set font
(cond ((string= system-name "Erics-MacBook-Pro.local")
       (set-frame-font "Source Code Pro 10" :frames t))
      (t
       (set-frame-font "Source Code Pro 11" :frames t)))

;; Resize the frame once everything is loaded
(add-hook 'emacs-startup-hook
	  '(lambda ()
	     (set-frame-size (selected-frame) 80 35)))

(add-hook 'magit-log-edit-mode-hook 'auto-fill-mode)

;; https://github.com/sellout/emacs-color-theme-solarized
(add-to-list 'custom-theme-load-path "~/.emacs.d/solarized/")
(load-theme 'solarized-dark t)

; Functions ----------------------------------------------------------

(defun delete-file-and-buffer ()
  (interactive)
  (delete-file buffer-file-name)
  (kill-buffer (buffer-name)))

(defun timestamp ()
  (interactive)
  (insert
   (format "- %s: " (format-time-string "%I:%M %p"))))

(defun xmllint ()
  "Run xmllint --format on a buffer."
  (interactive)
  (nxml-mode)
  (shell-command-on-region (point-min) (point-max) "xmllint --format -" nil t))

(defun rfc822 (&optional arg)
  "Return the current time formatted according to RFC 822.

When called with the prefix argument, insert at point in buffer.

With no prefix argument, return the string."
  (interactive "P")
  (let ((timestamp (format "%s GMT" (format-time-string "%a, %d %b %Y %H:%M:%S" (current-time) t))))
    (if arg
	(insert timestamp)
      timestamp)))

(defun pwgen ()
  (interactive)
  (let ((pw (shell-command-to-string "/usr/local/bin/pwgen 16 1")))
    (insert (replace-regexp-in-string "\n" "" pw))))

(defun uuidgen (&optional arg)
  (interactive "P")
  (let ((uuid (replace-regexp-in-string "\n" "" (shell-command-to-string "uuidgen"))))
    (if arg
	(insert uuid)
      uuid)))

(defun make-note ()
  (interactive)
  (let ((headline (org-element-at-point)))
    (unless (org-get-property-block)
      (org-insert-property-drawer)
      (org-set-property "type" "note")
      (org-set-property "url" nil)
      (org-set-property "created" (rfc822)))))

; Hooks --------------------------------------------------------------

(add-hook 'after-save-hook
	  'executable-make-buffer-file-executable-if-script-p)

; Packages -----------------------------------------------------------

(add-to-list 'load-path "~/.emacs.d/lib/")
(add-to-list 'load-path "~/.emacs.d/lib/org-8.2.5h/lisp/")

(load-library "org-opml")

;; Org Mode
(setq org-directory "~/Dropbox/Org"
      org-default-notes-file (concat org-directory "/notes.org")
      org-blank-before-new-entry '((heading . auto) (plain-list-item . nil))
      org-startup-indented t)

(define-key global-map "\C-cc" 'org-capture)
(define-key org-mode-map "\M-n" 'outline-next-visible-heading)
(define-key org-mode-map "\M-p" 'outline-previous-visible-heading)

(require 'ox-html)

(setq org-publish-project-alist '(("ericdavis.org"
				   :base-directory "~/Dropbox/Org/ericdavis.org/"
				   :publishing-directory "/tmp/html/ericdavis.org/"
				   :publishing-function (org-html-publish-to-html org-publish-attachment)
				   :recursive t
				   :with-toc nil
				   :html-doctype "html5"
				   :html-html5-fancy t
				   :html-head-include-scripts nil
				   :html-head-include-default-style nil
				   :html-preamble nil
				   :html-postamble nil)))

(defun hide-trailing-whitespace ()
  (setq show-trailing-whitespace nil))

(add-hook 'org-mode-hook 'hide-trailing-whitespace)
(add-hook 'org-mode-hook 'auto-fill-mode)

(setq org-capture-templates
      `(("t" "TODO" item (file ,(concat org-directory "/queue.org"))
	 "- %?\n  (Created: %<%D %r>)" :empty-lines 1)
	("w" "Add worknote" item (file+datetree ,(concat org-directory "/worknotes.org"))
	 "- %<%I:%M %p>: %?" :empty-lines 1)))

(defun rss-node (url title)
  (interactive "sURL: \nsTitle: ")
  (insert title)
  (let ((headline (org-element-at-point)))
    (unless (org-get-property-block)
      (org-insert-property-drawer)
      (org-set-property "created" (rfc822))
      (org-set-property "type" "rss")
      (org-set-property "xmlUrl" url))))

(defun camel-case (string)
  "Transform STRING into camelCase."
  (let ((split (split-string string " ")))
    (replace-regexp-in-string "[^A-Za-z0-9_-]" ""
			      (concat
			       (downcase (car split))
			       (mapconcat #'capitalize (cdr split) "")))))

;; Installed from Homebrew
(require 'magit)

(require 'ido)
(ido-mode 1)

(require 'uniquify)
(setq uniquify-buffer-name-style 'post-forward-angle-brackets)

(require 'yaml-mode)
(add-to-list 'auto-mode-alist '("\\.yml$" . yaml-mode))

(require 'web-mode)
(add-to-list 'auto-mode-alist '("\\.html$" . web-mode))
(add-to-list 'auto-mode-alist '("\\.css$" . web-mode))
(setq web-mode-engines-alist '(("django" . "\.html")))

(require 'markdown-mode)
(add-hook 'markdown-mode-hook 'auto-fill-mode)
(add-to-list 'auto-mode-alist '("\\.md$" . markdown-mode))

; Keybindings --------------------------------------------------------

(global-set-key (kbd "C-x SPC") 'just-one-space)
(global-set-key (kbd "M-+") 'text-scale-increase)
(global-set-key (kbd "M--") 'text-scale-decrease)
(global-set-key (kbd "RET") 'newline-and-indent)
(global-set-key (kbd "C-c C-x r") 'replace-string)
(global-set-key (kbd "C-x m") 'magit-status)
(global-set-key (kbd "C-M-e") 'eval-defun)
(global-set-key (kbd "C-x t") 'timestamp)
(global-set-key (kbd "C-c r") 'recompile)

; Custom -------------------------------------------------------------

(setq custom-file "~/.emacs.d/custom.el")
(load custom-file)
