(add-to-list 'load-path "~/src/org-opml")
(add-to-list 'auto-mode-alist '("\\.opml$" . org-mode))
(add-to-list 'format-alist '(opml "Outline Processor Markup Language"
				  "<[?]xml version=\"1.0\"[?]>[\n]?.*[\n]?<opml version=\"2.0\">"
				  "~/src/org-opml/opml2org.py" opml-encode t))

(load-library "ox-opml")

(defun set-buffer-file-format-to-opml ()
  "Set buffer-file-format to '(opml) when visiting an .opml file.

The regexp in `format-alist' works when loading an already
existing OPML file that contains the magic marker.

With this, if a blank file is created that ends with .opml it
will still be exported as OPML."
  (when (string-match "\.opml$" (buffer-file-name))
    (setq buffer-file-format '(opml))))

(add-hook 'find-file-hooks 'set-buffer-file-format-to-opml)

(defun opml-encode (begin end buffer)
  (let ((org-export-show-temporary-export-buffer nil)
	(name "*OPML Export Buffer*"))
    (org-export-to-buffer 'opml name)
    (erase-buffer)
    (insert-buffer-substring (get-buffer name))
    (point-max)))
