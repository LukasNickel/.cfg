;;; org-ref.el -*- lexical-binding: t; -*-

(defun org-ref-insert-glossary-link-with-symbols ()
  "Like https://github.com/jkitchin/org-ref/blob/195b8d3209aff956ecdd755422700e8517a34d11/org-ref-glossary.el#L682C1-L766C41 but with fixed symbol"
  (interactive)
  ;; gather entries
  (let* ((glossary-candidates '())
	 key entry type
	 type-annotation
	 completion-extra-properties
	 choice)

    ;; glossary terms
    (save-excursion
      (goto-char (point-min))
      (while (re-search-forward
	      "\\\\newglossaryentry{\\([[:ascii:]]+?\\)}" nil t)

	(setq key (match-string 1)
	      entry (or-parse-glossary-entry key))

	(cl-pushnew (cons
		     (propertize (format "%s: %s - glossary."
					 (plist-get entry :name)
					 (plist-get entry :description)
					 (plist-get entry :symbol))
				 'face 'org-ref-glossary-face)
		     entry)
		    glossary-candidates)))

    ;; get entries from the table
    (let ((entries (save-excursion
		     (catch 'found
		       (org-element-map
			   (org-element-parse-buffer)
			   'table
			 (lambda (el)
			   (when (and (org-element-property :name el)
				      (stringp (org-element-property :name el))
				      (string= "glossary"
					       (org-element-property :name el)))
			     (goto-char (org-element-property :contents-begin el))
			     (throw 'found
				    ;; skip header and hline
				    (nthcdr 2 (org-babel-read-table))))))))))
      (cl-loop for (label name description symbol) in entries do
	       (cl-pushnew (cons
			    (propertize (format "%s: %s - glossary."
						label
						description)
					'face 'org-ref-glossary-face)
			    (list :label label :name name :description description :symbol symbol))
			   glossary-candidates)))


    (setq choice (completing-read "Choose: " glossary-candidates)
	  entry (cdr (assoc choice glossary-candidates))
	  type-annotation (lambda (s)
			    (let ((item (assoc s minibuffer-completion-table)))
			      (when item (concat
					  (make-string (- 12 (length s)) ? )
					  "-- "
					  (cl-second item)))))
	  completion-extra-properties `(:annotation-function ,type-annotation)
	  type (completing-read "Type: "
				org-ref-glossary-gls-commands
				nil t))

    (insert (format
	     "[[%s:%s][%s]]"
	     type
	     (plist-get entry :label)
	     (pcase type
	       ("gls" (plist-get entry :name))
	       ("glspl" (concat (plist-get entry :name) "s"))
	       ("Gls" (concat (capitalize (substring (plist-get entry :name) 0 1))
			      (substring (plist-get entry :name) 1)))
	       ("Glspl" (concat (capitalize (substring (plist-get entry :name) 0 1))
				(substring (plist-get entry :name) 1)
				"s"))
	       ("glssymbol" (plist-get entry :symbol))
	       ("Glssymbol" (concat (capitalize (substring (plist-get entry :symbol) 0 1))
				    (substring (plist-get entry :symbol) 1)))
	       ("glsdesc" (plist-get entry :description))
	       ("Glsdesc" (concat (capitalize (substring (plist-get entry :description) 0 1))
				  (substring (plist-get entry :description) 1)))
	       ;; fall-through for everything else
	       (_ (plist-get entry :name)))))))

(defun org-ref-glossary-before-parsing-with-symbols (_backend)
  "Adapted from https://github.com/jkitchin/org-ref/blob/195b8d3209aff956ecdd755422700e8517a34d11/org-ref-glossary.el#L395C1-L438C39"
  (save-restriction
    (widen)
    (let* (begin
	   end
	   (entries (save-excursion
		      (catch 'found
			(org-element-map
			    (org-element-parse-buffer)
			    'table
			  (lambda (el)
			    (when (and (org-element-property :name el)
				       (stringp (org-element-property :name el))
				       (string= "glossary" (org-element-property :name el)))
			      (setq begin (org-element-property :begin el)
				    end (org-element-property :end el))
			      (goto-char (org-element-property :contents-begin el))
			      (throw 'found
				     (nthcdr 2 (org-babel-read-table))))))))))
      ;; Delete the table
      (when entries
	(cl--set-buffer-substring begin end ""))

      (goto-char (point-min))
      (cl-loop for (label name description symbol) in entries
	       do
	       (insert (format "#+latex_header_extra: \\newglossaryentry{%s}{name=%s,description={{%s}},symbol=%s}\n"
			       label name description symbol))))))

;; org ref is great, but somewhat limited in what you can put into the definitions
;; I slightly adapted some functions for use in my thesis
(advice-add 'org-ref-insert-glossary-link  :override 'org-ref-insert-glossary-link-with-symbols)
(advice-add 'org-ref-glossary-before-parsing  :override 'org-ref-glossary-before-parsing-with-symbols)

;; Hype
(use-package! org-ref
                                        ;:after org-roam
  :config
  (setq
   org-ref-completion-library 'org-ref-ivy-cite
   org-ref-get-pdf-filename-function 'org-ref-get-pdf-filename-helm-bibtex
                                        ; TODO This is not relevant anymore but might be named differently?
   org-ref-note-title-format "* %y - %t\n :PROPERTIES:\n  :Custom_ID: %k\n  :NOTER_DOCUMENT: %F\n :ROAM_KEY: cite:%k\n  :AUTHOR: %9a\n  :JOURNAL: %j\n  :YEAR: %y\n  :VOLUME: %v\n  :PAGES: %p\n  :DOI: %D\n  :URL: %U\n :END:\n\n"
   org-ref-notes-directory "~/org/references/notes/"
   org-ref-notes-function 'orb-edit-notes
   org-ref-default-ref-type "cref"
   org-ref-pdf-directory "~/org/references/"))
(setq ivy-bibtex-default-action 'ivy-bibtex-insert-citation)

(after! org-ref
  (setq
   bibtex-completion-library-path "~/org/references/"
   bibtex-completion-notes-path "~/org/references/notes/"
   bibtex-completion-bibliography '("~/org/references/library.bib")
   bibtex-completion-pdf-field "file"
   bibtex-completion-notes-template-multiple-files
   (concat
    "#+TITLE: ${title}\n"
    "#+ROAM_KEY: cite:${=key=}\n"
    "* TODO Notes\n"
    ":PROPERTIES:\n"
    ":Custom_ID: ${=key=}\n"
    ":NOTER_DOCUMENT: %(orb-process-file-field \"${=key=}\")\n"
    ":AUTHOR: ${author-abbrev}\n"
    ":JOURNAL: ${journaltitle}\n"
    ":DATE: ${date}\n"
    ":YEAR: ${year}\n"
    ":DOI: ${doi}\n"
    ":URL: ${url}\n"
    ":END:\n\n")))

