;;; orglatex.el -*- lexical-binding: t; -*-
;; Use latexmk instead of always pdflatex
;; This should make things more uniform everywhere
;; Use minted
(map! :map cdlatex-mode-map :i "TAB" #'cdlatex-tab)
                                        ;(add-to-list 'org-latex-packages-alist '("outputdir=build" "minted"))
(add-to-list 'org-latex-packages-alist '("" "cleveref"))
(use-package! engrave-faces
  :after ox-latex
  :config
  (add-to-list 'org-latex-engraved-options '("linenos" "true"))
  (setq org-latex-engraved-theme "t")
  )
;;(setq org-latex-src-block-backend 'engraved)
(add-to-list 'exec-path "/home/lukas/.local/texlive/2023/bin/x86_64-linux")
(defun org-export-output-file-name-modified (orig-fun extension &optional subtreep pub-dir)
  (unless pub-dir
    (setq pub-dir "build")
    (unless (file-directory-p pub-dir)
      (make-directory pub-dir)))
  (apply orig-fun extension subtreep pub-dir nil))
(advice-add 'org-export-output-file-name :around #'org-export-output-file-name-modified)


(setq org-latex-compiler "lualatex")
(setq org-latex-pdf-process (list "latexmk -f %f -output-directory=%o"))

(add-to-list 'org-latex-classes
             '("scrbook"
               "\\documentclass{scrbook}"
               ("\\section{%s}" . "\\section*{%s}")
               ("\\subsection{%s}" . "\\subsection*{%s}")
               ("\\subsubsection{%s}" . "\\subsubsection*{%s}")
               ("\\paragraph{%s}" . "\\paragraph*{%s}")
               ("\\subparagraph{%s}" . "\\subparagraph*{%s}"))
             )
(add-to-list 'org-latex-classes
             '("diss"
               "\\documentclass[a4paper, 11pt, twoside, onecolumn, openright]{memoir}
[NO-DEFAULT-PACKAGES]
\\usepackage[giveninits=true,urldate=iso,firstinits=true,maxbibnames=5]{biblatex}
\\addbibresource{/home/lukas/org/references/library.bib}
\\input{~/Nextcloud/uni_shared/PHD/header.tex}"
               ("\\chapter{%s}" . "\\chapter*{%s}")
               ("\\section{%s}" . "\\section*{%s}")
               ("\\subsection*{%s}" . "\\subsection*{%s}")
               )
             )

                                        ; https://emacs.stackexchange.com/questions/51328/how-can-i-position-label-outside-caption-in-latex-export-of-figures
(defun org-latex--caption/label-string (element info)
  "Return caption and label LaTeX string for ELEMENT.

INFO is a plist holding contextual information.  If there's no
caption nor label, return the empty string.

For non-floats, see `org-latex--wrap-label'."
  (let* ((label (org-latex--label element info nil t))
         (main (org-export-get-caption element))
         (attr (org-export-read-attribute :attr_latex element))
         (type (org-element-type element))
         (nonfloat (or (and (plist-member attr :float)
                            (not (plist-get attr :float))
                            main)
                       (and (eq type 'src-block)
                            (not (plist-get attr :float))
                            (null (plist-get info :latex-listings)))))
         (short (org-export-get-caption element t))
         (caption-from-attr-latex (plist-get attr :caption)))
    (cond
     ((org-string-nw-p caption-from-attr-latex)
      (concat caption-from-attr-latex "\n"))
     ((and (not main) (equal label "")) "")
     ((not main) label)
     ;; Option caption format with short name.
     (t
      (format (if nonfloat "\\captionof{%s}%s{%s}\n%s"
                "\\caption%s%s{%s}\n%s")
              (let ((type* (if (eq type 'latex-environment)
                               (org-latex--environment-type element)
                             type)))
                (if nonfloat
                    (cl-case type*
                      (paragraph "figure")
                      (image "figure")
                      (special-block "figure")
                      (src-block (if (plist-get info :latex-listings)
                                     "listing"
                                   "figure"))
                      (t (symbol-name type*)))
                  ""))
              (if short (format "[%s]" (org-export-data short info)) "")
              (org-export-data main info)
              label)))))
