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
               "\\documentclass[a4paper, 10pt, twoside, onecolumn, openright]{memoir}
[NO-DEFAULT-PACKAGES]
[NO-PACKAGES]
\\input{~/Nextcloud/uni_shared/PHD/header.tex}"
               ("\\part{%s}" . "\\part*{%s}")
               ("\\chapter{%s}" . "\\chapter*{%s}")
               ("\\section{%s}" . "\\section*{%s}")
               ("\\subsection{%s}" . "\\subsection*{%s}")
               )
             )
