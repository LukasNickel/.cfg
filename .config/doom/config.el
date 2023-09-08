;; -*- no-byte-compile: t; -*-
;;; I am stealing a lot from https://github.com/sunnyhasija/Academic-Doom-Emacs-Config
;;; to get a nice org-mode workflow
;;;
;;; $DOOMDIR/config.el -*- lexical-binding: t; -*-

;; Place your private configuration here! Remember, you do not need to run 'doom
;; sync' after modifying this file!


;; Some functionality uses this to identify you, e.g. GPG configuration, email
;; clients, file templates and snippets.
(setq user-full-name "Lukas Nickel"
      user-mail-address "lukasnickel@outlook.de")

; (ispell-change-dictionary "en_US" t)
;; Doom exposes five (optional) variables for controlling fonts in Doom. Here
;; are the three important ones:
;;
;; + `doom-font'
;; + `doom-variable-pitch-font'
;; + `doom-big-font' -- used for `doom-big-font-mode'; use this for
;;   presentations or streaming.
;;
;; They all accept either a font-spec, font string ("Input Mono-12"), or xlfd
;; font string. You generally only need these two:
;; (setq doom-font (font-spec :family "monospace" :size 12 :weight 'semi-light)
;;       doom-variable-pitch-font (font-spec :family "sans" :size 13))
(setq doom-font (font-spec :family "BerkeleyMono" :size 15)
      doom-variable-pitch-font (font-spec :family "BerkeleyMono" :size 15)
      doom-big-font (font-spec :family "BerkeleyMono" :size 24))


;; There are two ways to load a theme. Both assume the theme is installed and
;; available. You can either set `doom-theme' or manually load a theme with the
;; `load-theme' function. This is the default:
(setq doom-theme 'doom-gruvbox)

;; If you use `org' and don't want your org files in the default location below,
;; change `org-directory'. It must be set before org loads!
(setq org-directory "~/org"
      org-roam-directory "~/org"
      org-agenda-files (directory-files-recursively "~/org/" "\\.org$")
      org-roam-db-location "~/org/org-roam.db"
;      org-roam-database-connector 'sqlite3
      )


;; This determines the style of line numbers in effect. If set to `nil', line
;; numbers are disabled. For relative line numbers, set this to `relative'.
(setq display-line-numbers-type t)


;; Here are some additional functions/macros that could help you configure Doom:
;;
;; - `load!' for loading external *.el files relative to this one
;; - `use-package!' for configuring packages
;; - `after!' for running code after a package has loaded
;; - `add-load-path!' for adding directories to the `load-path', relative to
;;   this file. Emacs searches the `load-path' when you load packages with
;;   `require' or `use-package'.
;; - `map!' for binding new keys
;;
;; To get information about any of these functions/macros, move the cursor over
;; the highlighted symbol at press 'K' (non-evil users must press 'C-c c k').
;; This will open documentation for it, including demos of how they are used.
;;
;; You can also try 'gd' (or 'C-c c d') to jump to their definition and see how
;; they are implemented.

;;Loads of org-mode related
;;Easier use of latex
(use-package! org-glossary
  :hook (org-mode . org-glossary-mode))

;;
;; Some styling
(use-package! org-modern-indent
  :config
  (add-hook 'org-mode-hook #'org-modern-indent-mode 90))
(with-eval-after-load 'org (global-org-modern-mode))

(use-package! org-roam
  :after org
  :config
  (setq org-roam-v2-ack t)
  (setq org-roam-mode-sections
        (list #'org-roam-backlinks-insert-section
              #'org-roam-reflinks-insert-section
              #'org-roam-unlinked-references-insert-section))
  (org-roam-setup))

(use-package! oc
  :after org bibtex-completion bibtex-actions
  :config
  (setq org-cite-global-bibliography "~/org/references/library.bib"))

;; TODO: The templates could go in extra files
(after! org-roam
    (setq org-roam-capture-templates
          `(("s" "standard" plain "%?"
     :if-new
     (file+head "%<%Y%m%d%H%M%S>-${slug}.org"
      "#+title: ${title}\n#+filetags: \n\n ")
     :unnarrowed t)
        ("d" "definition" plain
         "%?"
         :if-new
         (file+head "${slug}.org" "#+title: ${title}\n#+filetags: definition \n\n* Definition\n\n\n* Examples\n")
         :unnarrowed t)
        ("r" "ref" plain "%?"
           :if-new
           (file+head "references/notes/${citekey}.org"
           "#+title: ${title}
\n#+filetags: reference ${keywords}
\n* Summary \n
\n* Notes
:PROPERTIES:
:NOTER_DOCUMENT: ${file}
:NOTER_PAGE:
:END:\n")
           :unnarrowed t
           :jump-to-captured t)
        ("p" "presentation" plain "%?"
         :if-new
         (file+head "${slug}.org"
"#+title: ${title}
#+filetags: presentation
#+AUTHOR: Lukas Nickel
#+OPTIONS: H:2 toc:t num:t
#+LATEX_CLASS: beamer
#+LATEX_CLASS_OPTIONS: [presentation, 10pt]
#+BEAMER_THEME: tudo
#+LATEX_HEADER: \\usepackage{amsmath}
#+LATEX_HEADER: \\usepackage{amssymb}
#+LATEX_HEADER: \\usepackage{mathtools}
#+LATEX_HEADER: \\usepackage{csquotes}
#+LATEX_HEADER: \\usepackage{unicode-math}
#+LATEX_HEADER: \\usepackage{siunitx}
#+LATEX_HEADER: \\unimathsetup{math-style=ISO, bold-style=ISO, nabla=upright, partial=upright, mathrm=sym}
")
         :unnarrowed t))))

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

;; Why not
(use-package! org-roam-bibtex
  :after org-roam
  :hook (org-mode . org-roam-bibtex-mode)
  :config
  (require 'org-ref)
  (setq orb-preformat-keywords
   '("citekey" "title" "url" "file" "author-or-editor" "keywords" "pdf" "doi" "author" "tags" "year" "author-bbrev")))


(with-eval-after-load 'org
  (plist-put org-format-latex-options :background 'default)
  (setq org-export-allow-bind-keywords t))

;; Why though?
(define-minor-mode prot/variable-pitch-mode
  "Toggle 'mixed-pitch-modei, except for programming modes"
  :init-value nil
  :global nil
  (if prot/variable-pitch-mode
      (unless (derived-mode-p 'prog-mode)
        (variable-pitch-mode 1))
    (variable-pitch-mode -1)))

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
(setq org-latex-listings 'engraved)
(add-to-list 'exec-path "/home/lukas/.local/texlive/2022/bin/x86_64-linux")
(setq org-publish-project-alist
  '(
    ("website-content"
     :base-directory "~/org/website"
     :base-extension "org"
     :publishing-directory "~/Nextcloud/website"
     :recursive t
     :publishing-function org-html-publish-to-html
     :headline-levels 3
     :auto-preamble t)
    ("website-static"
     :base-directory "~/org/website"
     :base-extension "css\\|js\\|png\\|jpg\\|gif\\|pdf\\|mp3\\"
     :publishing-directory "~/public_html/"
     :recursive t
     :publishing-function org-publish-attachment)
    ("website" :components ("website-content" "website-static"))
    ))

(defun org-export-output-file-name-modified (orig-fun extension &optional subtreep pub-dir)
  (unless pub-dir
    (setq pub-dir "build")
    (unless (file-directory-p pub-dir)
      (make-directory pub-dir)))
  (apply orig-fun extension subtreep pub-dir nil))
(advice-add 'org-export-output-file-name :around #'org-export-output-file-name-modified)

(setq org-latex-compiler "lualatex")
(setq org-latex-pdf-process (list "latexmk -f %f -output-directory=%o"))
;("latexmk -f -pdf -%latex -interaction=nonstopmode -output-directory=%o %f")
(setq org-babel-python-command "/home/lukas/.local/anaconda3/bin/python")

; org roam bindings
(map! :leader
      (:prefix-map ("r" . "regular")
       :desc "find file"            "f"   #'org-roam-node-find
       :desc "find ref"             "F"   #'org-roam-ref-find
       :desc "center scroll"        "s"   #'prot/scroll-center-cursor-mode
       :desc "start taking notes"   "S"   #'org-noter
       :desc "toggle buffer"        "b"   #'org-roam-buffer-toggle
       :desc "insert note"          "i"   #'org-roam-node-insert
       :desc "quit notes"           "q"   #'org-noter-kill-session
       :desc "tag (roam)"           "t"   #'org-roam-tag-add
       :desc "tag (org)"            "T"   #'org-set-tags-command
       :desc "rebuid db"            "d"   #'org-roam-db-build-cache
       :desc "cite"                 "c"   #'org-ref-insert-cite-link
     )
)

(require 'org-download)
;; Drag-and-drop to `dired`
(add-hook 'dired-mode-hook 'org-download-enable)

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
             "\\input{~/Nextcloud/diss/header.tex}
             [NO-DEFAULT-PACKAGES]
             [NO-PACKAGES]"
             ("\\section{%s}" . "\\section*{%s}")
             ("\\subsection{%s}" . "\\subsection*{%s}")
             )
          )

; https://takeonrules.com/2023/04/09/dig-my-grave-leveraging-the-triple-back-tick-in-org-mode/
(defvar dig-my-grave/templates-alist/org-mode
  '(("Bash" . "#+begin_src bash :results scalar replace :exports both :tangle yes\n#+end_src")
    ("Emacs Lisp" . "#+begin_src emacs-lisp\n#+end_src")
    ("Org Structure" . org-insert-structure-template)
    ("Python" . "#+begin_src python :tangle yes exports output \n#+end_src")
    ("Update" . tempel-insert-update_block))
  "A list of `cons' cells with `car' as the label and `cdr' as
 the value that we'll insert.  Used as the collection for the
 `dig-my-grave' `completing-read'.")

(define-key org-mode-map (kbd "`") #'dig-my-grave)

(defun dig-my-grave ()
  "Three consecutive graves (e.g. “`”) at the start of the line prompts for
 inserting content.  See `dig-my-grave/templates-alist/org-mode'."
  (interactive)
  (if (or (and (> (point) 3)
            (string= (buffer-substring-no-properties
                       (- (point) 3) (point)) "\n``"))
        ;; Account for starting on the first line
        (and (= (point) 3)
          (string= (buffer-substring-no-properties
                     (- (point) 2) (point)) "``")))
    ;; We have just hit our third backtick at the beginning of the line.
    (progn
      (delete-char -2)
      ;; I use the alist-get pattern a lot...perhaps a function?
      (let ((value (alist-get (completing-read "Special Content: "
                                  dig-my-grave/templates-alist/org-mode nil t)
                     dig-my-grave/templates-alist/org-mode nil nil #'string=)))
        (cond
          ;; Let's assume that we're dealing with registered org blocks.
          ((stringp value)
            (insert value) (forward-line -1) (org-edit-special))
          ;; Trust the function
          ((commandp value) (call-interactively value))
          ((functionp value) (funcall value))
          ((ad-lambda-p) (funcall value))
          ;; Time for a pull request
          (t (error "Unprocessable value %s for #'dig-my-grave" value)))))
    (setq last-command-event ?`)
    (call-interactively #'org-self-insert-command)))






(setq fancy-splash-image "~/.config/doom/blackhole.png")
;; true transparency
(set-frame-parameter nil 'alpha-background 100)
(add-to-list 'default-frame-alist '(alpha-background . 100))

;; only after emacs 29
(defun toggle-transparency ()
  "Toggle transparency."
  (interactive)
  (let ((alpha-background (frame-parameter nil 'alpha-background)))
    (set-frame-parameter
     nil 'alpha-background
     (if (eql (cond ((numberp alpha-background) alpha-background)
                    ((numberp (cdr alpha-background)) (cdr alpha-background))
                    )
              100)
         '96 '100))))

(global-set-key (kbd "C-c t r") 'toggle-transparency)

(setq-default elfeed-search-filter "@1-week-ago +unread ")
(require 'elfeed-org)
(use-package! elfeed-org
    :after elfeed
    :init
    (setq rmh-elfeed-org-files (list "~/Nextcloud/org/elfeed.org"))
    )
(elfeed-org)

(use-package elfeed-tube
  :ensure t ;; or :straight t
  :after elfeed
  :demand t
  :config
  ;; (setq elfeed-tube-auto-save-p nil) ; default value
  ;; (setq elfeed-tube-auto-fetch-p t)  ; default value
  (elfeed-tube-setup)

  :bind (:map elfeed-show-mode-map
         ("F" . elfeed-tube-fetch)
         ([remap save-buffer] . elfeed-tube-save)
         :map elfeed-search-mode-map
         ("F" . elfeed-tube-fetch)
         ([remap save-buffer] . elfeed-tube-save)))

(use-package elfeed-tube-mpv
  :ensure t ;; or :straight t
  :bind (:map elfeed-show-mode-map
              ("C-c C-f" . elfeed-tube-mpv-follow-mode)
              ("C-c C-w" . elfeed-tube-mpv-where)))

; refresh feeds every 2 hours
(run-at-time nil (* 2 60 60) #'elfeed-update)
(setq elfeed-db-directory "~/Nextcloud/stuff_to_sync")

; https://sqrtminusone.xyz/posts/2021-09-07-emms/
(use-package emms-setup)
(use-package emms
  :after emms-setup
  :config
  (emms-all)
  (setq emms-source-file-default-directory "~/Nextcloud/music/")
  (setq emms-player-mpd-music-directory "~/Nextcloud/music")
  (setq emms-player-mpd-server-name "localhost")
  (setq emms-player-mpd-server-port "6600")
  (add-to-list 'emms-info-functions 'emms-info-mpd)
  (add-to-list 'emms-player-list 'emms-player-mpd)
  (emms-player-mpd-connect)
  (add-hook 'emms-playlist-cleared-hook 'emms-player-mpd-clear)

)
