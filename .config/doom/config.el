;; -*- no-byte-compile: t; -*-
;;; I am stealing a lot from https://github.com/sunnyhasija/Academic-Doom-Emacs-Config
;;; to get a nice org-mode workflow
;;;
;;; $DOOMDIR/config.el -*- lexical-binding: t; -*-

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
;;(use-package! org-glossary
;;;;  :hook (org-mode . org-glossary-mode))

;;
;; Some styling
(use-package! org-modern-indent
  :config
  (add-hook 'org-mode-hook #'org-modern-indent-mode 90))
(with-eval-after-load 'org (global-org-modern-mode))


(use-package! oc
  :after org bibtex-completion bibtex-actions
  :config
  (setq org-cite-global-bibliography "~/org/references/library.bib"))


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

(setq org-babel-python-command "/home/lukas/.local/anaconda3/bin/python")



(require 'org-download)
;; Drag-and-drop to `dired`
(add-hook 'dired-mode-hook 'org-download-enable)

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

(load-file (expand-file-name "org-roam.el" doom-private-dir ))
(load-file (expand-file-name "org-ref.el" doom-private-dir ))
(load-file (expand-file-name "org-latex.el" doom-private-dir ))
(load-file (expand-file-name "elfeed.el" doom-private-dir ))
(load-file (expand-file-name "emms.el" doom-private-dir ))
