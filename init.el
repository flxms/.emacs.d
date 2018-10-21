;;; init.el --- My .emacs

(defun my-package-recompile()
  "Recompile all packages"
  (interactive)
  (byte-recompile-directory "~/.emacs.d/elpa" 0 t))

;;;;;;;;;;;;;;
;; emacsclient
;;;;;;;;;;;;;;
(server-start)

;; add personal static lisp files
(add-to-list 'load-path "~/.emacs.d/elisp")

;;;;;;;;;;;;;;;;;;;;;
;; Package Management
;;;;;;;;;;;;;;;;;;;;;
(require 'package)
  (add-to-list 'package-archives '("elpa" . "http://elpa.gnu.org/packages/") t)
  (add-to-list 'package-archives '("melpa" . "http://melpa.org/packages/") t)
  (add-to-list 'package-archives '("org" . "http://orgmode.org/elpa/") t)
  (add-to-list 'package-archives '("marmalade" . "http://marmalade-repo.org/packages/") t)
(package-initialize)

;;;;;;;;;
;; TabBar
;;;;;;;;;

(add-to-list 'load-path "~/.emacs.d/elisp/tabbar/git")
(require 'tabbar)
(tabbar-mode t)

;;;;;;;;;;;;;;;;;;;;;
;; Backup
;;;;;;;;;;;;;;;;;;;;;

;; no backup files
(setq make-backup-files nil)

;;;;;;;;;;;;;;;;;;;;;;;;;
;; key-bindings for MacOS
;;;;;;;;;;;;;;;;;;;;;;;;;

(setq mac-right-command-modifier 'control)

;;;;;;;;;;;
;; HideShow
;;;;;;;;;;;

(require 'hideshow-org)

;;;;;;;;;;;
;; Python
;;;;;;;;;;;

(elpy-enable)

(setq python-shell-interpreter "/opt/local/bin/jupyter-3.6"
      python-shell-interpreter-args "console --simple-prompt"
      python-shell-prompt-detect-failure-warning nil)
(add-to-list 'python-shell-completion-native-disabled-interpreters
             "jupyter")

;;(setq python-shell-interpreter "ipython"
;;      python-shell-interpreter-args "-i --simple-prompt")

;;;;;;;;;;;
;; Polymode
;;;;;;;;;;;

(unless (package-installed-p 'polymode)
  (package-install 'poly-markdown))

(require 'poly-R)
(require 'poly-markdown)

;;; MARKDOWN
(add-to-list 'auto-mode-alist '("\\.md" . poly-markdown-mode))

;;; R modes
(add-to-list 'auto-mode-alist '("\\.Snw" . poly-noweb+r-mode))
(add-to-list 'auto-mode-alist '("\\.Rnw" . poly-noweb+r-mode))
(add-to-list 'auto-mode-alist '("\\.Rmd" . poly-markdown+r-mode))

(setq markdown-enable-math t)
;; Wrap line in markdown. Comment if you don't dislike words cut in the middle
(add-hook 'markdown-mode-hook (lambda ()
				(visual-line-mode 1)
                                (xah-math-input-mode 1)
				(flyspell-mode 1)
				))

;;;;;;;;;;;
;; Flyspell
;;;;;;;;;;;

(setq ispell-program-name "/opt/local/bin/hunspell")

;;(require 'rw-language-and-country-codes)
;;(require 'rw-ispell)
;;(require 'rw-hunspell)
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(current-language-environment "UTF-8")
 '(custom-enabled-themes nil)
 '(global-linum-mode t)
 '(newsticker-html-renderer (quote w3m-region))
 '(newsticker-url-list
   (quote
    (("Tagesschau" "http://www.tagesschau.de/xml/rss2" nil nil nil)
     ("heise newsticker" "http://www.heise.de/newsticker/heise.rdf" nil nil nil)
     ("Richard Stallman" "http://www.stallman.org/rss/rss.xml" nil nil nil))))
 '(package-selected-packages
   (quote
    (dockerfile-mode docker 0blayout csv-mode pandoc-mode yasnippet-snippets helm-flycheck helm-flymake helm-flyspell typo unicode-whitespace xah-math-input wc-mode polymode zotelo yaml-mode textmate-to-yas tabbar-ruler swiper sokoban r-autoyas outline-magic org2blog org-wc org-ref org-jekyll org-dotemacs org-caldav markdown-mode magit isend-mode gnuplot-mode flyspell-lazy flycheck ess-view ess-smart-underscore ess-R-data-view elpy el-autoyas ebib dropdown-list bibtex-utils awk-it auctex-latexmk apache-mode)))
 '(show-paren-mode t))

;;;;;;;;;;;;
;; Asymptote
;;;;;;;;;;;;
(add-to-list 'load-path "/opt/local/share/asymptote/")
(autoload 'asy-mode "asy-mode.el" "Asymptote major mode." t)
(autoload 'lasy-mode "asy-mode.el" "hybrid Asymptote/Latex major mode." t)
(autoload 'asy-insinuate-latex "asy-mode.el" "Asymptote insinuate LaTeX." t)
(add-to-list 'auto-mode-alist '("\\.asy$" . asy-mode))

;;;;;;;;;;;
;; org-mode
;;;;;;;;;;;

;; set org system directory
;;(defvar org-sys-directory "~/.emacs.d/elisp/org/git/lisp")
;;(defvar org-sys-directory "~/.emacs.d/elisp/org/8.2.6/lisp")
;;(setq load-path (cons org-sys-directory load-path))

;; This file loads Org-mode and then loads the rest of our Emacs initialization from Emacs lisp
;; embedded in literate Org-mode files.

;; Load up Org Mode and (now included) Org Babel for elisp embedded in Org Mode files
(setq dotfiles-dir (file-name-directory (or (buffer-file-name) load-file-name)))

(let* ((org-dir (expand-file-name
                 "lisp" (expand-file-name
                         "org" (expand-file-name
                                "src" dotfiles-dir))))
       (org-contrib-dir (expand-file-name
                         "lisp" (expand-file-name
                                 "contrib" (expand-file-name
                                            ".." org-dir))))
       (load-path (append (list org-dir org-contrib-dir)
                          (or load-path nil))))
  ;; load up Org-mode and Org-babel
  (require 'org-install)
  (require 'ob-tangle))

;; load up all literate org-mode files in this directory
(mapc #'org-babel-load-file (directory-files dotfiles-dir t "\\.org$"))

(global-font-lock-mode 1)
(setq org-use-speed-commands t)

;; add org-mode contributed software (i.e. ditaa executable)
(add-to-list 'load-path "~/.emacs.d/git/org-mode/contrib/lisp")

;;;orgbabel;;;;
(setq org-confirm-babel-evaluate nil)

(org-babel-do-load-languages
 'org-babel-load-languages
 '((asymptote . t)
   (awk . t)
   (C . t)
   (css . t)
   (dot . t)
   (ditaa . t)
   (emacs-lisp . t)
   (gnuplot . t)
   (java . t)
   (js . t)
   (latex . t)
   (lisp . t)
   (maxima . t)
   (octave . t)
   (perl . t)
   (python . t)
   (R . t)
   (shell . t)
   (sql . t)
   (sqlite . t)
  )
 )

(defun flyspell-ignore-tex ()
  (interactive)
  (set (make-variable-buffer-local 'ispell-parser) 'tex))

;; ARROWS
;; subsitute "->" by \rightarrow
(add-hook 'org-latex-after-blockquotes-hook 'org-latex-rightarrows)
(defun org-latex-rightarrows ()
  (goto-char (point-min))
  (while (search-forward "->" nil t)
    (org-if-unprotected
     (replace-match (org-latex-protect-string "$\\rightarrow$") nil t)))
  )

;; subsitute "=>" by \Rightarrow
(add-hook 'org-latex-after-blockquotes-hook 'org-latex-Rightarrows)
(defun org-latex-Rightarrows ()
  (goto-char (point-min))
  (while (search-forward "=>" nil t)
    (org-if-unprotected
     (replace-match (org-latex-protect-string "$\\Rightarrow$") nil t)))
  )

;; subsitute "'->" by \hookrightarrow
(add-hook 'org-latex-after-blockquotes-hook 'org-latex-hookrightarrows)
(defun org-latex-hookrightarrows ()
  (goto-char (point-min))
  (while (search-forward "'->" nil t)
    (org-if-unprotected
     (replace-match (org-latex-protect-string "$\\hookrightarrow$") nil t)))
  )

;; subsitute "<-" by \leftarrow
(add-hook 'org-latex-after-blockquotes-hook 'org-latex-leftarrows)
(defun org-latex-leftarrows ()
  (goto-char (point-min))
  (while (search-forward "<-" nil t)
    (org-if-unprotected
     (replace-match (org-latex-protect-string "$\\leftarrow$") nil t)))
  )

;; subsitute "<-'" by \hookleftarrow
(add-hook 'org-latex-after-blockquotes-hook 'org-latex-hookleftarrows)
(defun org-latex-hookleftarrows ()
  (goto-char (point-min))
  (while (search-forward "<-'" nil t)
    (org-if-unprotected
     (replace-match (org-latex-protect-string "$\\hookleftarrow$") nil t)))
  )

;; subsitute "<->" by \leftrightarrow
(add-hook 'org-latex-after-blockquotes-hook 'org-latex-leftrightarrows)
(defun org-latex-leftrightarrows ()
  (goto-char (point-min))
  (while (search-forward "<->" nil t)
    (org-if-unprotected
     (replace-match (org-latex-protect-string "$\\leftrightarrow$") nil t)))
  )

;; subsitute "<=>" by \Leftrightarrow
(add-hook 'org-latex-after-blockquotes-hook 'org-latex-Leftrightarrows)
(defun org-latex-Leftrightarrows ()
  (goto-char (point-min))
  (while (search-forward "<=>" nil t)
    (org-if-unprotected
     (replace-match (org-latex-protect-string "$\\Leftrightarrow$") nil t)))
  )

;; (setq split-width-threshold nil) ;; avoid side by side splitting of frames
;; (setq split-window-preferred-function nil) ;discourage horizontal splits
;; display-buffer' tries to be smarter when splitting windows.  The new option split-window-preferred-function' lets you specify your own function to pop up new windows.  Its default value split-window-sensibly' can split a window either vertically or horizontally, whichever seems more suitable in the current configuration.  You can tune the behavior of split-window-sensibly by customizing split-height-threshold' and the new option `split-width-threshold'.  Both options now take the value nil to inhibit splitting in one direction.  Setting split-width-threshold to nil inhibits horizontal splitting and gets you the behavior of Emacs 22 in this respect.  In any case, display-buffer may now split the largest window vertically even when it is not as wide as the containing frame.

;; tramp
(setq tramp-default-method "ssh")
(setq tramp-default-user "root")
(setq tramp-default-host "euler.bioinformatix.org")

;; ESS
(require 'ess-smart-underscore)

;; ess rutils
(require 'ess-rutils)

;; yasnippet
(require 'yasnippet) ;; not yasnippet-bundle
;;(add-to-list 'load-path "~/.emacs.d/snippets")
;;(setq yas-snippet-dirs
;;      '("~/.emacs.d/snippets" ;; personal snippets
;;         ))
(yas-global-mode t)

(require 'dropdown-list)
(setq yas-prompt-functions '(yas-dropdown-prompt
                             yas-ido-prompt
                             yas-completing-prompt))

;; Show object summary and function argument completion while editing
;; R code and interacting with an inferior R process
(require 'ess-eldoc)
;; to show function arguments in the minibuffer while you are typing them in an ess-buffer.
(require 'r-autoyas)
(add-hook 'ess-mode-hook 'r-autoyas-ess-activate)

;; Note that the interactive mechanism of this feature is based on IDO which is part of Emacs.  If you are using other completion mechanisms (like Icicles) this feature might not work as desired.  If that is the case, put the following into your .emacs:
;;(setq ess-dbg-use-ido nil)
;; enable skeleton-pair insert globally
(setq skeleton-pair t)
;; (setq skeleton-pair-on-word t)
(global-set-key (kbd "(") 'skeleton-pair-insert-maybe)
(global-set-key (kbd "[") 'skeleton-pair-insert-maybe)
(global-set-key (kbd "{") 'skeleton-pair-insert-maybe)
(global-set-key (kbd "\"") 'skeleton-pair-insert-maybe)
(global-set-key (kbd "\'") 'skeleton-pair-insert-maybe)
(global-set-key (kbd "\`") 'skeleton-pair-insert-maybe)
(global-set-key (kbd "<") 'skeleton-pair-insert-maybe)

;; ESS ShiftEnter
(setq ess-ask-for-ess-directory nil)
(setq ess-local-process-name "R")
(setq ansi-color-for-comint-mode 'filter)
(setq comint-prompt-read-only t)
(setq comint-scroll-to-bottom-on-input t)
(setq comint-scroll-to-bottom-on-output t)
(setq comint-move-point-for-output t)

(defun my-ess-start-R ()
  (interactive)
  (if (not (member "*R*" (mapcar (function buffer-name) (buffer-list))))
      (progn
	;; (setq split-width-threshold nil) ;; avoid side by side splitting of windows
	(R)
	)))

(defun my-ess-eval ()
  (interactive)
  (my-ess-start-R)
  (if (and transient-mark-mode mark-active)
      (call-interactively 'ess-eval-region)
    (call-interactively 'ess-eval-line-and-step)))

(add-hook 'ess-mode-hook
          '(lambda ()
	     (local-set-key [(M-return)] 'my-ess-eval)
             ;; activate & configure ESS outline-mode
             (hs-minor-mode) ;; activate hs-minor-mode in ess by default
             (outline-minor-mode) ;; activate outline-minor-mode in ess by default
             )
	  )

(add-hook 'outline-minor-mode-hook
	'(lambda ()
	(require 'outline-magic)
	(define-key outline-minor-mode-map (kbd "<C-tab>") 'outline-cycle)
	(setq outline-regexp "\\(^#\\{1,10\\} \\)\\|\\(^[a-zA-Z0-9_\.]+ ?<-?function(.*{\\)")
		(defun outline-level ()
			(cond
				((looking-at "^# ") 1)
				((looking-at "^## ") 2)
				((looking-at "^### ") 3)
				((looking-at "^#### ") 4)
				((looking-at "^##### ") 5)
				((looking-at "^###### ") 6)
				((looking-at "^####### ") 7)
				((looking-at "^######## ") 8)
				((looking-at "^######### ") 9)
				((looking-at "^########## ") 10)
				((looking-at "^[a-zA-Z0-9_\.]+ ?<- ?function(.*{") 11)
				(t 1000)))
				))

;; set function keys for ESS
(define-key ess-mode-map [f1] 'ess-R-object-tooltip)
(define-key ess-mode-map [f2] 'ess-r-args-show)
(define-key ess-mode-map [f3] 'ess-r-args-insert)
(define-key inferior-ess-mode-map [f1] 'ess-R-object-tooltip)
(define-key inferior-ess-mode-map [f2] 'ess-r-args-show)
(define-key inferior-ess-mode-map [f3] 'ess-r-args-insert)
(setq ess-r-args-show-as 'tooltip)
(require 'ess-R-object-tooltip)

;; load ESS-View
(require 'ess-view)
(setq ess-view--spreadsheet-program "/opt/local/bin/gnumeric")

;; imaxima
;; for Lion users
(setq temporary-file-directory "/tmp")
;; for imaxima
(push "/usr/local/share/emacs/site-lisp" load-path)
(autoload 'imaxima "imaxima" "Maxima frontend" t)
(autoload 'imath-mode "imath" "Interactive Math mode" t)

;; shell mode
(add-hook 'sh-mode-hook '(lambda () (outline-minor-mode)))
;;(outline-minor-mode) ;; activate outline-minor-mode in ess by default

;;Dirtrack is a mode that keeps your shell buffers' directories in sync with
;;the shell. It does this by looking for the current directory in your prompt.
;;To use it, you need to set your prompt to match the regex, or vice versa.
(require 'dirtrack)

(defun shell-mode-switch-dirtrack ()
  "switch to dirtrack-mode when in shell-mode."
  ;;disables shell-dirtrack-mode
  (shell-dirtrack-mode nil)
  ;;sets local hostname to /
  ;;(comint-send-string (current-buffer) "export PS1=\"\u@\\w$ \"\n")
  ;; sets tracking variable for dirtrack-mode. regexp should extract "host:path" from your prompt.
  (setq dirtrack-list '("[a-zA-Z]*@\\([^$\t\r\n]*\\)\\$" 1))
  ;;enables dirtrack-mode instead
  (dirtrack-mode t))
(add-hook 'shell-mode-hook 'shell-mode-switch-dirtrack)

(defun make-comint-directory-tracking-work-remotely ()
  "Add this to `comint-mode-hook' to make directory tracking work while sshed into a remote host, e.g. for remote shell buffers started in tramp.  (This is a bug fix backported from Emacs 24: http://comments.gmane.org/gmane.emacs.bugs/39082."
  (set (make-local-variable 'comint-file-name-prefix)
       (or (file-remote-p default-directory) "")))
(add-hook 'comint-mode-hook 'make-comint-directory-tracking-work-remotely)

;; essh.el --- a set of commands that emulate for bash what ESS is to R
(require 'essh)
(defun essh-key-definitions ()
  (define-key sh-mode-map "\C-c\C-r" 'pipe-region-to-shell)
  (define-key sh-mode-map "\C-c\C-b" 'pipe-buffer-to-shell)
  (define-key sh-mode-map "\C-c\C-j" 'pipe-line-to-shell)
  (define-key sh-mode-map "\C-c\C-n" 'pipe-line-to-shell-and-step)
  (define-key sh-mode-map "\C-c\C-f" 'pipe-function-to-shell)
  (define-key sh-mode-map "\C-c\C-d" 'shell-cd-current-directory))
(add-hook 'shell-mode-hook 'essh-key-definitions)

;;--------------------------------------------------------------------
;; Lines enabling gnuplot-mode

;; move the files gnuplot.el to someplace in your Lisp load-path or
;; use a line like
;; (setq load-path (append (list "/path/to/gnuplot") load-path))

;; these lines enable the use of gnuplot mode
  (autoload 'gnuplot-mode "gnuplot" "gnuplot major mode" t)
  (autoload 'gnuplot-make-buffer "gnuplot" "open a buffer in gnuplot mode" t)

;; this line automatically causes all files with the .gp extension to
;; be loaded into gnuplot mode
  (setq auto-mode-alist (append '(("\\.gp$" . gnuplot-mode)) auto-mode-alist))

;; This line binds the function-9 key so that it opens a buffer into
;; gnuplot mode
  (global-set-key [(f9)] 'gnuplot-make-buffer)

;; end of line for gnuplot-mode
;;--------------------------------------------------------------------

(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(default ((t (:family "Monaco" :foundry "nil" :slant normal :weight normal :height 120 :width normal)))))

(provide 'init)

;;; init.el ends here
(put 'upcase-region 'disabled nil)
