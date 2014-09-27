;;;
;; init.el emacs config
;; requires emacs 24+
;;;

(require 'package)
(add-to-list 'package-archives
             '("marmalade" . "http://marmalade-repo.org/packages/"))
(add-to-list 'package-archives
             '("melpa" . "http://melpa.milkbox.net/packages/") t)

(package-initialize)

(when (not package-archive-contents)
 	(package-refresh-contents))

(defvar my-packages '(ac-nrepl
                      ac-slime
                      ac-js2
                      align-cljlet
                      auto-complete
                      cider
                      clojure-mode
                      clojure-test-mode
                      cyberpunk-theme
                      dash
                      direx
                      elisp-slime-nav
                      epl
                      f
                      find-file-in-project
                      flycheck
                      git-commit-mode
                      git-rebase-mode
                      go-autocomplete
                      go-direx
                      go-eldoc
                      go-errcheck
                      go-mode
                      handlebars-mode
                      idle-highlight-mode
                      ido-ubiquitous
                      js2-mode
                      js2-refactor
                      json-mode
                      json-reformat
                      less-css-mode
                      magit
                      markdown-mode
                      monokai-theme
                      nrepl
                      nrepl-eval-sexp-fu
                      paredit
                      pkg-info
                      popup
                      protobuf-mode
                      rainbow-delimiters
                      s
                      scala-mode2
                      scss-mode
                      smex
                      sr-speedbar
                      starter-kit
                      starter-kit-bindings
                      starter-kit-eshell
                      starter-kit-lisp
                      sublime-themes
                      tree-mode))

(dolist (p my-packages)
  (when (not (package-installed-p p))
    (package-install p)))

(add-to-list 'exec-path "/usr/local/bin")
(setenv "PATH" (concat (getenv "PATH") ":/usr/local/bin"))
(setq exec-path (append exec-path '("/usr/local/bin")))

;; frame geometry
; default window width and height
(defun custom-set-frame-size ()
  (add-to-list 'default-frame-alist '(height . 65))
  (add-to-list 'default-frame-alist '(width . 120)))
(custom-set-frame-size)
(add-hook 'before-make-frame-hook 'custom-set-frame-size)

;; Useful global settings as Emacs is used predominantely for Clojure development

;; Launch the Clojure repl via Leiningen - M-x clojure-jack-in 
;; Global shortcut definition to fire up clojure repl and connect to it

(global-set-key (kbd "C-c C-j") 'clojure-jack-in)

;; ;; scala
;; ;; load the ensime lisp code...
;; (add-to-list 'load-path "~/.emacs.d/vendor/ensime/elisp/")
;; (require 'ensime)

;; ;; This step causes the ensime-mode to be started whenever
;; ;; scala-mode is started for a buffer. You may have to customize this step
;; ;; if you're not using the standard scala mode.
;; (add-hook 'scala-mode-hook 'ensime-scala-mode-hook)

;; dirtree
(add-to-list 'load-path "~/.emacs.d/vendor/dirtree")
(autoload 'dirtree "dirtree" "Add directory to tree view" t)

;; golang settings requires godef (for jumping into symbol definition
;; and gocode for expanding / autocomplete symbol signatures)
;;
;; go get code.google.com/p/rog-go/exp/cmd/godef
;;
;; go get -u github.com/nsf/gocode
;;
;; See also: http://honnef.co/posts/2013/03/writing_go_in_emacs/

(setenv "GOPATH" (concat (getenv "HOME") "/Projects/golang"))
(setenv "PATH" (concat (getenv "PATH") ":" (concat (getenv "HOME") "/Projects/golang/bin")))
(setq exec-path (append exec-path (list (expand-file-name (concat (getenv "HOME") "/Projects/golang/bin")))))

(add-hook 'before-save-hook 'gofmt-before-save)

;; the following three lines are required to add gocode autocomplete
;; to auto-complete mode, and should be removed as soon as this code
;; merges into the ac mode. for this to ac mode to run with go files,
;; the gocode executable must be found in the exec-path.
(add-to-list 'load-path "~/.emacs.d/vendor/autocomplete")
(require 'go-autocomplete)
(require 'auto-complete-config)

(add-to-list 'load-path "~/.emacs.d/vendor/goflymake")
(require 'go-flycheck)
(add-hook 'go-mode-hook 'flycheck-mode)

;; auto-complete

(ac-config-default)

(ac-flyspell-workaround)

(global-auto-complete-mode t)
(setq ac-auto-show-menu t)
(setq ac-dwim t)
(setq ac-use-menu-map t)
(setq ac-quick-help-delay 1)
(setq ac-quick-help-height 60)

(set-default 'ac-sources
             '(ac-source-dictionary
               ac-source-words-in-buffer
               ac-source-words-in-same-mode-buffers
               ac-source-words-in-all-buffer
               ac-source-yasnippet
               ac-source-semantic))

(dolist (mode '(magit-log-edit-mode log-edit-mode org-mode text-mode haml-mode
                sass-mode yaml-mode csv-mode espresso-mode haskell-mode
                html-mode nxml-mode sh-mode smarty-mode clojure-mode
                lisp-mode textile-mode markdown-mode tuareg-mode))
  (add-to-list 'ac-modes mode))

;;;;Key triggers
(define-key ac-completing-map (kbd "C-M-n") 'ac-next)
(define-key ac-completing-map (kbd "C-M-p") 'ac-previous)
(define-key ac-completing-map "\t" 'ac-complete)
(define-key ac-completing-map (kbd "M-RET") 'ac-help)
(define-key ac-completing-map "\r" 'nil)

;; clojure setup

(eval-after-load 'clojure-mode
  '(font-lock-add-keywords
    'clojure-mode `(("(\\(fn\\)[\[[:space:]]"
                     (0 (progn (compose-region (match-beginning 1)
                                               (match-end 1) "λ")
                               nil))))))

(eval-after-load 'clojure-mode
  '(font-lock-add-keywords
    'clojure-mode `(("\\(#\\)("
                     (0 (progn (compose-region (match-beginning 1)
                                               (match-end 1) "ƒ")
                               nil))))))

(eval-after-load 'clojure-mode
  '(font-lock-add-keywords
    'clojure-mode `(("\\(#\\){"
                     (0 (progn (compose-region (match-beginning 1)
                                               (match-end 1) "∈")
                               nil))))))

(eval-after-load 'find-file-in-project
  '(add-to-list 'ffip-patterns "*.clj"))

(require 'clojure-mode)

(add-hook 'clojure-mode-hook
          (lambda ()
            (paredit-mode)
            (rainbow-delimiters-mode)
            (add-to-list 'ac-sources 'ac-source-yasnippet)
            (setq buffer-save-without-query t)))

;;command to align let statements
;;To use: M-x align-cljlet

;;Treat hyphens as a word character when transposing words
(defvar clojure-mode-with-hyphens-as-word-sep-syntax-table
  (let ((st (make-syntax-table clojure-mode-syntax-table)))
    (modify-syntax-entry ?- "w" st)
    st))

(setq auto-mode-alist (append '(("\\.cljs$" . clojure-mode))
                              auto-mode-alist))

(dolist (x '(scheme emacs-lisp lisp))
  (add-hook (intern (concat (symbol-name x) "-mode-hook")) 'paredit-mode)
  (add-hook (intern (concat (symbol-name x) "-mode-hook")) 'rainbow-delimiters-mode))

;; highlight
(setq nrepl-eval-sexp-fu-flash-duration 0.5)

;; nrepl
(add-hook 'nrepl-interaction-mode-hook
          (lambda ()
            (nrepl-turn-on-eldoc-mode)
            (paredit-mode)))

(add-hook 'nrepl-mode-hook
          (lambda ()
            (nrepl-turn-on-eldoc-mode)
            (paredit-mode)
            (define-key nrepl-mode-map
              (kbd "{") 'paredit-open-curly)
            (define-key nrepl-mode-map
              (kbd "}") 'paredit-close-curly)))

(setq nrepl-popup-stacktraces nil)
(add-to-list 'same-window-buffer-names "*nrepl*")

;;Auto Complete
(add-hook 'nrepl-mode-hook 'ac-nrepl-setup)
(add-hook 'nrepl-interaction-mode-hook 'ac-nrepl-setup)
(eval-after-load "auto-complete"
  '(add-to-list 'ac-modes 'nrepl-mode))

;; javascript use js2-mode instead of built-in js-mode (formerly
;; espresso-mode)
(autoload 'js2-mode "js2-mode" nil t)
(add-to-list 'auto-mode-alist '("\\.js$" . js2-mode))
(add-to-list 'auto-mode-alist '("\\.json$" . js2-mode))

(setq js-indent-level 2)
(setq js2-indent-level 2)
(setq js2-basic-offset 2)

(add-hook 'js2-mode-hook '(lambda ()
                            (local-set-key (kbd "RET") 'newline-and-indent)))

;;;;;;;;;;;;;;;;;;;;
;; set up unicode
(prefer-coding-system       'utf-8)
(set-default-coding-systems 'utf-8)
(set-terminal-coding-system 'utf-8)
(set-keyboard-coding-system 'utf-8)

;; Save desktop
(desktop-save-mode t)

;; default to unified diffs
(setq diff-switches "-u")

;; Markdown mode
(setq markdown-command "~/.emacs.d/markdown.pl | ~/.emacs.d/SmartyPants.pl")
(add-to-list 'auto-mode-alist '("\\.txt" . markdown-mode))
(add-to-list 'auto-mode-alist '("\\.md" . markdown-mode))
(setq markdown-css-path  (substitute-in-file-name "$HOME/.emacs.d/main.css"))

;; Requires Marked.app installed (non free - highly recommended though
;; to preview markdown).
(defun markdown-preview-file ()
  "run Marked on the current file and revert the buffer"
  (interactive)
  (shell-command 
   (format "open -a /Applications/Marked.app %s" 
       (shell-quote-argument (buffer-file-name))))
)
(global-set-key "\C-cm" 'markdown-preview-file)

;; fix the PATH variable
;; Commenting this code since the right fix under OS X is to modify
;; /etc/paths and ~/.MacOSX/environment.plist with the list of paths
;;
;; (defun set-exec-path-from-shell-PATH ()
;;   (let ((path-from-shell (shell-command-to-string "$SHELL -i -c 'echo $PATH'")))
;;   (setenv "PATH" path-from-shell)
;;   (setq exec-path (split-string path-from-shell path-separator))))
;;
;; (if window-system (set-exec-path-from-shell-PATH))

(setq make-backup-files nil)
(setq auto-save-default nil)
(setq-default tab-width 2)
(setq-default indent-tabs-mode nil)
(setq inhibit-startup-message t)

(fset 'yes-or-no-p 'y-or-n-p)

(delete-selection-mode t)
(scroll-bar-mode -1)
(tool-bar-mode -1)
(blink-cursor-mode t)
(show-paren-mode t)
(column-number-mode t)
(set-fringe-style -1)
(tooltip-mode -1)
(global-linum-mode 1)
(global-visual-line-mode 1)
(setq linum-format " %d  ")
(menu-bar-mode nil)
(setq speedbar-show-unknown-files t)

(set-frame-font "Menlo-12")
(setq frame-title-format t)

;; rebind the shift-up xterm event to S-up since emacs thinks it's <select>
(if (equal "xterm" (tty-type))
    (define-key input-decode-map "\e[1;2A" [S-up]))

(windmove-default-keybindings)

;; setup alternate keyboard mappings
;; leave right alt as dead key for special chars
(setq ns-alternate-modifier (quote meta))
;; use left alt as meta
(setq ns-right-alternate-modifier nil)

;; disable flyspell
(add-hook 'text-mode-hook 'turn-off-flyspell t)
(add-hook 'prog-mode-hook 'turn-off-flyspell t)

;; themes
(load-theme 'monokai t)
;;(load-theme 'zenburn t)
;;(load-theme 'spolsky t) ;; from sublime-themes package

;;(set-frame-parameter (selected-frame) 'alpha '(<active> [<inactive>]))
;;(set-frame-parameter (selected-frame) 'alpha '(90 90))
;;(add-to-list 'default-frame-alist '(alpha 90 90))

;; avoid compiling scss at save
(setq scss-compile-at-save nil)
