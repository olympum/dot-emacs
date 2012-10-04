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

(defvar my-packages '(starter-kit
                      starter-kit-lisp
                      starter-kit-eshell
                      starter-kit-bindings
                      starter-kit-js
                      clojure-mode
                      clojure-test-mode
                      rainbow-delimiters
                      ac-slime
                      markdown-mode
                      popup
                      ac-nrepl
                      cyberpunk-theme
                      clojure-mode
                      clojure-test-mode
                      elisp-slime-nav
                      find-file-in-project
                      idle-highlight-mode
                      ido-ubiquitous
                      magit
                      nrepl
                      paredit
                      protobuf-mode
                      smex
                      align-cljlet
                      nrepl-eval-sexp-fu
                      scss-mode
                      ))

(dolist (p my-packages)
  (when (not (package-installed-p p))
    (package-install p)))


;; Useful global settings as Emacs is used predominantely for Clojure development

;; Launch the Clojure repl via Leiningen - M-x clojure-jack-in 
;; Global shortcut definition to fire up clojure repl and connect to it

(global-set-key (kbd "C-c C-j") 'clojure-jack-in)

;; Colour mach parens and other structure characters to make code easy to follow
(global-rainbow-delimiters-mode)

;; auto-complete
(add-to-list 'load-path "~/.emacs.d/")
(require 'auto-complete-config)
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
            (enable-paredit-mode)
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
  (add-hook (intern (concat (symbol-name x) "-mode-hook")) 'enable-paredit-mode)
  (add-hook (intern (concat (symbol-name x) "-mode-hook")) 'rainbow-delimiters-mode))

;; highlight
(setq nrepl-eval-sexp-fu-flash-duration 0.5)

;; nrepl
(add-hook 'nrepl-interaction-mode-hook
          (lambda ()
            (nrepl-turn-on-eldoc-mode)
            (enable-paredit-mode)))

(add-hook 'nrepl-mode-hook
          (lambda ()
            (nrepl-turn-on-eldoc-mode)
            (enable-paredit-mode)
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

;; Fullscreen mode on Cocoa

(if (fboundp 'ns-toggle-fullscreen)
    (global-set-key [(meta return)] 'ns-toggle-fullscreen))

;;(when window-system (color-theme-solarized-dark))

;; fix the PATH variable
;; Commenting this code since the right fix under OS X is to modify
;; /etc/path and ~/.MacOSX/environment.plist with the list of paths
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
;;(load-theme 'wheatgrass)
;;(load-theme 'tango-dark)

;; rebind the shift-up xterm event to S-up since
;; emacs thinks it's <select>
(if (equal "xterm" (tty-type))
    (define-key input-decode-map "\e[1;2A" [S-up]))

(windmove-default-keybindings)

;; setup alternate keyboard mappings
;; leave left alt as dead key for special chars
(setq ns-alternate-modifier (quote meta))
;; use right alt as meta
(setq ns-right-alternate-modifier nil)

;; disable flyspell
(add-hook 'text-mode-hook 'turn-off-flyspell t)
(add-hook 'prog-mode-hook 'turn-off-flyspell t)

;; themes
(load-theme 'cyberpunk t)
