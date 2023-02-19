;; Make startup faster by reducing the frequency of garbage collection and then use a hook to measure Emacs startup time.
;; The default is 800 kilobytes. Measured in bytes.
(setq gc-cons-threshold (* 50 1000 1000))
(setq read-process-output-max (* 1024 1024)) ;; 1mb
(add-hook 'emacs-startup-hook
          (lambda ()
            (message "*** Emacs loaded in %s seconds with %d garbage collections."
                     (emacs-init-time "%.2f")
                     gcs-done)))

;; Install straight.el
(defvar bootstrap-version)
(let ((bootstrap-file
       (expand-file-name "straight/repos/straight.el/bootstrap.el" user-emacs-directory))
      (bootstrap-version 5))
  (unless (file-exists-p bootstrap-file)
    (with-current-buffer
        (url-retrieve-synchronously
         "https://raw.githubusercontent.com/raxod502/straight.el/develop/install.el"
         'silent 'inhibit-cookies)
      (goto-char (point-max))
      (eval-print-last-sexp)))
  (load bootstrap-file nil 'nomessage))

(straight-use-package 'use-package)
(require 'use-package)

;; https://vxlabs.com/2022/06/12/typescript-development-with-emacs-tree-sitter-and-lsp-in-2022/
(use-package tree-sitter
	     :straight (tree-sitter :type git
				    :host github
				    :repo "ubolonton/emacs-tree-sitter"
				    :files ("lisp/*.el"))
	     :config (add-to-list 'tree-sitter-major-mode-language-alist '(rustic-mode . rust))
	     :hook ((python-mode rustic-mode) . tree-sitter-hl-mode))

(use-package tree-sitter-langs
	     :straight (tree-sitter-langs :type git
					  :host github
					  :repo "ubolonton/emacs-tree-sitter"
					  :files ("langs/*.el" "langs/queries"))
	     :after tree-sitter)

(use-package typescript-mode
	     :after tree-sitter
	     :config
	     ;; we choose this instead of tsx-mode so that eglot can automatically figure out language for server
	     ;; see https://github.com/joaotavora/eglot/issues/624 and https://github.com/joaotavora/eglot#handling-quirky-servers
	     (define-derived-mode typescriptreact-mode typescript-mode
	       "TypeScript TSX")

	     ;; use our derived mode for tsx files
	     (add-to-list 'auto-mode-alist '("\\.tsx?\\'" . typescriptreact-mode))
	     ;; by default, typescript-mode is mapped to the treesitter typescript parser
	     ;; use our derived mode to map both .tsx AND .ts -> typescriptreact-mode -> treesitter tsx
	     (add-to-list 'tree-sitter-major-mode-language-alist '(typescriptreact-mode . tsx)))
(setq-default typescript-indent-level 2)

(use-package tsi
	     :after tree-sitter
	     ;; define autoload definitions which when actually invoked will cause package to be loaded
	     :commands (tsi-typescript-mode tsi-json-mode tsi-css-mode)
	     :init
	     (add-hook 'typescript-mode-hook (lambda () (tsi-typescript-mode 1)))
	     (add-hook 'json-mode-hook (lambda () (tsi-json-mode 1)))
	     (add-hook 'css-mode-hook (lambda () (tsi-css-mode 1)))
	     (add-hook 'scss-mode-hook (lambda () (tsi-scss-mode 1))))

(use-package apheleia
  :straight (apheleia :host github :repo "radian-software/apheleia")
  :config
  (setf (alist-get 'prettier apheleia-formatters)
        '(npx "prettier"
              "--trailing-comma"  "es5"
              "--bracket-spacing" "true"
              "--single-quote"    "true"
              "--semi"            "false"
              "--print-width"     "100"
              file))
  (add-to-list 'apheleia-mode-alist '(typescript-mode . prettier))
  (apheleia-global-mode t))

;; https://sagot.dev/en/articles/emacs-typescript/
(straight-use-package 'lsp-mode)
(straight-use-package 'lsp-ui)
(require 'lsp-mode)
(setq lsp-headerline-breadcrumb-enable nil)
(setq lsp-ui-sideline-enable nil)
(add-hook 'typescript-mode-hook 'lsp-deferred)
(add-hook 'javascript-mode-hook 'lsp-deferred)

(straight-use-package 'sql)
(straight-use-package 'sql-indent)

(straight-use-package 'go-mode)
(straight-use-package 'go-autocomplete)
(straight-use-package 'golint)
(straight-use-package 'flymake-go)
(straight-use-package 'web-mode)
(straight-use-package 'markdown-mode)
(straight-use-package 'py-autopep8)
(straight-use-package 'jedi)
(straight-use-package 'pylint)

;; theme
(add-to-list 'custom-theme-load-path "~/.emacs.d/themes/")
;; (load-theme 'melancholy t)

(straight-use-package 'rainbow-mode)
(define-globalized-minor-mode my-global-rainbow-mode rainbow-mode
  (lambda () (rainbow-mode 1)))
(my-global-rainbow-mode 1)

;; list recent file
(recentf-mode 1)
(global-set-key (kbd "C-x C-r") 'recentf-open-files)

;; comment or uncomment shortcut
(global-set-key (kbd "C-x C-/") 'comment-or-uncomment-region)

;; enable history of minibuffer
(setq history-length 25)
(savehist-mode 1)

;; shows available key bindings
(straight-use-package 'which-key)
(setq which-key-idle-delay 2)
(which-key-mode 1)

;; Remember last cursor location
(save-place-mode 1)

;; Delete selection before insert
(delete-selection-mode t)

;; improve scrolling
(setq mouse-wheel-scroll-amount '(1 ((shift) . 1))) ;; one line at a time
(setq mouse-wheel-progressive-speed nil) ;; don't accelerate scrolling
(setq mouse-wheel-follow-mouse 't) ;; scroll window under mouse
(setq scroll-step 1) ;; keyboard scroll one line at a time

(straight-use-package 'which-key)
(company-mode 1)

;; for bashrc aliases
(setq shell-file-name "zsh")
(setq shell-command-switch "-ic")

;; open emacs init.el file shortcut
(defun find-user-init-file ()
  "Edit the `user-init-file', in another window."
  (interactive)
  (find-file-other-window user-init-file))
(global-set-key (kbd "C-c I") 'find-user-init-file)

;; open ffap (find file at point) Keybindings
(ffap-bindings)

;; disable toolbar and menubar
;; turn off toolbar
(menu-bar-mode 0)

;; show which function currently
(which-function-mode 1)

;; open two files vertical on startup not horizontal
(defun 2-windows-vertical-to-horizontal ()
  (let ((buffers (mapcar 'window-buffer (window-list))))
    (when (= 2 (length buffers))
      (delete-other-windows)
      (set-window-buffer (split-window-horizontally) (cadr buffers)))))
(add-hook 'emacs-startup-hook '2-windows-vertical-to-horizontal)

;; yasnippet
;; (require 'yasnippet)
;; (yas-global-mode 1)

;; web mode
;; (require 'web-mode)
;; (add-to-list 'auto-mode-alist '("\\.js\\'" . web-mode))
;; (add-to-list 'auto-mode-alist '("\\.jsx\\'" . web-mode))
;; (add-to-list 'auto-mode-alist '("\\.html\\'" . web-mode))
;; (add-to-list 'auto-mode-alist '("\\.phtml\\'" . web-mode))
;; (add-to-list 'auto-mode-alist '("\\.tpl\\.php\\'" . web-mode))
;; (add-to-list 'auto-mode-alist '("\\.[agj]sp\\'" . web-mode))
;; (add-to-list 'auto-mode-alist '("\\.as[cp]x\\'" . web-mode))
;; (add-to-list 'auto-mode-alist '("\\.erb\\'" . web-mode))
;; (add-to-list 'auto-mode-alist '("\\.mustache\\'" . web-mode))
;; (add-to-list 'auto-mode-alist '("\\.php\\'" . web-mode))
;; (add-to-list 'auto-mode-alist '("\\.djhtml\\'" . web-mode))

;; (defun my-web-mode-hook ()
;;   (setq web-mode-markup-indent-offset 2)
;;   (setq web-mode-code-indent-offset 2)
;;   (setq web-mode-code-indent-offset 2))
;; (add-hook 'web-mode-hook 'my-web-mode-hook)

;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; golang!
(require 'go-mode)
(require 'golint)
(require 'flymake-go)
(require 'go-autocomplete)
(require 'auto-complete-config)

(defun my-go-mode-hook ()
  ;; Use goimports instead of go-fmt
  (setq gofmt-command "goimports")
  ;; Call Gofmt before saving
  (add-hook 'before-save-hook 'gofmt-before-save)
  ;; Customize compile command to run go build
  (if (not (string-match "go" compile-command))
      (set (make-local-variable 'compile-command)
           "go build -v"))
  ; Godef jump key binding
  (local-set-key (kbd "M-.") 'godef-jump))
(add-hook 'go-mode-hook 'my-go-mode-hook)

;; Python
(add-hook 'python-mode-hook 'jedi:setup)
(setq jedi:complete-on-dot t)
(require 'py-autopep8)
(add-hook 'python-mode-hook 'py-autopep8-enable-on-save)
(setq py-autopep8-options '("--max-line-length=100"))

;; aligns annotation to the right hand side
(setq company-tooltip-align-annotations t)

;; Aliases
(defalias 'g 'goto-line)
(defalias 'wm 'web-mode)
(defalias 'gm 'go-mode)
(defalias 'hm 'html-mode)

(defun unix-file ()
      "Change the current buffer to Latin 1 with Unix line-ends."
      (interactive)
      (set-buffer-file-coding-system 'iso-latin-1-unix t))
