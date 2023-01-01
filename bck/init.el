(defvar bootstrap-version)
(let ((bootstrap-file
       (expand-file-name "straight/repos/straight.el/bootstrap.el" user-emacs-directory))
      (bootstrap-version 6))
  (unless (file-exists-p bootstrap-file)
    (with-current-buffer
        (url-retrieve-synchronously
         "https://raw.githubusercontent.com/radian-software/straight.el/develop/install.el"
         'silent 'inhibit-cookies)
      (goto-char (point-max))
      (eval-print-last-sexp)))
  (load bootstrap-file nil 'nomessage))

(straight-use-package 'use-package)
(require 'use-package)

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

(straight-use-package 'go-mode)
(straight-use-package 'go-autocomplete)
(straight-use-package 'golint)
(straight-use-package 'flymake-go)
(straight-use-package 'web-mode)
(straight-use-package 'markdown-mode)
(straight-use-package 'py-autopep8)
(straight-use-package 'jedi)
(straight-use-package 'pylint)

;; (defvar my-packages
;;   '(tide zenburn-theme yaml-mode volatile-highlights solarized-theme rainbow-mode sass-mode markdown-mode yasnippet-snippets web-mode s golint go-mode go-autocomplete flymake-go expand-region dash company py-autopep8 jedi pylint )
;;   "A list of packages to ensure are installed at launch.")

;; (defun my-packages-installed-p ()
;;   (cl-loop for p in my-packages
;;            when (not (package-installed-p p)) do (cl-return nil)
;;            finally (cl-return t)))

;; (unless (my-packages-installed-p)
;;   ;; check for new packages (package versions)
;;   (package-refresh-contents)
;;   ;; install the missing packages
;;   (dolist (p my-packages)
;;     (when (not (package-installed-p p))
;;       (package-install p))))

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
(require 'yasnippet)
(yas-global-mode 1)

;; web mode
(require 'web-mode)
(add-to-list 'auto-mode-alist '("\\.js\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.jsx\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.html\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.phtml\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.tpl\\.php\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.[agj]sp\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.as[cp]x\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.erb\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.mustache\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.php\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.djhtml\\'" . web-mode))

(defun my-web-mode-hook ()
  (setq web-mode-markup-indent-offset 2)
  (setq web-mode-code-indent-offset 2)
  (setq web-mode-code-indent-offset 2))
(add-hook 'web-mode-hook 'my-web-mode-hook)

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

;; Typescript
(defun setup-tide-mode ()
  (interactive)
  (tide-setup)
  (flycheck-mode +1)
  (setq flycheck-check-syntax-automatically '(save mode-enabled))
  (eldoc-mode +1)
  (tide-hl-identifier-mode +1)
  ;; company is an optional dependency. You have to
  ;; install it separately via package-install
  ;; `M-x package-install [ret] company`
  (company-mode +1))
(setq-default typescript-indent-level 2)
(add-to-list 'auto-mode-alist '("\\.tsx\\'" . typescript-mode))


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
