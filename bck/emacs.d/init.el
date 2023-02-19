;; Performance
;; ----------------------------------------------------------------------
;; Make startup faster by reducing the frequency of garbage collection and use a hook to measure Emacs startup time
;; The default is 800 kilobytes. Measured in bytes
(setq gc-cons-threshold (* 50 1000 1000))
(setq read-process-output-max (* 1024 1024)) ;; 1mb
(add-hook 'emacs-startup-hook
          (lambda ()
            (message "*** Emacs loaded in %s seconds with %d garbage collections."
                     (emacs-init-time "%.2f")
                     gcs-done)))

;; Package system
;; ----------------------------------------------------------------------
(eval-and-compile
  (customize-set-variable
   'package-archives '(("melpa" . "https://melpa.org/packages/")
                       ("gnu" . "https://elpa.gnu.org/packages/")))
  (package-initialize)
  (unless (package-installed-p 'use-package)
    (package-refresh-contents)
    (package-install 'use-package)))

(unless (package-installed-p 'quelpa)
  (with-temp-buffer
    (url-insert-file-contents "https://raw.githubusercontent.com/quelpa/quelpa/master/quelpa.el")
    (eval-buffer)
    (quelpa-self-upgrade)))

(quelpa
 '(quelpa-use-package
   :fetcher git
   :url "https://github.com/quelpa/quelpa-use-package.git"))
(require 'quelpa-use-package)

;; Fast syntax highlighting & indentation
;; ----------------------------------------------------------------------
;; from https://vxlabs.com/2022/06/12/typescript-development-with-emacs-tree-sitter-and-lsp-in-2022/
(use-package tree-sitter
  :ensure t
  :config
  ;; activate tree-sitter on any buffer containing code for which it has a parser available
  (global-tree-sitter-mode)
  ;; you can easily see the difference tree-sitter-hl-mode makes for python, ts or tsx
  ;; by switching on and off
  (add-hook 'tree-sitter-after-on-hook #'tree-sitter-hl-mode))

(use-package tree-sitter-langs
  :ensure t
  :after tree-sitter)

;; https://github.com/orzechowskid/tsi.el/
;; great tree-sitter-based indentation for typescript/tsx, css, json
(use-package tsi
  :after tree-sitter
  :quelpa (tsi :fetcher github :repo "orzechowskid/tsi.el")
  ;; define autoload definitions which when actually invoked will cause package to be loaded
  :commands (tsi-typescript-mode tsi-json-mode tsi-css-mode)
  :init
  (add-hook 'typescript-mode-hook (lambda () (tsi-typescript-mode 1)))
  (add-hook 'json-mode-hook (lambda () (tsi-json-mode 1)))
  (add-hook 'css-mode-hook (lambda () (tsi-css-mode 1)))
  (add-hook 'scss-mode-hook (lambda () (tsi-scss-mode 1))))

;; ----------------------------------------------------------------------
;; Language support
;; ----------------------------------------------------------------------
(use-package lsp-mode :ensure t)
(use-package lsp-ui :ensure t)
(require 'lsp-mode)
(setq lsp-headerline-breadcrumb-enable nil)
(setq lsp-ui-sideline-enable nil)
(add-hook 'typescript-mode-hook 'lsp-deferred)
(add-hook 'javascript-mode-hook 'lsp-deferred)

(use-package sql :ensure t)
(use-package sql-indent :ensure t)
(use-package web-mode :ensure t)
(use-package markdown-mode :ensure t)

;; Typescript
;; ----------------------------------------------------------------------
;; Install language server
;;  - sudo npm i -g typescript-language-server; sudo npm i -g typescript
(use-package typescript-mode
  :ensure t
  :after tree-sitter
  :config
  ;; we choose this instead of tsx-mode so that eglot can automatically figure out language for server
  ;; see https://github.com/joaotavora/eglot/issues/624 and https://github.com/joaotavora/eglot#handling-quirky-servers
  (define-derived-mode typescriptreact-mode typescript-mode "TypeScript TSX")
  ;; use our derived mode for tsx files
  (add-to-list 'auto-mode-alist '("\\.tsx?\\'" . typescriptreact-mode))
  ;; by default, typescript-mode is mapped to the treesitter typescript parser
  ;; use our derived mode to map both .tsx AND .ts -> typescriptreact-mode -> treesitter tsx
  (add-to-list 'tree-sitter-major-mode-language-alist '(typescriptreact-mode . tsx)))

;; Deno
;; ----------------------------------------------------------------------
;; Workaround by piggy backing js-mode
(use-package typescript-mode
  :ensure t
  :hook (typescript-mode . (lambda ()
			     (setq lsp-disabled-clients '(ts-ls))
			     (lsp-deferred))))
(add-hook 'js-mode-hook (lambda ()
			  (setq lsp-disabled-clients '())
			  (lsp-deferred)))
;; auto-complete
(use-package company
  :ensure t
  :config
  (add-to-list 'company-backends 'company-capf)
  (global-company-mode))

;; auto-formatting different source code files
;; https://github.com/radian-software/apheleia
(use-package apheleia
  :ensure t
  :config  (setf (alist-get 'prettier apheleia-formatters)
		 '(npx "prettier"
		       "--trailing-comma"  "es5"
		       "--bracket-spacing" "true"
		       "--single-quote"    "true"
		       "--semi"            "false"
		       "--print-width"     "100"
		       file))
  (add-to-list 'apheleia-mode-alist '(typescript-mode . prettier))
  (apheleia-global-mode +1))

;; Utils
;; ----------------------------------------------------------------------

;; color hex code
(use-package rainbow-mode :ensure t)
(define-globalized-minor-mode my-global-rainbow-mode rainbow-mode
  (lambda () (rainbow-mode 1)))
(my-global-rainbow-mode 1)

;; list recent file
(recentf-mode 1)
(global-set-key (kbd "C-x C-r") 'recentf-open-files)

;; enable history of minibuffer
(setq history-length 25)
(savehist-mode 1)

;; shows available key bindings
(use-package which-key :ensure t)
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

;; import bashrc aliases
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

(defun unix-file ()
  "Change the current buffer to Latin 1 with Unix line-ends."
  (interactive)
  (set-buffer-file-coding-system 'iso-latin-1-unix t))

;; Shortcuts
;; ----------------------------------------------------------------------

;; goto line
(global-set-key (kbd "C-x C-g") 'goto-line)

;; commenting
(global-set-key (kbd "C-x /") 'comment-or-uncomment-region)


(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages '(apheleia tree-sitter-langs tree-sitter use-package)))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
