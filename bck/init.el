(require 'cl-lib)
(require 'package)
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)
(package-initialize)

(defvar my-packages
  '(tide zenburn-theme yaml-mode volatile-highlights solarized-theme rainbow-mode sass-mode markdown-mode yasnippet-snippets web-mode s golint go-mode go-autocomplete flymake-go expand-region dash company py-autopep8 jedi pylint)
  "A list of packages to ensure are installed at launch.")

(defun my-packages-installed-p ()
  (cl-loop for p in my-packages
           when (not (package-installed-p p)) do (cl-return nil)
           finally (cl-return t)))

(unless (my-packages-installed-p)
  ;; check for new packages (package versions)
  (package-refresh-contents)
  ;; install the missing packages
  (dolist (p my-packages)
    (when (not (package-installed-p p))
      (package-install p))))

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

;; Python
(add-hook 'python-mode-hook 'jedi:setup)
(setq jedi:complete-on-dot t)
(require 'py-autopep8)
(add-hook 'python-mode-hook 'py-autopep8-enable-on-save)
(setq py-autopep8-options '("--max-line-length=100"))

;; aligns annotation to the right hand side
(setq company-tooltip-align-annotations t)

;; formats the buffer before saving
;; (add-hook 'before-save-hook 'tide-format-before-save)
(add-hook 'typescript-mode-hook #'setup-tide-mode)

;; Aliases
(defalias 'g 'goto-line)
(defalias 'wm 'web-mode)
(defalias 'gm 'go-mode)
(defalias 'hm 'html-mode)

(defun unix-file ()
      "Change the current buffer to Latin 1 with Unix line-ends."
      (interactive)
      (set-buffer-file-coding-system 'iso-latin-1-unix t))


(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-safe-themes
   '("8aebf25556399b58091e533e455dd50a6a9cba958cc4ebb0aab175863c25b9a4" "d677ef584c6dfc0697901a44b885cc18e206f05114c8a3b7fde674fce6180879" "a8245b7cc985a0610d71f9852e9f2767ad1b852c2bdea6f4aadc12cce9c4d6d0" default))
 '(package-selected-packages
   '(typescript-mode expand-region flymake-go flymake-google-cpplint go-autocomplete go-mode golint web-mode yasnippet-snippets markdown-mode sass-mode rainbow-mode solarized-theme volatile-highlights yaml-mode zenburn-theme tide)))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
