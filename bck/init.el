(load-library "url-handlers")

(setq default-directory "/home/long/Workspaces/")
;; (setq inhibit-startup-message t)

;;; I prefer cmd key for meta
(setq mac-option-key-is-meta nil
      mac-command-key-is-meta t
      mac-command-modifier 'meta
      mac-option-modifier 'none)

;; use package and add MELPA
;; Package managers
(require 'package)
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)
(package-initialize) 
;; (package-refresh-contents) ;; uncomment if needed for new packages
 (setq mac-option-modifier 'super)
 (setq mac-command-modifier 'meta)
 
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
(tool-bar-mode -1)
(menu-bar-mode -1)

;; show which function currently
(which-function-mode 1)

;; make it hard for me in order to learn proper keys
;; (add-to-list 'load-path "~/.emacs.d/plugins/")
;; (require 'no-easy-keys)
;; (no-easy-keys 1)

;; ----------------------------------------------------------------------
;; doxymacs
;; ----------------------------------------------------------------------
; (add-hook 'c-mode-common-hook 'doxymacs-mode)
; (defun my-doxymacs-font-lock-hook ()
;   (if (or (or (eq major-mode 'c-mode) (eq major-mode 'c++-mode)) (eq major-mode 'go-mode))
;       (doxymacs-font-lock)))
; (add-hook 'font-lock-mode-hook 'my-doxymacs-font-lock-hook)

; (setq doxymacs-file-comment-template 
;  '(
;    "/**********************************************************************" > n
;    " *" > n
;    " * " (doxymacs-doxygen-command-char) "file   "
;    (if (buffer-file-name)
;        (file-name-nondirectory (buffer-file-name))
;      "") > n
;      " * " (doxymacs-doxygen-command-char) "author Long Hoang <long@mindworker.de>" > n
;      " * " > n
;      " * " (doxymacs-doxygen-command-char) "brief  " > n
;      " * " > n
;      " **********************************************************************/" > n > n
;      ))

;; ----------------------------------------------------------------------

;; open two files vertical on startup not horizontal
(defun 2-windows-vertical-to-horizontal ()
  (let ((buffers (mapcar 'window-buffer (window-list))))
    (when (= 2 (length buffers))
      (delete-other-windows)
      (set-window-buffer (split-window-horizontally) (cadr buffers)))))
(add-hook 'emacs-startup-hook '2-windows-vertical-to-horizontal)

;; yasnippet
(add-to-list 'load-path
	     "~/.emacs.d/plugins/yasnippet")
(require 'yasnippet)
(yas-global-mode 1)

;; web mode
(add-to-list 'load-path
	     "~/.emacs.d/plugins/web-mode")
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

;; go mode
;;(add-to-list 'load-path "~/.emacs.d/plugins/go-mode.el/")
;;(require 'go-mode-autoloads)

(defun set-exec-path-from-shell-PATH ()
  (let ((path-from-shell (replace-regexp-in-string
                          "[ \t\n]*$"
                          ""
                          (shell-command-to-string "$SHELL --login -i -c 'echo $PATH'"))))
    (setenv "PATH" path-from-shell)
    (setq eshell-path-env path-from-shell) ; for eshell users
    (setq exec-path (split-string path-from-shell path-separator))))

(when window-system (set-exec-path-from-shell-PATH))

(defun my-go-mode-hook ()
  ; Call Gofmt before saving                                                    
  (add-hook 'before-save-hook 'gofmt-before-save)
  ; Godef jump key binding                                                      
  (local-set-key (kbd "M-.") 'godef-jump))
(add-hook 'go-mode-hook 'my-go-mode-hook)
;; tab size for go
(defun my-go-mode-hook () 
  (add-hook 'before-save-hook 'gofmt-before-save) 
  (setq tab-width 4 indent-tabs-mode 1)) 
(add-hook 'go-mode-hook 'my-go-mode-hook) 

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

;; golint
(add-to-list 'load-path "/Users/long/Workspaces/go/src/github.com/golang/lint/misc/emacs")
(require 'golint)

;; go todo
;;  - go oracle


;; flymake mode which shows errors while coding
(eval-after-load "go-mode"
  '(require 'flymake-go))

;; Autocomplete
(add-to-list 'load-path "~/.emacs.d/go")
(add-to-list 'load-path "~/.emacs.d/lisp")
(require 'go-autocomplete)
(require 'auto-complete-config)
(add-to-list 'ac-dictionary-directories "~/.emacs.d/lisp/ac-dict")
(ac-config-default)

;; go-eldoc shows function header
(add-hook 'go-mode-hook 'go-eldoc-setup)
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages
   (quote
    (wakatime-mode solidity-mode web-mode js2-mode jsx-mode yasnippet go-eldoc flymake-go)))
 '(wakatime-cli-path "/usr/local/bin/wakatime")
 '(wakatime-python-bin "/usr/local/bin/python")
 '(xterm-mouse-mode t))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )

;; Aliases
(defalias 'g 'goto-line)
(defalias 'wm 'web-mode)
(defalias 'gm 'go-mode)
(defalias 'hm 'html-mode)

(defun unix-file ()
      "Change the current buffer to Latin 1 with Unix line-ends."
      (interactive)
      (set-buffer-file-coding-system 'iso-latin-1-unix t))

;; sol mode
(add-to-list 'load-path
	     "~/.emacs.d/plugins")
(require 'solidity-mode)
