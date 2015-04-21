(setq default-directory "/home/long/Workspaces/")
(setq inhibit-startup-message t)

;; for bashrc aliases
(setq shell-file-name "bash")
(setq shell-command-switch "-ic")

;; open emacs init.el file shortcut
(defun find-user-init-file ()
  "Edit the `user-init-file', in another window."
  (interactive)
  (find-file-other-window user-init-file))
(global-set-key (kbd "C-c I") 'find-user-init-file)

;; open ffap (find file at point) Keybindings
(ffap-bindings)

;; ----------------------------------------------------------------------
;; doxymacs
;; ----------------------------------------------------------------------
(add-hook 'c-mode-common-hook 'doxymacs-mode)
(defun my-doxymacs-font-lock-hook ()
  (if (or (eq major-mode 'c-mode) (eq major-mode 'c++-mode))
      (doxymacs-font-lock)))
(add-hook 'font-lock-mode-hook 'my-doxymacs-font-lock-hook)

(setq doxymacs-file-comment-template 
 '(
   "/**********************************************************************" > n
   " *" > n
   " * " (doxymacs-doxygen-command-char) "file   "
   (if (buffer-file-name)
       (file-name-nondirectory (buffer-file-name))
     "") > n
     " * " (doxymacs-doxygen-command-char) "author Long Hoang <long@mindworker.de>" > n
     " * " > n
     " * " (doxymacs-doxygen-command-char) "brief  " > n
     " * " > n
     " **********************************************************************/" > n > n
     ))

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

;; php mode
(autoload 'php-mode "php-mode" "Major mode for editing php code." t)
(add-to-list 'auto-mode-alist '("\\.php$" . php-mode))
(add-to-list 'auto-mode-alist '("\\.inc$" . php-mode))

;; go mode
(add-to-list 'load-path "~/habitat/go/")
(require 'go-mode-load)
(setenv "PATH" "/usr/bin:/bin:/usr/sbin:/sbin:/usr/local/bin:/usr/local/go/bin")
(setenv "GOPATH" "~/Workspaces/go")
;; tab size for go
(defun my-go-mode-hook () 
  (add-hook 'before-save-hook 'gofmt-before-save) 
  (setq tab-width 4 indent-tabs-mode 1)) 
(add-hook 'go-mode-hook 'my-go-mode-hook) 
