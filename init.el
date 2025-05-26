;; -*- lexical-binding: t; -*-
(setq make-backup-files nil) ; stop creating ~ files
(setq custom-file (concat user-emacs-directory "custom.el"))
(when (file-exists-p custom-file)
  (load custom-file))
(require 'package)
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)
(package-initialize)

(package-install 'vertico)
(package-install 'corfu)
(package-install 'marginalia)
(package-install 'nerd-icons-completion)
(package-install 'nerd-icons-corfu)
(package-install 'hotfuzz)
(package-install 'consult)
(package-install 'embark)
(package-install 'rainbow-delimiters)
(package-install 'ef-themes)
(package-install 'doom-themes)
(package-install 'doom-modeline)
(package-install 'gptel)

(package-install 'magit)

(package-install 'flycheck)
(package-install 'markdown-mode)
(package-install 'evil)
(package-install 'ace-window)
(package-install 'treemacs)
(package-install 'evil-escape)
(package-install 'restart-emacs)
(package-install 'golden-ratio)
(package-install 'dap-mode)
(package-install 'evil-collection)
(package-install 'evil-goggles)
(package-install 'vterm)
(package-install 'helpful)
(package-install 'beacon)
(package-install 'ample-theme)
(package-install 'embark-consult)

(package-install 'crux)
(package-install 'rainbow-identifiers)
(package-install 'org-modern)
(package-install 'solaire-mode)
(package-install 'centaur-tabs)
(package-install 'keyfreq)
(package-install 'org-appear)
(package-install 'org-fancy-priorities)
(package-install 'casual)
(package-install 'zop-to-char)
(package-install 'popper)
(package-install 'xkcd)
(package-install 'catppuccin-theme)
(package-install 'eros)
(package-install 'hackernews)
(package-install 'org-alert)
(package-install 'vscode-dark-plus-theme)
(package-install 'magit-delta)
(package-install 'org-dashboard)
(package-install 'breadcrumb)
(package-install 'org-kanban)
(package-install 'stimmung-themes)


(load-theme 'doom-badger t)
(menu-bar-mode -1)
(tool-bar-mode -1)
(scroll-bar-mode -1)

(add-hook 'magit-mode-hook (lambda () (magit-delta-mode 1)))
(solaire-global-mode 1)
(setq centaur-tabs-style "bar")
(setq centaur-tabs-height 42)
(setq centaur-tabs-set-icons t)
(setq centaur-tabs-icon-type 'all-the-icons)  ; or 'nerd-icons
(setq centaur-tabs-gray-out-icons 'buffer)
(setq centaur-tabs-set-bar 'left)
(setq centaur-tabs-set-bar 'over)
(setq centaur-tabs-set-modified-marker t)
(centaur-tabs-mode 1)

(setq evil-collection-setup-minibuffer t
      evil-escape-key-sequence "jk"
      evil-want-keybinding nil)

(evil-mode 1)
(evil-collection-init)
(evil-escape-mode 1)
(evil-goggles-mode 1)
(evil-goggles-use-diff-faces)

(load-file "~/.config/emacs/misc.el")
(load-file "~/.config/emacs/setup-ts.el")
;;(load-file "~/Schreibtisch/hl-block.el")

(vertico-mode 1)
(setq vertico-mouse-mode t)
(marginalia-mode 1)
(nerd-icons-completion-mode 1)

(require 'hotfuzz)
(setq completion-styles '(hotfuzz))

(defun setup-corfu ()
  (corfu-mode 1)
  (corfu-popupinfo-mode 1)
  ;;(my/add-conditional-toolbar-button)
  (setq corfu-auto t
	corfu-auto-delay 0.0
	corfu-auto-prefix 1
	corfu-echo-mode t
	corfu-popupinfo-delay 0
	corfu-min-width 50
	corfu-max-width 50
	corfu-echo-mode t
	corfu-echo-delay 0
	corfu-indexed-mode t
	corfu-preview-current 'insert
	corfu-popupinfo-max-width 50
	corfu-popupinfo-max-height 50))

(defun setup-prog-mode ()
  (setup-corfu)
  (rainbow-delimiters-mode 1)
  (electric-pair-mode 1)
  (hl-line-mode 1)
  (beacon-mode 1)
  (setq treesit-font-lock-level 4
	eldoc-idle-delay 0
	eldoc-documentation-strategy #'eldoc-documentation-enthusiast))

(defun setup-go-mode ()
  (eglot-ensure)
  (breadcrumb-mode 1)
  (load-file "~/Schreibtisch/hl-block.el")
  (hl-block-mode 1))

(add-to-list 'auto-mode-alist
             '("\\.go\\'" . (lambda ()
                              (go-ts-mode))))

(add-hook 'go-ts-mode-hook 'setup-go-mode)
  
(require 'corfu)
(add-to-list 'corfu-margin-formatters #'nerd-icons-corfu-formatter)
(add-hook 'prog-mode-hook #'setup-prog-mode)  ; All programming modes
 
(global-set-key (kbd "C-x b") 'consult-buffer)
(global-set-key (kbd "C-.") 'embark-act)
(global-set-key (kbd "M-a") 'ace-window)

(doom-modeline-mode 1)

(global-set-key (kbd "C-h f") #'helpful-callable)
(global-set-key (kbd "C-h v") #'helpful-variable)
(global-set-key (kbd "C-h k") #'helpful-key)
(global-set-key (kbd "C-h x") #'helpful-command)
(global-set-key (kbd "C-c C-d") #'helpful-at-point)
(global-set-key (kbd "C-h F") #'helpful-function)
