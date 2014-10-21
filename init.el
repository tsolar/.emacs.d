(setq package-archives '(("gnu" . "http://elpa.gnu.org/packages/")
			 ;; ("marmalade" . "http://marmalade-repo.org/packages/")
			 ("melpa" . "http://melpa.milkbox.net/packages/")))


(add-to-list 'load-path "~/.emacs.d")
(add-to-list 'load-path "~/.emacs.d/elpa")

;;;;;;;;;;;;; General settings
;; Turn on syntax colouring in all modes supporting it:
(global-font-lock-mode t)

(recentf-mode 1) ; keep a list of recently opened files
(delete-selection-mode 1) ;; replace selection when typing

(setq search-highlight           t) ; Highlight search object
(setq query-replace-highlight    t) ; Highlight query object
(setq mouse-sel-retain-highlight t) ; Keep mouse high-lightening

(setq stack-trace-on-error t)

;; stop annoying questions
(setq-default abbrev-mode t)
;;(read-abbrev-file “~/.abbrev_defs”)
(setq save-abbrevs t)

;; delete trailing whitespaces!
(add-hook 'before-save-hook 'delete-trailing-whitespace)

(fset 'yes-or-no-p 'y-or-n-p)

;; mover línea hacia arriba
(defun move-line-up ()
  (interactive)
  (transpose-lines 1)
  (forward-line -2))

;; mover línea hacia abajo
(defun move-line-down ()
  (interactive)
  (forward-line 1)
  (transpose-lines 1)
  (forward-line -1))


(global-set-key (kbd "S-M-<up>") 'move-line-up)
(global-set-key (kbd "S-M-<down>") 'move-line-down)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; revert

(global-auto-revert-mode 1)
(setq auto-revert-verbose nil)

(global-set-key (kbd "<f5>") 'revert-buffer)

;; end revert

;;; Variables
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(ansi-color-names-vector ["#212526" "#ff4b4b" "#b4fa70" "#fce94f" "#729fcf" "#ad7fa8" "#8cc4ff" "#eeeeec"])
 '(blink-cursor-mode nil)
 '(c-default-style "bsd")
 '(c-tab-always-indent nil)
 '(column-number-mode t)
 '(cua-mode t nil (cua-base))
 '(custom-enabled-themes (quote (deeper-blue)))
 '(display-battery-mode t)
 '(display-time-mode t)
 '(electric-pair-mode t)
 '(erc-auto-query (quote window-noselect))
 '(erc-autoaway-mode t)
 '(erc-away-nickname nil)
 '(erc-join-buffer (quote window-noselect))
 '(erc-modules (quote (completion list menu scrolltobottom autojoin button dcc fill irccontrols match move-to-prompt netsplit networks noncommands readonly ring stamp spelling track)))
 '(erc-nick-notify-mode t)
 '(erc-prompt ">")
 '(erc-public-away-p t)
 '(erc-speedbar-sort-users-type (quote alphabetical))
 '(erc-user-full-name "Tomás Solar")
 '(flymake-phpcs-location (quote tempdir))
 '(flymake-phpcs-standard "PSR2")
 '(flyspell-mode 1 t)
 '(git-state-modeline-decoration (quote git-state-decoration-small-dot))
 '(global-hl-line-mode t)
 '(global-hl-line-sticky-flag nil)
 '(global-linum-mode t)
 '(global-subword-mode t)
 '(haml-backspace-backdents-nesting nil)
 '(haml-indent-offset 4)
 '(identica-display-success-messages t)
 '(identica-soft-wrap-status t)
 '(ido-enable-flex-matching t)
 '(ido-mode 1 nil (ido))
 '(indent-tabs-mode t)
 '(inhibit-startup-screen t)
 '(ispell-extra-args (quote ("--sug-mode=fast")))
 '(ispell-list-command "--list")
 '(iswitchb-mode 1)
 '(jabber-show-offline-contacts nil)
 '(jabber-show-resources nil)
 '(js2-auto-indent-p t)
 '(js2-bounce-indent-p t)
 '(js2-cleanup-whitespace t)
 '(js2-enter-indents-newline t)
 '(less-css-compile-at-save t)
 '(rainbow-x-colors-major-mode-list (quote (emacs-lisp-mode lisp-interaction-mode c-mode c++-mode java-mode lua-mode html-helper-mode php-mode css-mode lisp-mode)))
 '(safe-local-variable-values (quote ((less-css-output-directory . "../css") (less-css-compile-at-save . t))))
 '(save-place t nil (saveplace))
 '(scroll-conservatively 1)
 '(send-mail-function (quote smtpmail-send-it))
 '(show-paren-mode t)
 '(show-paren-style (quote expression))
 '(size-indication-mode t)
 '(sml-modeline-mode t)
 '(smtpmail-smtp-server "mail.gnuchile.cl")
 '(smtpmail-smtp-service 25)
 '(tab-always-indent t)
 '(text-mode-hook (quote (text-mode-hook-identify)))
 '(tramp-chunksize 5000)
 '(tramp-default-host "localhost")
 '(tramp-default-method "ssh")
 '(transient-mark-mode 1)
 '(web-mode-enable-part-face nil)
 '(which-function-mode t))



;;; after declarating cua-mode
;; yanking
;; after copy Ctrl+c in X11 apps, you can paste by `yank' in emacs
(setq x-select-enable-clipboard t)

;; after mouse selection in X11, you can paste by `yank' in emacs
(setq x-select-enable-primary t)

(setq tramp-auto-save-directory "~/tmp/emacs-auto-save")

;;; faces
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(default ((t (:family "Inconsolata" :foundry "unknown" :slant normal :weight normal :height 113 :width normal))))
 '(hl-line ((t (:background "black"))))
 '(web-mode-block-face ((t nil)))
 '(web-mode-current-element-highlight-face ((t nil)))
 '(web-mode-inlay-face ((t nil)))
 '(web-mode-part-face ((t nil))))


;;;;;;;;;;;;;;; end general settings


;;;;;;;; Lets load the modes
;; init.el
(require 'package)
(package-initialize)

(load "lua-mode")
(load "php-mode")
(load "web-mode")
(load "flymake-easy")
(load "flymake-cursor")
(load "rainbow-mode")
(load "flycheck")

(require 'flymake-haml)
(require 'flymake-jslint)
(require 'flymake-json)
(require 'flymake-lua)
(require 'flymake-php)
(require 'flymake-phpcs)
(require 'flymake-shell)
(require 'flymake-jshint)
(require 'flymake-python-pyflakes)

;;;;;;;;;;;;; modes loaded


;;;;;;;;;;;; modes configs

;; ssh-config-mode
(autoload 'ssh-config-mode "ssh-config-mode" t)
(add-to-list 'auto-mode-alist '(".ssh/config\\'"  . ssh-config-mode))
(add-to-list 'auto-mode-alist '("sshd?_config\\'" . ssh-config-mode))
(add-hook 'ssh-config-mode-hook 'turn-on-font-lock)

;; nginx-mode
(add-to-list 'auto-mode-alist '("/etc/nginx/nginx.conf\\'" . nginx-mode))
(add-to-list 'auto-mode-alist '("/etc/nginx/sites-\\(enabled\\|available\\)/.*\\'" . nginx-mode))

;; varnish-mode
(add-to-list 'auto-mode-alist '("\\.vcl\\'" . vcl-mode))


;; Associate an engine
;; A specific engine can be forced with web-mode-engines-alist.
(setq web-mode-engines-alist
      '(("php"    . "\\.phtml\\'")
        ("blade"  . "\\.blade\\."))
)

(add-to-list 'auto-mode-alist '("\\.php\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.phtml\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.tpl\\.php\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.ctp\\.php\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.ctp\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.jsp\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.as[cp]x\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.erb\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.mustache\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.djhtml\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.html?\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.blade\\.php\\'" . web-mode))

(setq web-mode-markup-indent-offset 2)
(setq web-mode-css-indent-offset 4)
(setq web-mode-code-indent-offset 4)
(setq web-mode-indent-style 2)
(set-face-attribute 'web-mode-css-at-rule-face nil :foreground "Pink3")
(define-key web-mode-map (kbd "C-n") 'web-mode-tag-match)
(setq web-mode-disable-auto-pairing nil)
(setq web-mode-enable-block-face t)
(setq web-mode-enable-part-face t)
(setq web-mode-enable-comment-keywords t)
(setq web-mode-enable-heredoc-fontification t)
(setq web-mode-disable-css-colorization nil)
(defun web-mode-hook ()
  "Hooks for Web mode."
  (setq web-mode-markup-indent-offset 2)
  )
(add-hook 'web-mode-hook 'web-mode-hook)

(setq flymake-python-pyflakes-executable "flake8")

(setq flycheck-tip-avoid-show-func nil)


;; yaml
(add-to-list 'auto-mode-alist '("\\.yml$" . yaml-mode))

;; csv
(add-to-list 'auto-mode-alist '("\\.[Cc][Ss][Vv]\\'" . csv-mode))
(autoload 'csv-mode "csv-mode"
  "Major mode for editing comma-separated value files." t)

;;; hooks
;; rainbow-mode
(add-hook 'css-mode-hook 'rainbow-mode)
(add-hook 'php-mode-hook 'rainbow-mode)
(add-hook 'html-helper-mode-hook 'rainbow-mode)
(add-hook 'lua-mode-hook 'rainbow-mode)
(add-hook 'emacs-lisp-mode-hook 'rainbow-mode)

;; flymake
(add-hook 'haml-mode-hook 'flymake-haml-load)
(add-hook 'php-mode-hook 'flymake-php-load)
(add-hook 'php-mode-hook 'flymake-phpcs-load)
(add-hook 'lua-mode-hook 'flymake-lua-load)
(add-hook 'css-mode-hook 'flymake-css-load)
(add-hook 'js-mode-hook 'flymake-jslint-load)
(add-hook 'js2-mode-hook 'flymake-jslint-load)
(add-hook 'js3-mode-hook 'flymake-jslint-load)
(add-hook 'lua-mode-hook 'flymake-lua-load)
(add-hook 'yaml-mode-hook 'flymake-yaml-load)
(add-hook 'sh-set-shell-hook 'flymake-shell-load)
(add-hook 'js-mode-hook 'flymake-mode)
(add-hook 'js2-mode-hook 'flymake-mode)
(add-hook 'python-mode-hook 'flymake-python-pyflakes-load)

(add-hook 'yaml-mode-hook
    '(lambda ()
       (define-key yaml-mode-map "\C-m" 'newline-and-indent)))

;;;;;;;;;;;; end modes config


;;;;;;;;;; Flymake

;; Yes, I want my copies in the same dir as the original.
;; (setq flymake-run-in-place t)

;; Nope, I want my copies in the system temp dir.
(setq flymake-run-in-place nil)
;; This lets me say where my temp dir is.
;;(setq temporary-file-directory "~/.emacs.d/tmp/")
(setq temporary-file-directory "/tmp")

;; I want to see at most the first 4 errors for a line.
(setq flymake-number-of-errors-to-display 4)

;; I want to see all errors for the line.
(setq flymake-number-of-errors-to-display nil)
;;;;;;; end flymake
