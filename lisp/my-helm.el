(require 'helm-config)
(helm-mode 1)

;; HELM FUZZY!!!!
(require 'helm-fuzzier)
(helm-fuzzier-mode 1)

(require 'helm-ls-git)

;; helm keybindings
(global-set-key (kbd "M-x") 'helm-M-x)
(setq helm-M-x-fuzzy-match t) ;; optional fuzzy matching for helm-M-x

(global-set-key (kbd "M-y") 'helm-show-kill-ring)

(global-set-key (kbd "C-x b") 'helm-mini)
(setq helm-buffers-fuzzy-matching t
      helm-recentf-fuzzy-match    t)

(global-set-key (kbd "C-x C-f") 'helm-find-files)

(when (executable-find "ack-grep")
  (setq helm-grep-default-command "ack-grep -Hn --no-group --no-color %e %p %f"
        helm-grep-default-recurse-command "ack-grep -H --no-group --no-color %e %p %f"))

(setq helm-semantic-fuzzy-match t
      helm-imenu-fuzzy-match    t)

(setq helm-locate-fuzzy-match t)

(setq helm-apropos-fuzzy-match t)

(setq helm-lisp-fuzzy-completion t)

(global-set-key (kbd "C-h SPC") 'helm-all-mark-rings)

(require 'helm-eshell)
(add-hook 'eshell-mode-hook
          #'(lambda ()
              (define-key eshell-mode-map (kbd "C-c C-l")  'helm-eshell-history)))

(provide 'my-helm)
