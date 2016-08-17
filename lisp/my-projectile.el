;;(projectile-global-mode)
;; workaround for tramp: (see https://github.com/bbatsov/prelude/issues/594)
(add-hook 'text-mode-hook #'projectile-mode)
(add-hook 'prog-mode-hook #'projectile-mode)
(add-hook 'magit-mode-hook #'projectile-mode)
(add-hook 'css-mode-hook #'projectile-mode)
(add-hook 'yaml-mode-hook #'projectile-mode)
(add-hook 'gitignore-mode-hook #'projectile-mode)


(setq projectile-completion-system 'helm)
(helm-projectile-on)

(add-hook 'projectile-mode-hook 'projectile-rails-on)

(setq projectile-find-dir-includes-top-level t)

(provide 'my-projectile)
