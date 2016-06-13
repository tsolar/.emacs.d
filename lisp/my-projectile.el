;;(projectile-global-mode)
;; workaround for tramp: (see https://github.com/bbatsov/prelude/issues/594)
(add-hook 'text-mode-hook #'projectile-mode)
(add-hook 'prog-mode-hook #'projectile-mode)

(setq projectile-completion-system 'helm)
(helm-projectile-on)

(add-hook 'projectile-mode-hook 'projectile-rails-on)

(provide 'my-projectile)
