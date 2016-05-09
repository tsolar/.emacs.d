(setq sml/no-confirm-load-theme t)

(display-time-mode)
(setq powerline-arrow-shape 'curve)
(setq powerline-default-separator-dir '(right . left))
(setq sml/theme 'powerline)
(setq sml/mode-width 0)
(setq sml/name-width 20)
(rich-minority-mode 1)
(setf rm-blacklist "")
(sml/setup)

(sml/apply-theme 'dark)

(provide 'my-sml)
