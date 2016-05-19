;; (add-hook 'ruby-mode-hook 'hs-minor-mode)
;; (add-hook 'ruby-mode-hook (lambda () (hs-minor-mode)))

(add-hook 'prog-mode-hook 'hs-minor-mode)

(eval-after-load "hideshow"
  '(add-to-list 'hs-special-modes-alist
                `(ruby-mode
                  ,(rx (or "def" "class" "module" "do" "if" "{" "[")) ; Block start
                  ,(rx (or "}" "]" "end"))                       ; Block end
                  ,(rx (or "#" "=begin"))                        ; Comment start
                  ruby-forward-sexp nil)))

(global-set-key (kbd "C-c h") 'hs-hide-block)
(global-set-key (kbd "C-c s") 'hs-show-block)

(provide 'my-hideshow)
