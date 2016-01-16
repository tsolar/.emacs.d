(require 'company)                                   ; load company mode
(require 'company-web-html)                          ; load company mode html backend

;; you may key bind, for example for web-mode:
(define-key web-mode-map (kbd "C-'") 'company-web-html)

;; company-mode
(setq company-tooltip-limit 20)                      ; bigger popup window
(setq company-tooltip-align-annotations 't)          ; align annotations to the right tooltip border
(setq company-idle-delay .3)                         ; decrease delay before autocompletion popup shows
(setq company-begin-commands '(self-insert-command)) ; start autocompletion only after typing
(global-set-key (kbd "C-c /") 'company-files)        ; Force complete file names on "C-c /" key

;; helm company
(eval-after-load 'company
  '(progn
     (define-key company-mode-map (kbd "C-:") 'helm-company)
     (define-key company-active-map (kbd "C-:") 'helm-company)))


;; company-mode hooks
;; (add-hook 'after-init-hook 'global-company-mode) ;; don't wanna load everywhere
(add-hook 'web-mode-hook (lambda ()
                          (set (make-local-variable 'company-backends) '(company-web-html))
                          (company-mode t)))

(provide 'my-company)
