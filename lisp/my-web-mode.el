(require 'web-mode)

;; Associate an engine
;; A specific engine can be forced with web-mode-engines-alist.
;;;
(setq web-mode-engines-alist
      '(("php"    . "\\.phtml\\'")
        ("blade"  . "\\.blade\\.")
	("php"  . "\\.php\\.")
        ("jsx"  . "\\.js[x]?\\'")
	)
)


(add-to-list 'auto-mode-alist '("\\.php\\'" . php-mode))
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
(add-to-list 'auto-mode-alist '("\\.js[x]?\\'" . web-mode))
;; (add-to-list 'auto-mode-alist '("\\.js[x]?\\'" . js3-mode))

(setq web-mode-markup-indent-offset 2)
(setq web-mode-css-indent-offset 2)
(setq web-mode-code-indent-offset 2)
(setq web-mode-indent-style 2)
(set-face-attribute 'web-mode-css-at-rule-face nil :foreground "Pink3")
;;(define-key web-mode-map (kbd "C-n") 'web-mode-tag-match)
(setq web-mode-disable-auto-pairing nil)
(setq web-mode-enable-block-face t)
(setq web-mode-enable-part-face t)
(setq web-mode-enable-comment-keywords t)
(setq web-mode-enable-heredoc-fontification t)
(setq web-mode-disable-css-colorization nil)
(setq web-mode-enable-current-element-highlight t)
(defun web-mode-hook ()
  "Hooks for Web mode."
  (setq web-mode-markup-indent-offset 2)
  )
(add-hook 'web-mode-hook 'web-mode-hook)

(setq web-mode-ac-sources-alist
      '(
	("php" . (ac-source-yasnippet ac-source-php-auto-yasnippets))
	("html" . (ac-source-emmet-html-aliases ac-source-emmet-html-snippets))
	("css" . (ac-source-css-property ac-source-emmet-css-snippets))))

(add-hook 'web-mode-before-auto-complete-hooks
          '(lambda ()
             (let ((web-mode-cur-language
                    (web-mode-language-at-pos)))
               (if (string= web-mode-cur-language "php")
                   (yas-activate-extra-mode 'php-mode)
                 (yas-deactivate-extra-mode 'php-mode))
               (if (string= web-mode-cur-language "css")
                   (setq emmet-use-css-transform t)
                 (setq emmet-use-css-transform nil)))))

(defun js-mode-hook ()
  "Hooks for Javascript mode."
  ;(setq tab-width 2)
  (setq js-indent-level 2)
  )
(add-hook 'js-mode-hook js-mode-hook)
(add-hook 'js3-mode-hook js-mode-hook)
(add-hook 'web-mode-hook js-mode-hook)

;; JSX stuff
(defadvice web-mode-highlight-part (around tweak-jsx activate)
  (if (equal web-mode-content-type "jsx")
      (let ((web-mode-enable-part-face nil))
        ad-do-it)
    ad-do-it))

;; from https://truongtx.me/2014/03/10/emacs-setup-jsx-mode-and-jsx-syntax-checking
;; make sure jsxhint is installed: npm install -g jsxhint
(flycheck-define-checker jsxhint-checker
  "A JSX syntax and style checker based on JSXHint."

  :command ("jsxhint" source)
  :error-patterns
  ((error line-start (1+ nonl) ": line " line ", col " column ", " (message) line-end))
  :modes (web-mode))
(add-hook 'web-mode-hook
          (lambda ()
            (when (equal web-mode-content-type "jsx")
              ;; enable flycheck
              (flycheck-select-checker 'jsxhint-checker)
              (flycheck-mode))))

(provide 'my-web-mode)
