(load "flymake-easy")
(load "flymake-cursor")

(require 'flymake-haml)
(require 'flymake-jslint)
(require 'flymake-json)
(require 'flymake-lua)
(require 'flymake-php)
(require 'flymake-phpcs)
(require 'flymake-shell)
(require 'flymake-jshint)
(require 'flymake-python-pyflakes)

(setq flymake-python-pyflakes-executable "flake8")

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

;;;; Disable flymake during expansion
(defvar flymake-is-active-flag nil)

(defadvice yas/expand-snippet
  (before inhibit-flymake-syntax-checking-while-expanding-snippet activate)
  (setq flymake-is-active-flag
        (or flymake-is-active-flag
            (assoc-default 'flymake-mode (buffer-local-variables))))
  (when flymake-is-active-flag
    (flymake-mode-off)))

(add-hook 'yas/after-exit-snippet-hook
          '(lambda ()
             (when flymake-is-active-flag
               (flymake-mode-on)
               (setq flymake-is-active-flag nil))))
;;; end disable flymake during expansion

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

(provide 'my-flymake)
