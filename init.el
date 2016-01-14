(setq debug-on-error t)

(load "~/.emacs.d/lisp/my-packages.el")
(require 'my-packages)

;; exec-path-from-shell
(exec-path-from-shell-initialize)
(exec-path-from-shell-copy-env "PATH")
(exec-path-from-shell-copy-env "JAVA_HOME")

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


(global-set-key (kbd "S-M-<up>") 'move-text-up)
(global-set-key (kbd "S-M-<down>") 'move-text-down)

;; cuando cambio de buffer me quedo en el buffer nuevo!
(global-set-key "\C-x2" (lambda () (interactive)(split-window-vertically) (other-window 1)))
(global-set-key "\C-x3" (lambda () (interactive)(split-window-horizontally) (other-window 1)))

;; cycle through buffers
(global-set-key (kbd "<C-tab>") 'bury-buffer)

;; tea time
(define-key global-map "\C-ct" 'tea-time)

(global-set-key "\C-xk" 'kill-this-buffer) ; Kill buffer without confirmation

(defun duplicate-line-or-region (&optional n)
  "Duplicate current line, or region if active.
With argument N, make N copies.
With negative N, comment out original line and use the absolute value."
  (interactive "*p")
  (let ((use-region (use-region-p)))
    (save-excursion
      (let ((text (if use-region        ;Get region if active, otherwise line
                      (buffer-substring (region-beginning) (region-end))
                    (prog1 (thing-at-point 'line)
                      (end-of-line)
                      (if (< 0 (forward-line 1)) ;Go to beginning of next line, or make a new one
                          (newline))))))
        (dotimes (i (abs (or n 1)))     ;Insert N times, or once if not specified
          (insert text))))
    (if use-region nil                  ;Only if we're working with a line (not a region)
      (let ((pos (- (point) (line-beginning-position)))) ;Save column
        (if (> 0 n)                             ;Comment out original with negative arg
            (comment-region (line-beginning-position) (line-end-position)))
        (forward-line 1)
        (forward-char pos)))))

(global-set-key (kbd "C-c d") 'duplicate-line-or-region) ; duplicate line or region
(global-set-key "\C-c\C-d" "\C-a\C- \C-n\M-w\C-y\C-p") ; clone current line

(global-set-key (kbd "C-x g") 'magit-status)


(setq gtags-auto-update nil) ;; drupal fix/workaround?

;; full name in title
(setq frame-title-format
      (list (format "%s %%S: %%j " (system-name))
            '(buffer-file-name "%f" (dired-directory dired-directory "%b"))))

;; full path in modeline
;; (setq-default mode-line-buffer-identification
;;               (list 'buffer-file-name
;;                     (propertized-buffer-identification "%12f")
;;                     (propertized-buffer-identification "%12b")
;;                     ))

;;(setq sml/theme 'powerline)
(setq sml/no-confirm-load-theme t)
(sml/setup)

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
;; end helm


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
 '(ansi-color-faces-vector
   [default default default italic underline success warning error])
 '(ansi-color-names-vector
   ["#212526" "#ff4b4b" "#b4fa70" "#fce94f" "#729fcf" "#ad7fa8" "#8cc4ff" "#eeeeec"])
 '(blink-cursor-mode nil)
 '(c-tab-always-indent nil)
 '(column-number-mode t)
 '(company-auto-complete (quote (quote company-explicit-action-p)))
 '(compilation-message-face (quote default))
 '(cua-global-mark-cursor-color "#2aa198")
 '(cua-mode t nil (cua-base))
 '(cua-normal-cursor-color "#657b83")
 '(cua-overwrite-cursor-color "#b58900")
 '(cua-read-only-cursor-color "#859900")
 '(custom-enabled-themes (quote (railscasts)))
 '(custom-safe-themes
   (quote
    ("79a3f477ac0cb4a106f78b6109614e991564a5c2467c36e6e854d4bc1102e178" "3b0a350918ee819dca209cec62d867678d7dac74f6195f5e3799aa206358a983" "a8245b7cc985a0610d71f9852e9f2767ad1b852c2bdea6f4aadc12cce9c4d6d0" "d677ef584c6dfc0697901a44b885cc18e206f05114c8a3b7fde674fce6180879" "8aebf25556399b58091e533e455dd50a6a9cba958cc4ebb0aab175863c25b9a4" "98a619757483dc6614c266107ab6b19d315f93267e535ec89b7af3d62fb83cad" "405b0ac2ac4667c5dab77b36e3dd87a603ea4717914e30fcf334983f79cfd87e" default)))
 '(delete-selection-mode t)
 '(display-battery-mode t)
 '(display-time-mode t)
 '(ecb-options-version "2.40")
 '(erc-auto-query (quote window-noselect))
 '(erc-autoaway-mode t)
 '(erc-away-nickname nil)
 '(erc-join-buffer (quote window-noselect))
 '(erc-modules
   (quote
    (completion list menu scrolltobottom autojoin button dcc fill irccontrols match move-to-prompt netsplit networks noncommands readonly ring stamp spelling track)))
 '(erc-nick-notify-mode t)
 '(erc-prompt ">")
 '(erc-public-away-p t)
 '(erc-speedbar-sort-users-type (quote alphabetical))
 '(erc-user-full-name "Tomás Solar")
 '(fci-rule-color "#eee8d5")
 '(flymake-phpcs-location (quote tempdir))
 '(flymake-phpcs-standard "PSR2")
 '(flyspell-mode 1 t)
 '(git-state-modeline-decoration (quote git-state-decoration-small-dot))
 '(global-diff-hl-mode t)
 '(global-subword-mode t)
 '(gtags-auto-update nil t)
 '(haml-backspace-backdents-nesting nil)
 '(haml-indent-offset 4)
 '(helm-autoresize-mode t)
 '(helm-mode-fuzzy-match t)
 '(highlight-changes-colors (quote ("#d33682" "#6c71c4")))
 '(highlight-symbol-foreground-color "#586e75")
 '(highlight-tail-colors
   (quote
    (("#eee8d5" . 0)
     ("#B4C342" . 20)
     ("#69CABF" . 30)
     ("#69B7F0" . 50)
     ("#DEB542" . 60)
     ("#F2804F" . 70)
     ("#F771AC" . 85)
     ("#eee8d5" . 100))))
 '(hl-bg-colors
   (quote
    ("#DEB542" "#F2804F" "#FF6E64" "#F771AC" "#9EA0E5" "#69B7F0" "#69CABF" "#B4C342")))
 '(hl-fg-colors
   (quote
    ("#fdf6e3" "#fdf6e3" "#fdf6e3" "#fdf6e3" "#fdf6e3" "#fdf6e3" "#fdf6e3" "#fdf6e3")))
 '(identica-display-success-messages t)
 '(identica-soft-wrap-status t)
 '(ido-enable-flex-matching t)
 '(indent-tabs-mode nil)
 '(inhibit-startup-screen t)
 '(ispell-extra-args (quote ("--sug-mode=fast")))
 '(ispell-list-command "--list")
 '(jabber-show-offline-contacts nil)
 '(jabber-show-resources nil)
 '(js2-auto-indent-p t)
 '(js2-bounce-indent-p t)
 '(js2-cleanup-whitespace t)
 '(js2-enter-indents-newline t)
 '(less-css-compile-at-save t)
 '(magit-auto-revert-mode nil)
 '(magit-diff-use-overlays nil)
 '(magit-last-seen-setup-instructions "1.4.0")
 '(nrepl-message-colors
   (quote
    ("#dc322f" "#cb4b16" "#b58900" "#546E00" "#B4C342" "#00629D" "#2aa198" "#d33682" "#6c71c4")))
 '(password-cache-expiry nil)
 '(php-lineup-cascaded-calls t)
 '(php-mode-coding-style (quote pear))
 '(php-template-compatibility nil)
 '(pos-tip-background-color "#eee8d5")
 '(pos-tip-foreground-color "#586e75")
 '(projectile-require-project-root nil)
 '(rainbow-x-colors-major-mode-list
   (quote
    (emacs-lisp-mode lisp-interaction-mode c-mode c++-mode java-mode lua-mode html-helper-mode php-mode css-mode lisp-mode)))
 '(recentf-mode t)
 '(safe-local-variable-values
   (quote
    ((encoding . utf-8)
     (less-css-output-directory . "../css")
     (less-css-compile-at-save . t))))
 '(save-place t nil (saveplace))
 '(scroll-conservatively 1)
 '(send-mail-function (quote smtpmail-send-it))
 '(show-paren-mode t)
 '(show-paren-style (quote mixed))
 '(show-trailing-whitespace t)
 '(size-indication-mode t)
 '(smartrep-mode-line-active-bg (solarized-color-blend "#859900" "#eee8d5" 0.2))
 '(sml-modeline-mode t)
 '(sml/mode-width
   (if
       (eq powerline-default-separator
           (quote arrow))
       (quote right)
     (quote full)))
 '(sml/pos-id-separator
   (quote
    (""
     (:propertize " " face powerline-active1)
     (:eval
      (propertize " "
                  (quote display)
                  (funcall
                   (intern
                    (format "powerline-%s-%s" powerline-default-separator
                            (car powerline-default-separator-dir)))
                   (quote powerline-active1)
                   (quote powerline-active2))))
     (:propertize " " face powerline-active2))))
 '(sml/pos-minor-modes-separator
   (quote
    (""
     (:propertize " " face powerline-active1)
     (:eval
      (propertize " "
                  (quote display)
                  (funcall
                   (intern
                    (format "powerline-%s-%s" powerline-default-separator
                            (cdr powerline-default-separator-dir)))
                   (quote powerline-active1)
                   nil)))
     (:propertize " " face sml/global))))
 '(sml/pre-id-separator
   (quote
    (""
     (:propertize " " face sml/global)
     (:eval
      (propertize " "
                  (quote display)
                  (funcall
                   (intern
                    (format "powerline-%s-%s" powerline-default-separator
                            (car powerline-default-separator-dir)))
                   nil
                   (quote powerline-active1))))
     (:propertize " " face powerline-active1))))
 '(sml/pre-minor-modes-separator
   (quote
    (""
     (:propertize " " face powerline-active2)
     (:eval
      (propertize " "
                  (quote display)
                  (funcall
                   (intern
                    (format "powerline-%s-%s" powerline-default-separator
                            (cdr powerline-default-separator-dir)))
                   (quote powerline-active2)
                   (quote powerline-active1))))
     (:propertize " " face powerline-active1))))
 '(sml/pre-modes-separator (propertize " " (quote face) (quote sml/modes)))
 '(sml/replacer-regexp-list
   (quote
    (("^~/org/" ":Org:")
     ("^~/\\.emacs\\.d/" ":ED:")
     ("^/sudo:.*:" ":SU:")
     ("^~/Documents/" ":Doc:")
     ("^~/Dropbox/" ":DB:")
     ("^:\\([^:]*\\):Documento?s/" ":\\1/Doc:")
     ("^~/[Gg]it/" ":Git:")
     ("^~/[Gg]it[Hh]ub/" ":Git:")
     ("^~/[Gg]it\\([Hh]ub\\|\\)-?[Pp]rojects/" ":Git:"))))
 '(sml/shorten-directory nil)
 '(smtpmail-smtp-server "mail.gnuchile.cl")
 '(smtpmail-smtp-service 25)
 '(tab-always-indent t)
 '(tabbar-separator (quote (0.5)))
 '(term-default-bg-color "#fdf6e3")
 '(term-default-fg-color "#657b83")
 '(text-mode-hook (quote (text-mode-hook-identify)))
 '(tramp-chunksize 5000)
 '(tramp-default-host "localhost")
 '(tramp-default-method "ssh")
 '(transient-mark-mode 1)
 '(uniquify-buffer-name-style (quote forward) nil (uniquify))
 '(vc-annotate-background nil)
 '(vc-annotate-color-map
   (quote
    ((20 . "#dc322f")
     (40 . "#c85d17")
     (60 . "#be730b")
     (80 . "#b58900")
     (100 . "#a58e00")
     (120 . "#9d9100")
     (140 . "#959300")
     (160 . "#8d9600")
     (180 . "#859900")
     (200 . "#669b32")
     (220 . "#579d4c")
     (240 . "#489e65")
     (260 . "#399f7e")
     (280 . "#2aa198")
     (300 . "#2898af")
     (320 . "#2793ba")
     (340 . "#268fc6")
     (360 . "#268bd2"))))
 '(vc-annotate-very-old-color nil)
 '(web-mode-enable-current-column-highlight t)
 '(web-mode-enable-part-face nil)
 '(weechat-color-list
   (quote
    (unspecified "#fdf6e3" "#eee8d5" "#990A1B" "#dc322f" "#546E00" "#859900" "#7B6000" "#b58900" "#00629D" "#268bd2" "#93115C" "#d33682" "#00736F" "#2aa198" "#657b83" "#839496")))
 '(which-function-mode t)
 '(xterm-color-names
   ["#eee8d5" "#dc322f" "#859900" "#b58900" "#268bd2" "#d33682" "#2aa198" "#073642"])
 '(xterm-color-names-bright
   ["#fdf6e3" "#cb4b16" "#93a1a1" "#839496" "#657b83" "#6c71c4" "#586e75" "#002b36"]))



;;; after declarating cua-mode
;; yanking
;; after copy Ctrl+c in X11 apps, you can paste by `yank' in emacs
(setq x-select-enable-clipboard t)

;; after mouse selection in X11, you can paste by `yank' in emacs
(setq x-select-enable-primary t)

(setq tramp-auto-save-directory "~/tmp/emacs-auto-save")

;; disable linum
;(setq linum-disabled-modes-list '(eshell-mode wl-summary-mode compilation-mode emms)) (defun linum-on () (unless (or (minibufferp) (member major-mode linum-disabled-modes-list)) (linum-mode 1)))

(add-hook 'prog-mode-hook 'linum-mode)

;;; faces
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(default ((t (:family "Inconsolata" :foundry "unknown" :slant normal :weight normal :height 113 :width normal))))
 '(iedit-occurrence ((t (:background "gold" :foreground "black"))))
 '(show-paren-match ((t (:background nil :foreground "gold" :bold t))))
 '(web-mode-block-face ((t nil)))
 '(web-mode-inlay-face ((t nil)))
 '(web-mode-part-face ((t nil))))

;;;;;;;;;;;;;;; end general settings

;;;;;;;; Lets load the modes
;; init.el
(require 'package)
;(package-initialize)

(load "lua-mode")
;;(load "php-mode")
(load "web-mode")
(load "flymake-easy")
(load "flymake-cursor")
(load "rainbow-mode")
(load "flycheck")
(load "tea-time")
(load "iedit")

(require 'popwin)
(popwin-mode 1)
(setq display-buffer-function 'popwin:display-buffer)
(push '("\*[Hh]elm" :regexp t) popwin:special-display-config)


;;(rainbow-identifiers-mode 1)

;;(require 'ido)
;;(ido-mode t)

;; undo-tree http://www.dr-qubit.org/git/undo-tree.git
(global-undo-tree-mode)

(bash-completion-setup)

(require 'flx-ido)
(ido-mode 1)
(ido-everywhere 1)
(flx-ido-mode 1)
;; disable ido faces to see flx highlights.
(setq ido-enable-flex-matching t)
(setq ido-use-faces nil)

(projectile-global-mode)
(setq projectile-completion-system 'helm)
(helm-projectile-on)

;; Load rvm.el
(require 'rvm)
;; use rvm’s default ruby for the current Emacs session
(rvm-use-default)

;; rails rinari-mode
;;(require 'rinari)
;;(load "rinari")
;;(global-rinari-mode)

;;(setq rinari-tags-file-name "TAGS")


;; (global-git-gutter+-mode)
;; (require 'git-gutter-fringe+)

(require 'ecb)

(require 'neotree)
(global-set-key [f8] 'neotree-toggle)


(require 'auto-complete)
(add-to-list 'ac-dictionary-directories "~/.emacs.d/ac-dict")
(require 'auto-complete-config)
(ac-config-default)

(require 'php-auto-yasnippets)
;; (load "php-auto-yasnippets")
(define-key php-mode-map (kbd "C-c C-y") 'yas/create-php-snippet)
(payas/ac-setup)

(require 'emmet-mode)
(require 'ac-emmet)
(add-hook 'sgml-mode-hook 'ac-emmet-html-setup)
(add-hook 'css-mode-hook 'ac-emmet-css-setup)

(require 'flymake-haml)
(require 'flymake-jslint)
(require 'flymake-json)
(require 'flymake-lua)
(require 'flymake-php)
(require 'flymake-phpcs)
(require 'flymake-shell)
(require 'flymake-jshint)
(require 'flymake-python-pyflakes)
(require 'notifications)

(require 'hl-anything)
;;(require 'hl-paren-mode)

(require 'easy-repeat)

(require 'company)                                   ; load company mode
(require 'company-web-html)                          ; load company mode html backend
;; you may key bind, for example for web-mode:
(define-key web-mode-map (kbd "C-'") 'company-web-html)

(require 'helm-config)

(helm-mode 1)

;; HELM FUZZY!!!!
(require 'helm-fuzzier)
(helm-fuzzier-mode 1)

(require 'helm-ls-git)


;;;;;;;;;;;;; modes loaded


;;;;;;;;;;;; modes configs

;; ag!
(setq ag-highlight-search t)
(setq ag-reuse-window t)

;; highlight symbol
(require 'highlight-symbol)
(global-set-key [(control f3)] 'highlight-symbol)
(global-set-key [f3] 'highlight-symbol-next)
(global-set-key [(shift f3)] 'highlight-symbol-prev)
(global-set-key [(meta f3)] 'highlight-symbol-query-replace)

;; stop annoying when autocompleting
(ac-linum-workaround)


;; company-mode
(setq company-tooltip-limit 20)                      ; bigger popup window
(setq company-tooltip-align-annotations 't)          ; align annotations to the right tooltip border
(setq company-idle-delay .3)                         ; decrease delay before autocompletion popup shows
(setq company-begin-commands '(self-insert-command)) ; start autocompletion only after typing
;(global-set-key (kbd "C-c /") 'company-files)        ; Force complete file names on "C-c /" key

;; helm company
(eval-after-load 'company
  '(progn
     (define-key company-mode-map (kbd "C-:") 'helm-company)
     (define-key company-active-map (kbd "C-:") 'helm-company)))

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

;; markdown-mode
(add-to-list 'auto-mode-alist '("\\.text\\'" . markdown-mode))
(add-to-list 'auto-mode-alist '("\\.markdown\\'" . markdown-mode))
(add-to-list 'auto-mode-alist '("\\.md\\'" . markdown-mode))
(add-to-list 'auto-mode-alist '("README\\.md\\'" . gfm-mode))

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

(setq web-mode-markup-indent-offset 4)
(setq web-mode-css-indent-offset 4)
(setq web-mode-code-indent-offset 4)
(setq web-mode-indent-style 4)
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
  (setq web-mode-markup-indent-offset 4)
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


(setq flymake-python-pyflakes-executable "flake8")

(setq flycheck-tip-avoid-show-func nil)

;; web-beautify
(eval-after-load 'js2-mode
  '(define-key js2-mode-map (kbd "C-c b") 'web-beautify-js))
(eval-after-load 'js3-mode
  '(define-key js3-mode-map (kbd "C-c b") 'web-beautify-js))
;; Or if you're using 'js-mode' (a.k.a 'javascript-mode')
(eval-after-load 'js
  '(define-key js-mode-map (kbd "C-c b") 'web-beautify-js))

(eval-after-load 'json-mode
  '(define-key json-mode-map (kbd "C-c b") 'web-beautify-js))

(eval-after-load 'sgml-mode
  '(define-key html-mode-map (kbd "C-c b") 'web-beautify-html))

(eval-after-load 'css-mode
  '(define-key css-mode-map (kbd "C-c b") 'web-beautify-css))


;; yaml
(add-to-list 'auto-mode-alist '("\\.yml$" . yaml-mode))

;; csv
(add-to-list 'auto-mode-alist '("\\.[Cc][Ss][Vv]\\'" . csv-mode))
(autoload 'csv-mode "csv-mode"
  "Major mode for editing comma-separated value files." t)

;;; hooks
;; company-mode
;; (add-hook 'after-init-hook 'global-company-mode) ;; don't wanna load everywhere
(add-hook 'web-mode-hook (lambda ()
                          (set (make-local-variable 'company-backends) '(company-web-html))
                          (company-mode t)))
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

(add-hook 'projectile-mode-hook 'projectile-rails-on)

(add-hook 'yaml-mode-hook
	  '(lambda ()
	     (define-key yaml-mode-map "\C-m" 'newline-and-indent)))

(add-hook 'tea-time-notification-hook
	  (lambda ()
	    (notifications-notify :title "Time is up!"
				  :body "I know you're busy, but it's TEA TIME!!"
				  :app-name "Tea Time"
				  :sound-name "alarm-clock-elapsed")))

(add-hook 'php-mode-hook (lambda () (setq comment-start "// "
					  comment-end ""
					  comment-style 'indent
					  comment-use-syntax t
					  )))

;; indent region after exit yasnippet
(add-hook 'yas/after-exit-snippet-hook
	  '(lambda ()
	     (indent-region yas/snippet-beg yas/snippet-end)))

;; I am using yasnippet in PHP mode, so, load minor mode!
(add-hook 'php-mode-hook 'yas-minor-mode)

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

;; git-gutter+
(eval-after-load 'git-gutter+
  '(progn
     ;;; Jump between hunks
     (define-key git-gutter+-mode-map (kbd "C-x n") 'git-gutter+-next-hunk)
     (define-key git-gutter+-mode-map (kbd "C-x p") 'git-gutter+-previous-hunk)

     ;;; Act on hunks
     (define-key git-gutter+-mode-map (kbd "C-x v =") 'git-gutter+-show-hunk)
     (define-key git-gutter+-mode-map (kbd "C-x r") 'git-gutter+-revert-hunks)

     ;; Stage hunk at point.
     ;; If region is active, stage all hunk lines within the region.
     (define-key git-gutter+-mode-map (kbd "C-x t") 'git-gutter+-stage-hunks)
     (define-key git-gutter+-mode-map (kbd "C-x c") 'git-gutter+-commit)
     (define-key git-gutter+-mode-map (kbd "C-x C") 'git-gutter+-stage-and-commit)
     (define-key git-gutter+-mode-map (kbd "C-x C-y") 'git-gutter+-stage-and-commit-whole-buffer)
     (define-key git-gutter+-mode-map (kbd "C-x U") 'git-gutter+-unstage-whole-buffer)))
;; end git-gutter+


;; ruby indent accessors
(defadvice ruby-indent-line (around outdent-modifiers activate)
  (if (save-excursion
        (beginning-of-line)
        (looking-at "\s*\\(private\\|protected\\|public\\)\s*$"))
      (save-excursion
        (beginning-of-line)
        (just-one-space 0))
      ad-do-it))
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

;; paren-mode-helper
(defadvice show-paren-function
    (after show-matching-paren-offscreen activate)
  "If the matching paren is offscreen, show the matching line in the
        echo area. Has no effect if the character before point is not of
        the syntax class ')'."
  (interactive)
  (let* ((cb (char-before (point)))
  	 (matching-text (and cb
  			     (char-equal (char-syntax cb) ?\) )
  			     (blink-matching-open))))
    (when matching-text (message matching-text))))

;;;; the following makes a jump to code, I do not like that :/
;; (defadvice show-paren-function (after my-echo-paren-matching-line activate)
;;   "If a matching paren is off-screen, echo the matching line."
;;   (when (char-equal (char-syntax (char-before (point))) ?\))
;;     (let ((matching-text (blink-matching-open)))
;;       (when matching-text
;;         (message matching-text)))))
;; end paren mode helper

;;; comment or uncomment line or region
(defun comment-or-uncomment-current-line-or-region ()
  "Comments or uncomments current current line or whole lines in region."
  (interactive)
  (save-excursion
    (let (min max)
      (if (region-active-p)
          (setq min (region-beginning) max (region-end))
        (setq min (point) max (point)))
      (comment-or-uncomment-region
       (progn (goto-char min) (line-beginning-position))
       (progn (goto-char max) (line-end-position))))))
(global-set-key (kbd "C-7") 'comment-or-uncomment-current-line-or-region)
;;; end comment or uncoment...




;(load-file (expand-file-name "custom/tabbar.el" user-emacs-directory))

(require 'my-emms)
(require 'my-smartparents)
;(require 'my-tabbar)
(require 'my-highlight-parentheses)

(nyan-mode 1)
(sml/apply-theme 'powerline)
