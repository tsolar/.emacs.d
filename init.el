(setq debug-on-error t)

(setq package-archives '(("gnu" . "http://elpa.gnu.org/packages/")
			 ("elpa" . "http://tromey.com/elpa/")
			 ("melpa" . "http://melpa.milkbox.net/packages/")
			 ("marmalade" . "http://marmalade-repo.org/packages/")
			 ))
;; list the packages you want
;; with C-h v package-activated-list
(setq package-list '(ac-emmet auto-complete popup emmet-mode auto-complete popup color-theme-github color-theme csv-mode diff-hl drupal-mode php-mode easy-repeat elscreen emmet-mode emms-info-mediainfo emms f dash s flycheck-pyflakes flycheck let-alist pkg-info epl dash flycheck-tip s popup dash flycheck let-alist pkg-info epl dash flylisp flymake-css flymake-easy flymake-cursor flymake-gjshint flymake-haml flymake-easy flymake-hlint flymake-easy flymake-jshint flymake-easy flymake-jslint flymake-easy flymake-json flymake-easy flymake-less less-css-mode flymake-lua flymake-php flymake-easy flymake-phpcs flymake-easy flymake-python-pyflakes flymake-easy flymake-ruby flymake-easy flymake-sass flymake-easy flymake-shell flymake-easy flymake-yaml flymake-easy github-theme gitignore-mode hl-anything js3-mode json-mode json-snatcher json-reformat json-reformat json-snatcher less-css-mode let-alist lua-mode magit git-rebase-mode git-commit-mode markdown-mode minimap move-text neotree nginx-mode php-auto-yasnippets yasnippet php-mode php-mode pkg-info epl popup rainbow-mode s sass-mode haml-mode scss-mode smartparens dash ssh-config-mode syslog-mode hide-lines tea-time twittering-mode vcl-mode web-beautify web-mode yaml-mode yasnippet iedit))

(add-to-list 'load-path "~/.emacs.d/lisp")
(add-to-list 'load-path "~/.emacs.d/elpa")

; activate all the packages (in particular autoloads)
(package-initialize)

; fetch the list of packages available
(unless package-archive-contents
  (package-refresh-contents))

; install the missing packages
(dolist (package package-list)
  (unless (package-installed-p package)
    (package-install package)))



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

;; cycle through buffers
(global-set-key (kbd "<C-tab>") 'bury-buffer)

;; tea time
(define-key global-map "\C-ct" 'tea-time)

(global-set-key "\C-xk" 'kill-this-buffer) ; Kill buffer without confirmation
(global-set-key "\C-c\C-d" "\C-a\C- \C-n\M-w\C-y\C-p") ; clone current line

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
 '(ansi-color-names-vector
   ["#212526" "#ff4b4b" "#b4fa70" "#fce94f" "#729fcf" "#ad7fa8" "#8cc4ff" "#eeeeec"])
 '(blink-cursor-mode nil)
 '(c-tab-always-indent nil)
 '(column-number-mode t)
 '(cua-mode t nil (cua-base))
 '(custom-enabled-themes (quote (deeper-blue)))
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
 '(flymake-phpcs-location (quote tempdir))
 '(flymake-phpcs-standard "PSR2")
 '(flyspell-mode 1 t)
 '(git-state-modeline-decoration (quote git-state-decoration-small-dot))
 '(global-diff-hl-mode t)
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
 '(jabber-show-offline-contacts nil)
 '(jabber-show-resources nil)
 '(js2-auto-indent-p t)
 '(js2-bounce-indent-p t)
 '(js2-cleanup-whitespace t)
 '(js2-enter-indents-newline t)
 '(less-css-compile-at-save t)
 '(magit-auto-revert-mode nil)
 '(password-cache-expiry nil)
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

;; disable linum
(setq linum-disabled-modes-list '(eshell-mode wl-summary-mode compilation-mode emms)) (defun linum-on () (unless (or (minibufferp) (member major-mode linum-disabled-modes-list)) (linum-mode 1)))

;;; faces
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(default ((t (:family "Inconsolata" :foundry "unknown" :slant normal :weight normal :height 113 :width normal))))
 '(cursor ((t (:background "green"))))
 '(iedit-occurrence ((t (:background "yellow" :foreground "black"))))
 '(web-mode-block-face ((t nil)))
 '(web-mode-current-element-highlight-face ((t nil)))
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

(require 'ecb)

(require 'neotree)
(global-set-key [f8] 'neotree-toggle)

(require 'smartparens-config)
(sp-local-pair 'web-mode "{" "}" :actions nil)
(sp-local-pair 'web-mode "<" ">" :actions nil)

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
      '(("php" . (ac-source-yasnippet ac-source-php-auto-yasnippets))
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




;;; EMMS

(require 'emms-setup)
(emms-standard)
(emms-default-players)

(require 'emms-source-file)
(require 'emms-source-playlist)
(require 'emms-info)
(require 'emms-cache)
(require 'emms-playlist-mode)
;; (require 'emms-mode-line)
(require 'emms-playing-time)
(require 'emms-player-mpd)
(require 'emms-playlist-sort)
(require 'emms-mark)
(require 'emms-browser)
(require 'emms-lyrics)
(require 'emms-last-played)
(require 'emms-score)
;;(require 'emms-lastfm)

(setq emms-playlist-default-major-mode 'emms-playlist-mode)

(add-to-list 'emms-track-initialize-functions 'emms-info-initialize-track)
;; (emms-mode-line 1)
;; (emms-mode-line-blank)
(emms-playing-time 1)
(emms-lyrics 1)
(add-hook 'emms-player-started-hook 'emms-last-played-update-current)
(emms-score 1)
(when (fboundp 'emms-cache)           ; work around compiler warning
  (emms-cache 1))
(setq emms-score-default-score 3)

(require 'emms-player-mpd)
;;Set the variables emms-player-mpd-server-name and emms-player-mpd-server-port to the location and port (respectively) of your MusicPD server. For example:
(setq emms-player-mpd-server-name "localhost")
(setq emms-player-mpd-server-port "6601")

;; If your MusicPD setup requires a password, you will to set emms-player-mpd-server-password as follows.
;;(setq emms-player-mpd-server-password "mypassword")

;; To get track information from MusicPD, invoke the following:
(add-to-list 'emms-info-functions 'emms-info-mpd)

;; Adding `emms-player-mpd' to your Emms player list is accomplished by invoking:
(add-to-list 'emms-player-list 'emms-player-mpd)

(setq emms-player-mpd-sync-playlist t)

(defvar emms-browser-info-title-format "%i%n  :: %a - %A - %T. %t")
(defvar emms-browser-playlist-info-title-format
  emms-browser-info-title-format)

;; Playlist format
(defun my-describe (track)
  (let* ((empty "...")
         (name (emms-track-name track))
         (type (emms-track-type track))
         (short-name (file-name-nondirectory name))
         (play-count (or (emms-track-get track 'play-count) 0))
         (last-played (or (emms-track-get track 'last-played) '(0 0 0)))
         (artist (or (emms-track-get track 'info-artist) empty))
         (year (emms-track-get track 'info-year))
         (playing-time (or (emms-track-get track 'info-playing-time) 0))
         (min (/ playing-time 60))
         (sec (% playing-time 60))
         (album (or (emms-track-get track 'info-album) empty))
         (tracknumber (emms-track-get track 'info-tracknumber))
         (short-name (file-name-sans-extension
                      (file-name-nondirectory name)))
         (title (or (emms-track-get track 'info-title) short-name))
         (rating (emms-score-get-score name))
         (rate-char ?☭)
         )
    (format "%15s - %.4s [%-20s] - %2s. %-30s |%2d %s"
            artist
            year
            album
            tracknumber
            title
            play-count
            (make-string rating rate-char)))
)

(setq emms-track-description-function 'my-describe)
;; end EMMS

;; smartparent config
;;;;;;;;;
;; global
;; (require 'smartparens-config)
(smartparens-global-mode t)

;; highlights matching pairs
(show-smartparens-global-mode t)

;;;;;;;;;;;;;;;;;;;;;;;;
;; keybinding management

;; (define-key sp-keymap (kbd "C-M-f") 'sp-forward-sexp)
;; (define-key sp-keymap (kbd "C-M-b") 'sp-backward-sexp)

;; (define-key sp-keymap (kbd "C-M-d") 'sp-down-sexp)
;; (define-key sp-keymap (kbd "C-M-a") 'sp-backward-down-sexp)
;; (define-key sp-keymap (kbd "C-S-a") 'sp-beginning-of-sexp)
;; (define-key sp-keymap (kbd "C-S-d") 'sp-end-of-sexp)

;; (define-key sp-keymap (kbd "C-M-e") 'sp-up-sexp)
;; (define-key emacs-lisp-mode-map (kbd ")") 'sp-up-sexp)
;; (define-key sp-keymap (kbd "C-M-u") 'sp-backward-up-sexp)
;; (define-key sp-keymap (kbd "C-M-t") 'sp-transpose-sexp)

;; (define-key sp-keymap (kbd "C-M-n") 'sp-next-sexp)
;; (define-key sp-keymap (kbd "C-M-p") 'sp-previous-sexp)

;; (define-key sp-keymap (kbd "C-M-k") 'sp-kill-sexp)
;; (define-key sp-keymap (kbd "C-M-w") 'sp-copy-sexp)

;; (define-key sp-keymap (kbd "M-<delete>") 'sp-unwrap-sexp)
;; (define-key sp-keymap (kbd "M-<backspace>") 'sp-backward-unwrap-sexp)

;; (define-key sp-keymap (kbd "C-<right>") 'sp-forward-slurp-sexp)
;; (define-key sp-keymap (kbd "C-<left>") 'sp-forward-barf-sexp)
;; (define-key sp-keymap (kbd "C-M-<left>") 'sp-backward-slurp-sexp)
;; (define-key sp-keymap (kbd "C-M-<right>") 'sp-backward-barf-sexp)

;; (define-key sp-keymap (kbd "M-D") 'sp-splice-sexp)
;; (define-key sp-keymap (kbd "C-M-<delete>") 'sp-splice-sexp-killing-forward)
;; (define-key sp-keymap (kbd "C-M-<backspace>") 'sp-splice-sexp-killing-backward)
;; (define-key sp-keymap (kbd "C-S-<backspace>") 'sp-splice-sexp-killing-around)

;; (define-key sp-keymap (kbd "C-]") 'sp-select-next-thing-exchange)
;; (define-key sp-keymap (kbd "C-<left_bracket>") 'sp-select-previous-thing)
;; (define-key sp-keymap (kbd "C-M-]") 'sp-select-next-thing)

;; (define-key sp-keymap (kbd "M-F") 'sp-forward-symbol)
;; (define-key sp-keymap (kbd "M-B") 'sp-backward-symbol)

;; (define-key sp-keymap (kbd "H-t") 'sp-prefix-tag-object)
;; (define-key sp-keymap (kbd "H-p") 'sp-prefix-pair-object)
;; (define-key sp-keymap (kbd "H-s c") 'sp-convolute-sexp)
;; (define-key sp-keymap (kbd "H-s a") 'sp-absorb-sexp)
;; (define-key sp-keymap (kbd "H-s e") 'sp-emit-sexp)
;; (define-key sp-keymap (kbd "H-s p") 'sp-add-to-previous-sexp)
;; (define-key sp-keymap (kbd "H-s n") 'sp-add-to-next-sexp)
;; (define-key sp-keymap (kbd "H-s j") 'sp-join-sexp)
;; (define-key sp-keymap (kbd "H-s s") 'sp-split-sexp)

;;;;;;;;;;;;;;;;;;
;; pair management

(sp-local-pair 'minibuffer-inactive-mode "'" nil :actions nil)

;;; markdown-mode
(sp-with-modes '(markdown-mode gfm-mode rst-mode)
  (sp-local-pair "*" "*" :bind "C-*")
  (sp-local-tag "2" "**" "**")
  (sp-local-tag "s" "```scheme" "```")
  (sp-local-tag "<"  "<_>" "</_>" :transform 'sp-match-sgml-tags))

;;; tex-mode latex-mode
(sp-with-modes '(tex-mode plain-tex-mode latex-mode)
  (sp-local-tag "i" "\"<" "\">")
  (sp-local-tag "<"  "<_>" "</_>" :transform 'sp-match-sgml-tags))

;;; html-mode
(sp-with-modes '(html-mode sgml-mode)
  (sp-local-pair "<" ">"))

;;; lisp modes
(sp-with-modes sp--lisp-modes
  (sp-local-pair "(" nil :bind "C-("))
;; end smart parens
