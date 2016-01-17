(setq debug-on-error t)

(setq-default fill-column 80)

(when (fboundp 'tool-bar-mode) (tool-bar-mode -1))
(when (fboundp 'scroll-bar-mode) (scroll-bar-mode -1))
(unless (display-graphic-p) (menu-bar-mode -1))


(load "~/.emacs.d/lisp/my-packages.el")
(require 'my-packages)

(setq custom-file "~/.emacs.d/lisp/custom.el")
(load custom-file)

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


;;(setq gtags-auto-update nil) ;; drupal fix/workaround?

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


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; revert

(global-auto-revert-mode 1)
(setq auto-revert-verbose nil)

(global-set-key (kbd "<f5>") 'revert-buffer)

;; end revert


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

;;;;;;;;;;;;;;; end general settings

;;;;;;;; Lets load the modes
;; init.el
(require 'package)
;(package-initialize)

;; load my setup
(require 'my-helm)
(require 'my-projectile)
(require 'my-web-mode)
(require 'my-smartparents)
(require 'my-highlight-parentheses)
(require 'my-rainbow-delimiters)
(require 'my-company)
(require 'my-flymake)
(require 'my-auto-complete)

(require 'my-emms)
;(require 'my-tabbar)

(require 'my-sml)

(load "lua-mode")
;;(load "php-mode")
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




(require 'php-auto-yasnippets)
;; (load "php-auto-yasnippets")
(define-key php-mode-map (kbd "C-c C-y") 'yas/create-php-snippet)
(payas/ac-setup)

(require 'emmet-mode)
(require 'ac-emmet)
(add-hook 'sgml-mode-hook 'ac-emmet-html-setup)
(add-hook 'css-mode-hook 'ac-emmet-css-setup)

(require 'notifications)

(require 'hl-anything)
;;(require 'hl-paren-mode)

(require 'easy-repeat)

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
