(setq debug-on-error t)
(setq debug-on-quit t)

(load "~/.emacs.d/lisp/my-default-stuff.el")

(require 'package)

(setq package-archives '(("gnu" . "http://elpa.gnu.org/packages/")
                         ("elpa" . "http://tromey.com/elpa/")
                         ("melpa" . "http://melpa.org/packages/")
                         ("melpa-stable" . "http://stable.melpa.org/packages/")
                         ("marmalade" . "http://marmalade-repo.org/packages/")
                         ))

;; keep the installed packages in .emacs.d
(setq package-user-dir (expand-file-name "elpa" user-emacs-directory))

(package-initialize)

;; keep the installed packages in .emacs.d
(setq package-user-dir (expand-file-name "elpa" user-emacs-directory))

;; fetch the list of packages available
(unless package-archive-contents
  (package-refresh-contents))

(add-to-list 'load-path "~/.emacs.d/lisp")
(add-to-list 'load-path "~/.emacs.d/elpa")

(unless (package-installed-p 'use-package)
  (package-install 'use-package))

(require 'use-package)
(setq use-package-verbose t)

(use-package cua-base
  :init (cua-mode 1)
  :config)

(use-package exec-path-from-shell
  :config
  (push "HISTFILE" exec-path-from-shell-variables)
  (setq exec-path-from-shell-check-startup-files nil)
  (exec-path-from-shell-copy-env "PATH")
  (exec-path-from-shell-copy-env "JAVA_HOME")
  (exec-path-from-shell-initialize))

(use-package ido
  :ensure t
  :config
  (setq ido-enable-prefix nil
        ido-enable-flex-matching t
        ido-create-new-buffer 'always
        ido-use-filename-at-point t ;; 'guess
        ido-default-file-method 'selected-window
        ido-auto-merge-work-directories-length -1)
  (ido-mode +1))

(use-package magit
  :ensure t
  :bind (("C-x g" . magit-status)))

(use-package diff-hl
  :ensure t
  :config
  (global-diff-hl-mode)
  (diff-hl-flydiff-mode)
  ;; (diff-hl-margin-mode)
  (diff-hl-dired-mode)
  ;; using magit 2.4 or newer
  (add-hook 'magit-post-refresh-hook 'diff-hl-magit-post-refresh))

(use-package rainbow-mode
  :ensure t
  :config
  (add-hook 'prog-mode-hook #'rainbow-mode))

(use-package whitespace
  :init
  (dolist (hook '(prog-mode-hook text-mode-hook))
    (add-hook hook #'whitespace-mode))
  (add-hook 'before-save-hook #'whitespace-cleanup)
  :config
  (setq whitespace-line-column 80) ;; limit line length
  (setq whitespace-style '(face tabs empty trailing lines-tail)))

(use-package smartparens
  :ensure t
  :diminish smartparens-mode
  :config
  (progn
    (require 'smartparens-config)
    (sp-local-pair 'web-mode "{" "}" :actions nil)
    (sp-local-pair 'web-mode "<" ">" :actions nil)
    (sp-local-pair 'minibuffer-inactive-mode "'" nil :actions nil)
    ;;; markdown-mode
    (sp-with-modes '(markdown-mode gfm-mode rst-mode)
      (sp-local-pair "*" "*" :bind "C-*")
      (sp-local-tag "2" "**" "**")
      (sp-local-tag "s" "```scheme" "```")
      (sp-local-tag "<"  "<_>" "</_>" :transform 'sp-match-sgml-tags))

    ;; tex-mode latex-mode
    (sp-with-modes '(tex-mode plain-tex-mode latex-mode)
      (sp-local-tag "i" "\"<" "\">")
      (sp-local-tag "<"  "<_>" "</_>" :transform 'sp-match-sgml-tags))

    ;; html-mode
    (sp-with-modes '(html-mode sgml-mode)
      (sp-local-pair "<" ">"))

    ;; lisp modes
    (sp-with-modes sp--lisp-modes
      (sp-local-pair "(" nil :bind "C-("))
    (smartparens-global-mode t)
    (show-smartparens-global-mode t)
    )

  )

(use-package enh-ruby-mode
  :ensure t
  :mode
  (("Capfile" . enh-ruby-mode)
   ("Gemfile\\'" . enh-ruby-mode)
   ("Rakefile" . enh-ruby-mode)
   ("\\.rb" . enh-ruby-mode)
   ("\\.ru" . enh-ruby-mode)
   ("\\.xlsx\\.axlsx\\'" . enh-ruby-mode))

  :config
  (progn
    (add-to-list 'interpreter-mode-alist '("ruby" . enh-ruby-mode))
    (remove-hook 'enh-ruby-mode-hook 'erm-define-faces)
    (setq enh-ruby-deep-indent-paren nil)

    (use-package rvm
      :ensure t
      :init (rvm-use-default)
      :config (setq rvm-verbose nil)
      (add-hook 'enh-ruby-mode-hook #'subword-mode))

    (use-package rspec-mode
      :ensure t
      :config
      (progn
        (setq rspec-use-rvm t)
        (eval-after-load 'rspec-mode
          '(rspec-install-snippets))
        ))

    (use-package yard-mode
      :ensure t
      :config
      (add-hook 'enh-ruby-mode-hook 'yard-mode))

    (use-package robe
      :ensure t
      :config
      (add-hook 'enh-ruby-mode-hook 'robe-mode)
      (add-hook 'robe-mode-hook 'ac-robe-setup))
    (defadvice inf-ruby-console-auto (before activate-rvm-for-robe activate)
      (rvm-activate-corresponding-ruby))
    ))

(use-package inf-ruby
  :ensure t
  :after enh-ruby-mode
  :config
  (add-hook 'enh-ruby-mode-hook #'inf-ruby-minor-mode))

(use-package php-mode
  :ensure t
  ;;:mode "\\.php[345]?\\'"
  :config
  (add-hook 'php-mode-hook (lambda () (setq comment-start "// "
                                            comment-end ""
                                            comment-style 'indent
                                            comment-use-syntax t
                                            ))))

;; projectile
(use-package projectile
  :diminish projectile-mode
  :config
  (setq projectile-enable-caching nil
        projectile-completion-system 'helm
        projectile-find-dir-includes-top-level t
        projectile-switch-project-action 'helm-projectile)
  ;; (projectile-global-mode)
  (add-hook 'text-mode-hook #'projectile-mode)
  (add-hook 'prog-mode-hook #'projectile-mode)
  (add-hook 'magit-mode-hook #'projectile-mode)
  (add-hook 'css-mode-hook #'projectile-mode)
  (add-hook 'yaml-mode-hook #'projectile-mode)
  (add-hook 'gitignore-mode-hook #'projectile-mode)

  )

(use-package helm
  :ensure    helm
  :config    (setq helm-ff-transformer-show-only-basename nil
                   helm-boring-file-regexp-list           '("\\.git$" "\\.svn$" "\\.elc$")
                   helm-yank-symbol-first                 t
                   helm-buffers-fuzzy-matching            t
                   helm-ff-auto-update-initial-value      t
                   helm-input-idle-delay                  0.1
                   helm-idle-delay                        0.1
                   helm-semantic-fuzzy-match t
                   helm-imenu-fuzzy-match    t
                   helm-lisp-fuzzy-completion t
                   helm-M-x-fuzzy-match t
                   )

  :init      (progn
               (require 'helm-config)
               (helm-mode t)
               ;; (helm-adaptative-mode t)

               (use-package helm-ag
                 :ensure    helm-ag
                 :bind      ("C-c a" . helm-ag))

               (use-package helm-descbinds
                 :ensure    helm-descbinds
                 :bind      ("C-h b"   . helm-descbinds))

               (add-hook 'eshell-mode-hook
                         #'(lambda ()
                             (bind-key "M-p" 'helm-eshell-history eshell-mode-map)))

               (use-package helm-swoop
                 :ensure    helm-swoop
                 :bind      (("C-c o" . helm-swoop)
                             ("C-c M-o" . helm-multi-swoop)))

               (use-package helm-fuzzier
                 :ensure helm-fuzzier
                 :config
                 (helm-fuzzier-mode 1))

               ;; (when (executable-find "ack-grep")
               ;;   (setq helm-grep-default-command "ack-grep -Hn --no-group --no-color %e %p %f"
               ;;         helm-grep-default-recurse-command "ack-grep -H --no-group --no-color %e %p %f"))
               ;; use silver searcher when available
               (when (executable-find "ag-grep")
                 (setq helm-grep-default-command "ag-grep -Hn --no-group --no-color %e %p %f"
                       helm-grep-default-recurse-command "ag-grep -H --no-group --no-color %e %p %f"))
               (bind-key "C-c C-SPC" 'helm-ff-run-toggle-auto-update helm-find-files-map))

  :bind (("C-x r l" . helm-bookmarks)
         ("M-x" . helm-M-x)
         ("C-h i"   . helm-google-suggest)
         ("M-y"     . helm-show-kill-ring)
         ("C-h a"   . helm-apropos)
         ("C-x C-f" . helm-find-files)
         ("C-x p" .   helm-top)
         ("C-x C-b" . helm-buffers-list)
         ("C-x b"   . helm-mini))

  :diminish helm-mode)

(use-package helm-projectile
  :ensure t
  :after helm
  :commands (helm-projectile)
  :config
  (setq projectile-completion-system 'helm)
  (setq projectile-find-dir-includes-top-level t)
  (helm-projectile-on)
  ;; (helm-projectile-toggle 1)
  )

(use-package projectile-rails
  :ensure t
  :defer t
  :init
  (progn
    (add-hook 'projectile-mode-hook 'projectile-rails-on)))

(use-package web-mode
  :ensure t
  :mode (("\\.html\\'" . web-mode)
         ("\\.html\\.erb\\'" . web-mode)
         ("\\.mustache\\'" . web-mode)
         ("\\.jinja\\'" . web-mode)
         ("\\.php\\'" . web-mode)
         )
  :init
  (progn
    (setq web-mode-engines-alist
          '(("\\.jinja\\'"  . "django")))
    (setq web-mode-markup-indent-offset 2)
    (setq web-mode-css-indent-offset 2)
    (setq web-mode-code-indent-offset 2)
    (setq web-mode-indent-style 2)

    ;;(define-key web-mode-map (kbd "C-n") 'web-mode-tag-match)
    (setq web-mode-disable-auto-pairing nil)
    (setq web-mode-enable-block-face t)
    (setq web-mode-enable-part-face t)
    (setq web-mode-enable-comment-keywords t)
    (setq web-mode-enable-heredoc-fontification t)
    (setq web-mode-disable-css-colorization nil)
    (setq web-mode-enable-current-element-highlight t)
    (setq web-mode-comment-style 2)
    (setq web-mode-style-padding 2)
    (setq web-mode-script-padding 2)
    ;; (add-to-list 'auto-mode-alist '("\\.php\\'" . php-mode))
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
    )
  :config
  (progn
    (set-face-attribute 'web-mode-css-at-rule-face nil :foreground "Pink3")
    )
  )

(use-package slim-mode
  :ensure t
  :defer t)

(use-package gitignore-mode
  :ensure t
  :defer t)

(use-package railscasts-reloaded-theme
  :ensure t
  :init
  (load-theme 'railscasts-reloaded t))

(use-package move-text
  :ensure t
  :config
  (move-text-default-bindings))

(use-package ssh-config-mode
  :ensure t
  :init (autoload 'ssh-config-mode "ssh-config-mode" t)
  (add-hook 'ssh-config-mode-hook 'turn-on-font-lock)
  :mode (("/\\.ssh/config\\'"     . ssh-config-mode)
         ("/sshd?_config\\'"      . ssh-config-mode)
         ("/known_hosts\\'"       . ssh-known-hosts-mode)
         ("/authorized_keys2?\\'" . ssh-authorized-keys-mode)))

(use-package nginx-mode
  :ensure t
  :config
  (progn
    (add-to-list 'auto-mode-alist '("/etc/nginx/nginx.conf\\'" . nginx-mode))
    (add-to-list 'auto-mode-alist '("/etc/nginx/sites-\\(enabled\\|available\\)/.*\\'" . nginx-mode))))

(use-package docker
  :ensure t
  :commands docker-mode)

(use-package dockerfile-mode
  :ensure t
  :mode "Dockerfile.*\\'")

(use-package markdown-mode
  :ensure t
  :config
  (add-to-list 'auto-mode-alist '("\\.text\\'" . markdown-mode))
  (add-to-list 'auto-mode-alist '("\\.markdown\\'" . markdown-mode))
  (add-to-list 'auto-mode-alist '("\\.md\\'" . markdown-mode))
  (add-to-list 'auto-mode-alist '("README\\.md\\'" . gfm-mode)))

(use-package ag
  :ensure t)

(use-package vcl-mode
  :ensure t
  :mode "\\.vcl\\'")

(use-package yaml-mode
  :ensure t
  :mode "\\.yml$")

(use-package linum
  :config
  (progn
    (add-hook 'prog-mode-hook 'linum-mode)
    (add-hook 'text-mode-hook 'linum-mode)
    (add-hook 'yaml-mode-hook 'linum-mode)
    ))

(use-package emmet-mode
  :commands (emmet-mode)
  :config
  (progn
    (add-hook 'emmet-mode-hook (lambda ()
                                 (setq emmet-preview-default nil)
                                 (setq emmet-indentation 2)))
    (use-package ac-emmet
      :ensure t
      :config
      (add-hook 'sgml-mode-hook 'ac-emmet-html-setup)
      (add-hook 'css-mode-hook 'ac-emmet-css-setup)
      )
    )
  :init
  (progn
    (add-hook 'sgml-mode-hook 'emmet-mode)
    (add-hook 'web-mode-hook 'emmet-mode)
    (add-hook 'css-mode-hook 'emmet-mode)))

(use-package undo-tree
  :ensure t
  :diminish (undo-tree-mode . "")
  :config
  (global-undo-tree-mode 1))

(use-package iedit
  :ensure t)

(use-package smart-mode-line
  :ensure smart-mode-line
  :init
  (progn
    (setq sml/no-confirm-load-theme t)
    (display-time-mode)
    ;;(setq powerline-arrow-shape 'curve)
    ;;(setq powerline-default-separator-dir '(right . left))
    ;;(setq sml/theme 'powerline)
    (setq sml/theme 'dark)
    (setq sml/mode-width 0)
    (setq sml/name-width 20)
    (rich-minority-mode 1)
    (setf rm-blacklist "")
    (sml/setup)))

(use-package auto-complete
  :ensure t
  :init
  (progn
    (require 'auto-complete-config)
    (setq ac-auto-show-menu t)
    (setq ac-auto-start t)
    (setq ac-quick-help-delay 0.3)
    (setq ac-quick-help-height 30)
    (setq ac-show-menu-immediately-on-auto-complete t)
    (ac-config-default)
    )
  :config
  (ac-linum-workaround)
  (add-to-list 'ac-dictionary-directories "~/.emacs.d/ac-dict")
  (add-hook 'enh-ruby-mode-hook
            (lambda ()
              (make-local-variable 'ac-stop-words)
              (add-to-list 'ac-stop-words "end"))))

(use-package yasnippet
  :ensure t
  :init
  (progn
    (yas-global-mode 1)))

(use-package multiple-cursors
  :ensure t
  :config
  (global-set-key (kbd "C-S-c C-S-c") 'mc/edit-lines)
  (global-set-key (kbd "C->") 'mc/mark-next-like-this)
  (global-set-key (kbd "C-<") 'mc/mark-previous-like-this)
  (global-set-key (kbd "C-c C-<") 'mc/mark-all-like-this))

(use-package erc
  :defer t
  :init
  (progn
    (setq
     erc-hide-list '("JOIN" "PART" "QUIT")
     erc-insert-timestamp-function 'erc-insert-timestamp-left
     erc-timestamp-format "[%H:%M] "
     erc-timestamp-only-if-changed-flag nil
     erc-truncate-mode t

     ;; erc-auto-query (quote window-noselect)
     erc-auto-query 'buffer

     erc-autoaway-mode t
     erc-away-nickname nil
     erc-join-buffer (quote window-noselect)
     erc-modules (quote (completion list menu scrolltobottom autojoin button dcc fill irccontrols match move-to-prompt netsplit networks noncommands readonly ring stamp spelling track))
     erc-nick-notify-mod t
     erc-prompt ">"
     erc-public-away-p t
     erc-speedbar-sort-users-type (quote alphabetical)
     erc-user-full-name "Tomás Solar"
     erc-email-userid "tsolar"

     ;; Join channels whenever connecting to Freenode.
     erc-autojoin-channels-alist '(("freenode.net" "#parabola" "#fsfla" "#flisol-cl")
                                   )
     ;; Interpret mIRC-style color commands in IRC chats
     erc-interpret-mirc-color t

     ;; Kill buffers for channels after /part
     erc-kill-buffer-on-part t
     ;; Kill buffers for private queries after quitting the server
     erc-kill-queries-on-quit t
     ;; Kill buffers for server messages after quitting the server
     erc-kill-server-buffer-on-quit t
     )
    )
  :config
  (progn
    (use-package :erc-hl-nicks
      :ensure t)
    (use-package :erc-nick-notify
      :ensure t)
    (erc-spelling-mode 1)
    (add-hook
     'window-configuration-change-hook
     (lambda () (setq erc-fill-column (- (window-width) 2))))
    )
  )

(setq debug-on-error nil)
(setq debug-on-quit nil)
