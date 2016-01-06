(setq package-archives '(("gnu" . "http://elpa.gnu.org/packages/")
			 ("elpa" . "http://tromey.com/elpa/")
			 ("melpa" . "http://melpa.milkbox.net/packages/")
			 ("marmalade" . "http://marmalade-repo.org/packages/")
			 ))

;; list the packages you want
;; with C-h v package-activated-list
(setq package-list
      '(
        ac-emmet
        ag
        async
        auto-complete
        auto-highlight-symbol
        bash-completion
        color-theme
        color-theme-github
        company
        company-web
        csv-mode
        darktooth-theme
        dash
        diff-hl
        drupal-mode
        easy-repeat
        ecb
        elscreen
        emmet-mode
        emms
        emms-info-mediainfo
        epl
        exec-path-from-shell
        f
        findr
        flx
        flx-ido
        flycheck
        flycheck-pyflakes
        flycheck-tip
        flylisp
        flymake-css
        flymake-cursor
        flymake-easy
        flymake-gjshint
        flymake-haml
        flymake-hlint
        flymake-jshint
        flymake-jslint
        flymake-json
        flymake-less
        flymake-lua
        flymake-php
        flymake-phpcs
        flymake-python-pyflakes
        flymake-ruby
        flymake-sass
        flymake-shell
        flymake-yaml
        fringe-helper
        git-commit
        git-gutter+
        git-gutter-fringe+
        github-theme
        gitignore-mode
        haml-mode
        helm
        helm-ag
        helm-company
        helm-core
        helm-emmet
        helm-fuzzier
        helm-ls-git
        helm-projectile
        helm-rails
        helm-rhythmbox
        hide-lines
        highlight-indentation
        highlight-parentheses
        hl-anything
        iedit
        inf-ruby
        inflections
        js3-mode
        json-mode
        json-reformat
        json-snatcher
        jump
        less-css-mode
        let-alist
        lua-mode
        magit
        magit-popup
        markdown-mode
        minimap
        move-text
        mustard-theme
        neotree
        nginx-mode
        php-auto-yasnippets
        php-mode
        pkg-info
        popup
        projectile
        projectile-rails
        railscasts-theme
        rainbow-delimiters
        rainbow-mode
        rake
        rinari
        ruby-compilation
        rvm
        s
        sass-mode
        scss-mode
        seq
        slim-mode
        smartparens
        solarized-theme
        ssh-config-mode
        syslog-mode
        tea-time
        twittering-mode
        undo-tree
        vcl-mode
        web-beautify
        web-completion-data
        web-mode
        with-editor
        yaml-mode
        yasnippet
        )
      )

; activate all the packages (in particular autoloads)
(package-initialize)

;; (setq abg-required-packages
;;       (list 'xml-rpc 'magit 'gh 'inf-ruby))
;; ; ...

; fetch the list of packages available
(unless package-archive-contents
  (package-refresh-contents))

; install the missing packages
(dolist (package package-list)
  (when (not (package-installed-p package))
    ;(package-refresh-contents)
    (package-install package)))



(add-to-list 'load-path "~/.emacs.d/lisp")
(add-to-list 'load-path "~/.emacs.d/elpa")

(provide 'my-packages)
