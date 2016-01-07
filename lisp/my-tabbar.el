;; -*- mode: emacs-lisp; coding: utf-8-unix; indent-tabs-mode: nil -*-

(when (require 'tabbar nil t)
  (tabbar-mode 1)
  (tabbar-mwheel-mode -1)
  (setq tabbar-buffer-groups-function nil)

  ;; disabling buttons
  (dolist (btn '(tabbar-buffer-home-button
                 tabbar-scroll-left-button
                 tabbar-scroll-right-button))
    (set btn (cons (cons "" nil)
                   (cons "" nil))))

  (setq tabbar-auto-scroll-flag nil)
  (setq tabbar-separator '(1.5))

  ;; adding spaces
  (defun tabbar-buffer-tab-label (tab)
    "Return a label for TAB.
That is, a string used to represent it on the tab bar."
    (let ((label  (if tabbar--buffer-show-groups
                      (format " [%s] " (tabbar-tab-tabset tab))
                    (format " %s " (tabbar-tab-value tab)))))
      ;; Unless the tab bar auto scrolls to keep the selected tab
      ;; visible, shorten the tab label to keep as many tabs as possible
      ;; in the visible area of the tab bar.
      (if tabbar-auto-scroll-flag
          label
        (tabbar-shorten
         label (max 1 (/ (window-width)
                         (length (tabbar-view
                                  (tabbar-current-tabset)))))))))

  (set-face-attribute
   'tabbar-default nil
   :family "Inconsolata"
   :background "black"
   :foreground "gray72"
   :height 1.0)
  (set-face-attribute
   'tabbar-unselected nil
   :background "black"
   :foreground "gray72"
   :box nil)
  (set-face-attribute
   'tabbar-selected nil
   :background "black"
   :foreground "#c82829"
   :box nil)
  (set-face-attribute
   'tabbar-button nil
   :box nil)
  (set-face-attribute
   'tabbar-separator nil
   :height 1.2)
  (set-face-attribute
   'tabbar-highlight nil
   :background "white"
   :foreground "black"
   :underline nil
   :box '(:line-width 5 :color "white" :style nil))

  ;; ;; Tabbar settings
  ;; (set-face-attribute
  ;;  'tabbar-default nil
  ;;  :background "gray20"
  ;;  :foreground "gray20"
  ;;  :box '(:line-width 1 :color "gray20" :style nil))
  ;; (set-face-attribute
  ;;  'tabbar-unselected nil
  ;;  :background "gray30"
  ;;  :foreground "white"
  ;;  :box '(:line-width 5 :color "gray30" :style nil))
  ;; (set-face-attribute
  ;;  'tabbar-selected nil
  ;;  :background "gray75"
  ;;  :foreground "black"
  ;;  :box '(:line-width 5 :color "gray75" :style nil))
  ;; (set-face-attribute
  ;;  'tabbar-highlight nil
  ;;  :background "white"
  ;;  :foreground "black"
  ;;  :underline nil
  ;;  :box '(:line-width 5 :color "white" :style nil))
  ;; (set-face-attribute
  ;;  'tabbar-button nil
  ;;  :box '(:line-width 1 :color "gray20" :style nil))
  ;; (set-face-attribute
  ;;  'tabbar-separator nil
  ;;  :background "gray20"
  ;;  :height 0.6)

  ;; (custom-set-variables
  ;;  '(tabbar-separator (quote (0.5))))

  ;; (setq tabbar-separator '(1.5))

;;   (defvar my-tabbar-displayed-buffers
;;     '("*Backtrace*" "*Colors*" "*Faces*" "*vc-")
;;     "*Regexps matches buffer name always included tabs.")
;;   (defun my-tabbar-buffer-list ()
;;     "Return the list of buffers to show in tab.
;;   Exclude buffers whose name starts with a space or an asterisk.
;;   The current buffer and buffers match `my-tabbar-displayed-buffers'
;;   are always included."
;;     (let* ((hides (list ?\  ?\*))
;;            (re (regexp-opt my-tabbar-displayed-buffers))
;;            (cur-buf (current-buffer))
;;            (tabs (delq nil
;;                        (mapcar (lambda (buf)
;;                                  (let ((name (buffer-name buf)))
;;                                    (when (or (string-match re name)
;;                                              (not (memq (aref name 0) hides)))
;;                                      buf)))
;;                                (buffer-list)))))
;;       ;; Always include the current buffer
;;       (if (memq cur-buf tabs)
;;           tabs
;;         (cons cur-buf tabs))))
;;   (setq tabbar-buffer-list-function 'my-tabbar-buffer-list)

;; ;;;;;;;;;;;;;;;;;;;;;;;;;;
  (global-set-key [C-prior] 'tabbar-backward-tab)
  (global-set-key [C-next] 'tabbar-forward-tab)
)

(provide 'my-tabbar)
