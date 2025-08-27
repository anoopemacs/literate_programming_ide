;; Enable vertico
(use-package vertico
  :custom
  ;; (vertico-scroll-margin 0) ;; Different scroll margin
  (vertico-count 25) ;; Show more candidates
  ;; (vertico-resize t) ;; Grow and shrink the Vertico minibuffer
  ;; (vertico-cycle t) ;; Enable cycling for `vertico-next/previous'
  :init
  (vertico-mode)
  :bind
  (("C-x C-r" . recentf))
  :hook
  (rfn-eshadow-update-overlay . vertico-directory-tidy)
  :config
  (use-package vertico-multiform
  :after vertico
  :ensure nil ;;installed automatically as part of vertico. Just enabling this optional extension
  :config
  (vertico-multiform-mode +1)
  (defvar +vertico-transform-functions nil))
(define-key vertico-map (kbd "C-k") (kmacro "C-. k C-g C-x b")))

;; Use `consult-completion-in-region' if Vertico is enabled.
;; Otherwise use the default `completion--in-region' function.
(setq completion-in-region-function
      (lambda (&rest args)
        (apply (if vertico-mode
                   #'consult-completion-in-region
                 #'completion--in-region)
               args)))

(defun anup/routed/find-file (&optional _prefix)
  "If C-x C-f is called with a prefix argument, open files in current project. Open file with sudo if two prefix arguments are given."
  (interactive "P")
  (cond ((equal current-prefix-arg '(4))
         ;;nil means dont create a new window, reuse current window to open the file in project
         (find-file-in-project nil))
        (t (call-interactively 'find-file-at-point))))

(global-set-key (kbd "C-x C-f") 'anup/routed/find-file)
(global-set-key (kbd "C-x C-d") 'anup/routed/find-file)
(global-set-key (kbd "C-x d") 'anup/routed/find-file)

(cl-defmethod vertico--format-candidate :around
  (cand prefix suffix index start &context ((not +vertico-transform-functions) null))
  (dolist (fun (ensure-list +vertico-transform-functions))
    (setq cand (funcall fun cand)))
  (cl-call-next-method cand prefix suffix index start))

(defun +vertico-highlight-directory (file)
  "If FILE ends with a slash, highlight it as a directory."
  (if (string-suffix-p "/" file)
      (propertize file 'face 'marginalia-file-priv-dir) ; or face 'dired-directory
    file))

;; add-to-list works if 'file isn't already in the alist
;; setq can be used but will overwrite all existing values
(add-to-list 'vertico-multiform-categories
             '(file
               (+vertico-transform-functions . +vertico-highlight-directory)))

;; function to highlight enabled modes similar to counsel-M-x
(defun +vertico-highlight-enabled-mode (cmd)
  "If MODE is enabled, highlight it as font-lock-constant-face."
  (let ((sym (intern cmd)))
    (if (or (eq sym major-mode)
            (and
             (memq sym minor-mode-list)
             (boundp sym)))
      (propertize cmd 'face 'font-lock-string-face)
      cmd)))

(add-to-list 'vertico-multiform-commands
             '(execute-extended-command 
               (+vertico-transform-functions . +vertico-highlight-enabled-mode)))

;; Optionally use the `orderless' completion style.
(use-package orderless
  :custom
  ;; Configure a custom style dispatcher (see the Consult wiki)
  ;; (orderless-style-dispatchers '(+orderless-consult-dispatch orderless-affix-dispatch))
  ;; (orderless-component-separator #'orderless-escapable-split-on-space)
  (completion-styles '(orderless basic))
  (completion-category-defaults nil)
  (completion-category-overrides '((file (styles partial-completion)))))

;; Enable rich annotations using the Marginalia package
(use-package marginalia
  ;; Bind `marginalia-cycle' locally in the minibuffer.  To make the binding
  ;; available in the *Completions* buffer, add it to the
  ;; `completion-list-mode-map'.
  :bind (:map minibuffer-local-map
         ("M-A" . marginalia-cycle))

  ;; The :init section is always executed.
  :init

  ;; Marginalia must be activated in the :init section of use-package such that
  ;; the mode gets enabled right away. Note that this forces loading the
  ;; package.
  (marginalia-mode))

(use-package embark
  :bind
  (("C-." . embark-act)
   ("C-;" . embark-dwim)
   ("C-h B" . embark-bindings)  ;; alternative for `describe-bindings'

   :map embark-file-map
   ;;("S" . sudo-edit)
("S" . sudo-find-file)

   :map embark-general-map
   ;;("S" . sudo-edit)
("S" . sudo-find-file)
("k" . kill-buffer)
("G" . browse-url-dwim-search)
("C-c g" . browse-url-dwim-search))
  
  :init
  ;; Optionally replace the key help with a completing-read interface
  (setq prefix-help-command #'embark-prefix-help-command)

  ;; Show the Embark target at point via Eldoc. You may adjust the
  ;; Eldoc strategy, if you want to see the documentation from
  ;; multiple providers. Beware that using this can be a little
  ;; jarring since the message shown in the minibuffer can be more
  ;; than one line, causing the modeline to move up and down:

  ;; (add-hook 'eldoc-documentation-functions #'embark-eldoc-first-target)
  ;; (setq eldoc-documentation-strategy #'eldoc-documentation-compose-eagerly)

  :config
  ;; Hide the mode line of the Embark live/completions buffers
  (add-to-list 'display-buffer-alist
	       '("\\`\\*Embark Collect \\(Live\\|Completions\\)\\*"
		 nil
		 (window-parameters (mode-line-format . none))))
  
  (defun sudo-find-file (file)
  "Open FILE as root."
  (interactive "FOpen file as root: ")
  (when (file-writable-p file)
    (user-error "File is user writeable, aborting sudo"))
  (find-file (if (file-remote-p file)
                 (concat "/" (file-remote-p file 'method) ":"
                         (file-remote-p file 'user) "@" (file-remote-p file 'host)
                         "|sudo:root@"
                         (file-remote-p file 'host) ":" (file-remote-p file 'localname))
               (concat "/sudo:root@localhost:" file))))
(setq embark-quit-after-action '((t . t) (kill-buffer . nil)))
(setf (alist-get 'kill-buffer embark-pre-action-hooks) nil))

(use-package smex
  :config
  (smex-initialize))

;; Example configuration for Consult
(use-package consult
  :bind
  (
   ;;("M-s" . consult-ripgrep)  ;;I prefer to use M-s as a prefix key
   ([remap Info-search] . consult-info)
   ("M-g M-g" . consult-goto-line)
   ("C-x r b" . consult-bookmark)            ;; orig. bookmark-jump
   ;;("M-s r" . consult-ripgrep)             ;; I prefer to use helm-rg bound to M-g M-s instead
   :map org-mode-map
   ;;("M-g h" . consult-org-heading)
   ("M-g h" . consult-org-heading)
   )
  ;; Replace bindings. Lazily loaded by `use-package'.
  ;;;;;;;;Re-enable after making M-s a prefix key
  ;;;;;;          :bind (;; C-c bindings in `mode-specific-map'
  ;;;;;;                 ("C-c M-x" . consult-mode-command)
  ;;;;;;                 ("C-c h" . consult-history)
  ;;;;;;                 ("C-c k" . consult-kmacro)
  ;;;;;;                 ("C-c m" . consult-man)
  ;;;;;;                 ("C-c i" . consult-info)
  ;;;;;;                 ;; C-x bindings in `ctl-x-map'
  ;;;;;;                 ("C-x M-:" . consult-complex-command)     ;; orig. repeat-complex-command
  ;;;;;;                 ("C-x p b" . consult-project-buffer)      ;; orig. project-switch-to-buffer
  ;;;;;;                 ;; Custom M-# bindings for fast register access
  ;;;;;;                 ("M-#" . consult-register-load)
  ;;;;;;                 ("M-'" . consult-register-store)          ;; orig. abbrev-prefix-mark (unrelated)
  ;;;;;;                 ("C-M-#" . consult-register)
  ;;;;;;                 ;; Other custom bindings
  ;;;;;;                 ("M-y" . consult-yank-pop)                ;; orig. yank-pop. I found the posframe annoying for this.
  ;;;;;;                 ;; M-g bindings in `goto-map'
  ;;;;;;                 ("M-g e" . consult-compile-error)
  ;;;;;;                 ("M-g f" . consult-flymake)               ;; Alternative: consult-flycheck
  ;;;;;;                 ("M-g o" . consult-outline)               ;; Alternative: consult-org-heading
  ;;;;;;                 ("M-g m" . consult-mark)
  ;;;;;;                 ("M-g k" . consult-global-mark)
  ;;;;;;                 ("M-g i" . consult-imenu)
  ;;;;;;                 ("M-g I" . consult-imenu-multi)
  ;;;;;;                 ;; M-s bindings in `search-map'
  ;;;;;;                 ("M-s d" . consult-find)                  ;; Alternative: consult-fd. Found it confusing. I prefer find-file-in-project
  ;;;;;;                 ("M-s c" . consult-locate)
  ;;;;;;                 ("M-s g" . consult-grep)
  ;;;;;;                 ("M-s G" . consult-git-grep)
  ;;;;;;                 
  ;;;;;;                 ("M-s l" . consult-line)
  ;;;;;;                 ("M-s L" . consult-line-multi)
  ;;;;;;                 ("M-s k" . consult-keep-lines)
  ;;;;;;                 ("M-s u" . consult-focus-lines)
  ;;;;;;                 ;; Isearch integration
  ;;;;;;                 ("M-s e" . consult-isearch-history)
  ;;;;;;                 :map isearch-mode-map
  ;;;;;;                 ("M-e" . consult-isearch-history)         ;; orig. isearch-edit-string
  ;;;;;;                 ("M-s e" . consult-isearch-history)       ;; orig. isearch-edit-string
  ;;;;;;                 ("M-s l" . consult-line)                  ;; needed by consult-line to detect isearch
  ;;;;;;                 ("M-s L" . consult-line-multi)            ;; needed by consult-line to detect isearch
  ;;;;;;                 ;; Minibuffer history
  ;;;;;;                 :map minibuffer-local-map
  ;;;;;;                 ("M-s" . consult-history)                 ;; orig. next-matching-history-element
  ;;;;;;                 ("M-r" . consult-history))                ;; orig. previous-matching-history-element

  ;; Enable automatic preview at point in the *Completions* buffer. This is
  ;; relevant when you use the default completion UI.
  :hook (completion-list-mode . consult-preview-at-point-mode)

  ;; The :init configuration is always executed (Not lazy)
  :init

  ;; Optionally configure the register formatting. This improves the register
  ;; preview for `consult-register', `consult-register-load',
  ;; `consult-register-store' and the Emacs built-ins.
  (setq register-preview-delay 0.5
        register-preview-function #'consult-register-format)

  ;; Optionally tweak the register preview window.
  ;; This adds thin lines, sorting and hides the mode line of the window.
  (advice-add #'register-preview :override #'consult-register-window)

  ;; Configure other variables and modes in the :config section,
  ;; after lazily loading the package.
  :config

  ;; Optionally configure preview. The default value
  ;; is 'any, such that any key triggers the preview.
  ;; (setq consult-preview-key 'any)
  ;; (setq consult-preview-key "M-.")
  ;; (setq consult-preview-key '("S-<down>" "S-<up>"))
  ;; For some commands and buffer sources it is useful to configure the
  ;; :preview-key on a per-command basis using the `consult-customize' macro.
  (consult-customize
   consult-theme :preview-key '(:debounce 0.2 any)
   consult-ripgrep consult-git-grep consult-grep
   consult-bookmark consult-recent-file consult-xref
   consult--source-bookmark consult--source-file-register
   consult--source-recent-file consult--source-project-recent-file
   ;; :preview-key "M-."
   :preview-key '(:debounce 0.4 any))

  ;; Optionally configure the narrowing key.
  ;; Both < and C-+ work reasonably well.
  (setq consult-narrow-key "<") ;; "C-+"

  ;; Optionally make narrowing help available in the minibuffer.
  ;; You may want to use `embark-prefix-help-command' or which-key instead.
  ;; (keymap-set consult-narrow-map (concat consult-narrow-key " ?") #'consult-narrow-help)

  
)

;; Consult users will also want the embark-consult package.
(use-package embark-consult
  :ensure t ; only need to install it, embark loads it after consult if found
  :hook
  (embark-collect-mode . consult-preview-at-point-mode))

(use-package wgrep)

(defun anoop/helper/_read-only-p (bname)
  "Predicate that checks if buffer named bname is open in read only mode"
  (with-current-buffer bname
    buffer-read-only))
(defun +vertico-greenify-read-only-buffers (completion-candidate-string0)
  "If buffer is opened in read only mode, add green color to this text"
  (if (anoop/helper/_read-only-p completion-candidate-string0)
      (propertize completion-candidate-string0 'face 'anoop-read-only-mode-greenish-face)
    completion-candidate-string0))
(defface anoop-read-only-mode-greenish-face
  '((t (:foreground "forest green")))
  "The face used for read only buffers inside Vertico 'C-x b' completion")
(defun anoop/switch-to-buffer ()
  (interactive)
  (switch-to-buffer (let ((vertico-multiform-categories  '(;;I have no idea why both the below are required. But, commenting either one of them -
                                       ;;-makes greenish color fail to apply
                                       (symbol (+vertico-transform-functions . +vertico-greenify-read-only-buffers))
                                       (buffer (+vertico-transform-functions . +vertico-greenify-read-only-buffers))))
      ;;Below makes vertico not mess with the order of items in the list given to completing-read
      (vertico-sort-function nil))
  (completing-read "Switch to Buffer: " (let* ((buffer-names (anup/remove-matching-items (mapcar #'buffer-name (buffer-list (selected-frame)))
                            '("\\`\\*tramp/" "\\` \\*" "\\`\\*Echo Area" "\\`\\*Minibuf" "\\`\\*helm" "\\`\\*Compile-Log" "\\`\\*straight-process" "\\`\\*quelpa-build-checkout" "tq-temp-epdfinfo" "magit-process:*" "*Kill Ring*" "*XELB-DEBUG*")))
       (current-buffer-name (car buffer-names))
       (other-buffer-names (cdr buffer-names))
       (list-of-buffers-to-switch-to (append other-buffer-names (list current-buffer-name)))
       (list-of-buffers-to-switch-to (anoop/mark-category list-of-buffers-to-switch-to 'buffer)))
  list-of-buffers-to-switch-to)))))

(global-set-key (kbd "C-x b") 'anoop/switch-to-buffer)
;;(global-set-key (kbd "C-x C-b") 'anoop/switch-to-buffer) ;;because, its bound to helm version
