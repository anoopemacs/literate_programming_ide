(setq org-babel-default-header-args
      (cons '(:noweb . "no-export")
            (assq-delete-all :noweb org-babel-default-header-args)))

(setq org-hide-emphasis-markers t)

;;(global-set-key (kbd "C-c y") (lambda () (interactive) (insert "§")))

;;(setq org-bullets-bullet-list '(" "))

(use-package org-starless
  :vc (:url "https://github.com/TonCherAmi/org-starless")
  :config
  (add-hook 'org-mode-hook #'org-starless-mode))

;;(defun org-bullets-level-char (level)
;;  "Always return ''"
;;  (string-to-char ""))

(setq org-hide-drawer-startup t)

(setq org-startup-folded "nofold")

(use-package org-variable-pitch
  :hook (org-mode . org-variable-pitch-minor-mode)
  ;;:config
  ;;(set-face-attribute 'fixed-pitch nil :family "Iosevka")
  ;;(set-face-attribute 'variable-pitch nil :family "Alegreya")
  ;;(set-face-attribute 'variable-pitch nil :family "Atkinson Hyperlegible")
  )

(use-package rainbow-mode
  :if window-system
  :hook (emacs-lisp-mode . rainbow-mode)
  :config
  (defalias 'show-css-colors-mode0 'rainbow-mode))

;;(add-to-list 'default-frame-alist '(cursor-color . "light coral"))
;;(add-to-list 'default-frame-alist '(cursor-color . "yellow"))
;;(add-to-list 'default-frame-alist '(cursor-color . "black"))
(add-to-list 'default-frame-alist '(cursor-color . "red"))
;;(set-cursor-color "black")

(setq org-fontify-whole-block-delimiter-line t)

(setq org-fontify-whole-heading-line t)

(setq org-export-with-sub-superscripts '{})

;;helper
(defun pos-at-line-col0 (l c)
  (save-excursion
    (goto-char (point-min))
    (forward-line (- l 1))
    (move-to-column c)
    (point)))
;;(numberp (pos-at-line-col0 3 0))

;;(pos-at-line-col0 3 0)

(defun find-matching-line-numbers (regexp0)
  ;;(interactive)
  (org-element-map (org-element-parse-buffer) 'src-block
    (lambda (o)
      (let* ((value0 (format "%s" (plist-get (cadr o) :value)))
             ;;(match0 (cl-search "\<\<Collect all functions\>\>" value0))
             (match1 (string-match regexp0 value0)))
        (if match1
            ;;(print line-number-where-match-occurs)
            ;;(print (numberp pos-where-src-body-begins))
            (let* (
                   (begin0 (plist-get (cadr o) :begin))
                   ;;(end0 (plist-get (cadr o) :end))
                   (begin_src-line-number (line-number-at-pos begin0))
                   ;;(end_src-line-number (line-number-at-pos end0))
                   (pos-where-src-body-begins (pos-at-line-col0 (+ begin_src-line-number 1) 0))
                   (pos-where-match-occurs (+ pos-where-src-body-begins match1))
                   (line-number-where-match-occurs (line-number-at-pos pos-where-match-occurs))
                   )
              ;;(print begin0)
              ;;(print begin_src-line-number)
              ;;(print pos-where-src-body-begins)
              ;;(print pos-where-match-occurs)
              ;;(print line-number-where-match-occurs)
              (identity line-number-where-match-occurs) ;;add to the returned map
              ))))))

(setq ref-name-regex
      (rx ":noweb-ref "       ;begins with :noweb-ref
          (group (one-or-more (not (any ":")))) ;match until next param " :"  ;;DOUBT:- will there be a bug if there are no further " :" params?
          (zero-or-more ":" (one-or-more any)))) ;the rest of the parameters if any

(defun find-definitions ()
  ;;(interactive)
  ;;since all targets must have a definition, it suffices to just iterate over ref-names
  ;;i.e. ref-names is always a superset of target-names
  (find-all-ref-names-and-their-line-numbers))
;;returns: (("Collect all functions" (16 23)) ("The Main" (36)))

(defun find-ref-name-and-its-line-number (src-block0)
  (let ((parameters0 (format "%s" (plist-get (cadr src-block0) :parameters)))
        (begin0 (plist-get (cadr src-block0) :begin)))
    (when (string-match ref-name-regex parameters0)
      (let ((ref-name (s-trim-right (match-string 1 parameters0)))
            (ref-line-number (line-number-at-pos begin0)))
        (list ref-name ref-line-number)))))

(defun find-all-ref-names-and-their-line-numbers ()
  "Returns a list of all :noweb-ref ref-name0s in the current buffer, doesnt include duplicates"
  ;;(interactive)
  (let* ((parsed-buffer (org-element-parse-buffer))
         (ref-names-with-line-number (org-element-map parsed-buffer 'src-block #'find-ref-name-and-its-line-number))
         (ref-names-deduplicated (find-all-ref-names))) ;defined elsewhere
    (mapcar (lambda (ref-name)
              (let* ((locations-for-ref-name (mapcar (lambda (tuple)
                                                       (if (string= (car tuple) ref-name)
                                                           (cadr tuple)))
                                                     ref-names-with-line-number)))
                (list ref-name (delq nil locations-for-ref-name))))
            ref-names-deduplicated)))

;;note: this also includes unused definitions
(defun find-usages ()
  ;;(interactive)
  (let* ((parsed-buffer (org-element-parse-buffer))

        (definitions-that-are-actually-used-with-line-numbers
         (delete-dups
          (apply #'append
                 (org-element-map parsed-buffer 'src-block
                   'find-all-targets-and-their-line-numbers))))

        (ref-names (find-all-ref-names))
        (target-names (find-all-target-names))
        (ref-names-unused (set-difference ref-names target-names :test #'string=))
        (ref-names-unused-with-line-numbers (mapcar (lambda (ref-name) (list ref-name nil)) ref-names-unused)))
    
    (append definitions-that-are-actually-used-with-line-numbers
            ref-names-unused-with-line-numbers)))

;; ;;note: this doensnt include unused definitions
(defun find-all-targets-and-their-line-numbers (src-block0)
  (let ((value0 (format "%s" (plist-get (cadr src-block0) :value)))
        (pos 0)
        ret)
    (while (and (< pos (length value0)) (string-match org-target-regexp value0 pos))
      (setq pos (match-end 0)) ;;I must store this before calling any other function that may modify match data, such as my next line invocation of ~match-string~
      (let ((target-name (match-string 0 value0)))
        (setq topush (list (substring target-name 2 -2) (find-matching-line-numbers target-name)))
        (push topush ret)))
    ret))

(defun find-target-names (src-block0)
  "Returns a list of all the < <target-names> > in a given src-block0, includes duplicates"
  (let ((value0 (format "%s" (plist-get (cadr src-block0) :value)))
        (pos 0)
        target-names)
    (while (string-match org-target-regexp value0 pos) ;org-target-regexp is a builtin
      (push (substring (match-string 0 value0) 2 -2) target-names)
      (setq pos (match-end 0)))
    (reverse target-names)))

(defun find-all-target-names ()
  "Returns a list of all < <target-names> > in the current buffer, doesnt include duplicates"
  ;;(interactive)
  (let* ((parsed-buffer (org-element-parse-buffer))
         (target-names (delete-duplicates (reduce #'append (org-element-map parsed-buffer 'src-block #'find-target-names)) :test #'string=)))
    target-names))

(defun delete-all-definitions ()
  ;;(interactive)
  (mapcar #'delete-one-definition (find-all-target-names)))

(defun delete-one-definition (target-name)
  ;;(interactive)
  ;;very dirty macro:- :( My first macro usage though :)
  (let ((regexp0 (eval `(rx line-start "\/" ,@target-name "\/" " is defined at"))))
    (beginning-of-buffer)               ;because delete lines works from point to end-of-file
    (delete-matching-lines regexp0)))

(defun find-ref-name (src-block0)
  "Returns the :noweb-ref ref-name0 for the given src-block0, if it exists"
  (let ((parameters0 (format "%s" (plist-get (cadr src-block0) :parameters))))
    (when (string-match ref-name-regex parameters0)
      (s-trim-right (match-string 1 parameters0)))))

(defun find-all-ref-names ()
  "Returns a list of all :noweb-ref ref-name0s in the current buffer, doesnt include duplicates"
  ;;(interactive)
  (let* ((parsed-buffer (org-element-parse-buffer))
         (ref-names (delete-duplicates (org-element-map parsed-buffer 'src-block #'find-ref-name) :test #'string=)))
    ref-names))

(defun delete-all-usages ()
  ;;(interactive)
  (mapcar #'delete-one-usage (find-all-ref-names)))

(defun delete-one-usage (ref-name)
  ;;(interactive)
  ;;very dirty macro:- :( My first macro usage though :)
  (let ((regexp0 (eval `(rx line-start "\/" ,@ref-name "\/" " is used at"))))
    (beginning-of-buffer)               ;because delete lines works from point to end-of-file
    (delete-matching-lines regexp0)))

;;Assume that buffer contains no definitions currently
;;TODO: LATER, SPECIAL CASE what happens when an src block contains both targets & also it itself defines something

(defun find-number-of-refs-for-each (src-block0)
  "Returns either 1 or 0"
  (let ((parameters0 (format "%s" (plist-get (cadr src-block0) :parameters))))
    (if (string-match ref-name-regex parameters0)
        1
      0)))

(defun blank-usages-location-for-each (src-block0)
  "Returns a list of line numbers where /Definitions/ need to be inserted for each src-block0
Misnomer: better to call it source block ending line number"
  (let* ((end0 (plist-get (cadr src-block0) :end))
         (end_src-line-number (line-number-at-pos end0)))
    (identity end_src-line-number)))

(defun insert-blank-usages ()
  "This enables us to not mess up line numbers when we run =find-definitions="
  ;;(interactive)
  (let* ((parsed-buffer (org-element-parse-buffer))
         (number-of-refs (org-element-map parsed-buffer 'src-block #'find-number-of-refs-for-each))
         (usage-insert-locations (org-element-map parsed-buffer 'src-block #'blank-usages-location-for-each))
         (ref-names (org-element-map parsed-buffer 'src-block #'find-ref-name))
         (adjusted-tuple (adjust-line-numbers2 number-of-refs usage-insert-locations)))
    (insert-blank-usages-text adjusted-tuple ref-names)))

;;helpers:
;; same repeat function
(defun running-sum (lst)
  (cl-loop with sum = 0
        for x in lst
        collect (setf sum (+ sum x))))
(running-sum '(1 2 3 4))

(defun adjust-line-numbers2 (number-of-refs usage-insert-locations)
  "Adjust because inserting lines alters the line numbers from the original cached parsed buffer.
Returns a list of tuples of form (adjusted line number, number of blank definitions)"
  (setq lines-to-skip (cons 0 (running-sum number-of-refs)))
  (cl-mapcar (lambda (num line skip)
               (list (+ line skip) num))
             number-of-refs usage-insert-locations lines-to-skip))

(defun insert-blank-usages-text (list-of-tuples ref-names)
  "Tuple of form ((l1, n1), (l2, n2))
At line l1, insert n1 definitions for target-name t1"
  (mapcar (lambda (tuple)
            (if (/= (cadr tuple) 0)
              (insert-blanks-usages-text-helper (car tuple) (pop ref-names))))
          list-of-tuples))

(defun insert-blanks-usages-text-helper (line-number ref-name)
  "At line-number, insert blank usage for given ref-name"
  (goto-line line-number)
  (insert (format "/%s/ is used at (BLANK PLACEHOLDER)\n" ref-name)))

(defun find-number-of-targets-for-each (src-block0)
  "Returns the number of < <targets> > for given src-block0 as a list"
  (let ((value0 (format "%s" (plist-get (cadr src-block0) :value)))
         (pos 0)
         target-names)
    (while (string-match org-target-regexp value0 pos)
      (push (match-string 0 value0) target-names)
      (setq pos (match-end 0)))
    (length target-names)))

(defun blank-definitions-location-for-each (src-block0)
  "Returns a list of line numbers where /Definitions/ need to be inserted for each src-block0"
  (let* (
         ;;(N (find-number-of-targets-for-each src-block0))
        (end0 (plist-get (cadr src-block0) :end))
        (end_src-line-number (line-number-at-pos end0)))
    (identity end_src-line-number)))

;; (defun find-target-names-for-each (src-block0)
;;   "Returns a list of all the < <target-names> > in a given src-block0, includes duplicates
;; Return nil if no targets exist for the src-block0"
;;   (let ((value0 (format "%s" (plist-get (cadr src-block0) :value)))
;;         (pos 0)
;;         target-names)
;;     (while (string-match org-target-regexp value0 pos)
;;       (push (substring (match-string 0 value0) 2 -2) target-names)
;;       (setq pos (match-end 0)))
;;     (if (/= (length target-names) 0)
;;         (reverse target-names)
;;       (list nil)                        ;(nil) return indicates no target-names found
;;       )))

;; (defun find-all-targets (src-block0)
;;   (let ((value0 (format "%s" (plist-get (cadr src-block0) :value)))
;;          (pos 0)
;;          target-names)
;;     (while (string-match org-target-regexp value0 pos)
;;       (push (match-string 0 value0) target-names)
;;       (setq pos (match-end 0)))
;;     target-names))

(defun insert-blank-definitions ()
  "This enables us to not mess up line numbers when we run =find-definitions="
  ;;(interactive)
  (let* ((parsed-buffer (org-element-parse-buffer))
         (number-of-targets (org-element-map parsed-buffer 'src-block #'find-number-of-targets-for-each))
         (insert-locations (org-element-map parsed-buffer 'src-block #'blank-definitions-location-for-each))
         (target-names (apply #'append (org-element-map parsed-buffer 'src-block #'find-target-names)))
         (adjusted-tuple (adjust-line-numbers number-of-targets insert-locations))
         )
    (insert-blanks adjusted-tuple target-names)))

;;helpers:
(defun running-sum (lst)
  (cl-loop with sum = 0
        for x in lst
        collect (setf sum (+ sum x))))
(running-sum '(1 2 3 4))

;; (defun get-lines-to-skip ()
;;   "After each src-block, skip = number of definitions in it + number of usages in it.
;; Lines to skip = a cumulative sum of the above"
;;   (interactive)
;;   (let* ((parsed-buffer (org-element-parse-buffer))
;;          (number-of-refs (org-element-map parsed-buffer 'src-block #'find-number-of-refs-for-each))
;;          (number-of-targets (org-element-map parsed-buffer 'src-block #'find-number-of-targets-for-each)) ;defined elsewhere
;;          )
;;     (running-sum
;;      (cl-mapcar (lambda (r1 t1) (+ r1 t1))
;;                 number-of-refs number-of-targets))))


(defun adjust-line-numbers (number-of-targets insert-locations)
  "Adjust because inserting lines alters the line numbers from the original cached parsed buffer.
Returns a list of tuples of form (adjusted line number, number of blank definitions)"
  ;;the very first definition needs to skip 0 lines due to interference from 0 previous-
  ;;definition insertions
  (setq lines-to-skip (cons 0 (running-sum number-of-targets)))
  (cl-mapcar (lambda (num line skip)
               (list (+ line skip) num))
             number-of-targets insert-locations lines-to-skip))

(defun insert-blanks (list-of-tuples target-names)
  "Tuple of form ((l1, n1), (l2, n2))
At line l1, insert n1 definitions for target-name t1"
  (mapcar (lambda (tuple)
            (dotimes (skip (cadr tuple))
              (insert-blanks-helper (+ (car tuple) skip) (pop target-names))))
          list-of-tuples))

(defun insert-blanks-helper (line-number target-name)
  "At line-number, insert blank definition for given target-name"
  (goto-line line-number)
  (insert (format "/%s/ is defined at (BLANK PLACEHOLDER)\n" target-name)))

(defun insert-definitions ()
  ;;(interactive)
  (mapcar (lambda (tuple)
            (let* ((ref-name (car tuple))
                   (line-numbers (cadr tuple))
                   (line-numbers-links (linkify-line-numbers line-numbers)))
              (mark-whole-buffer)
              (replace-regexp-in-entire-buffer
               (eval `(rx line-start "\/" ,@ref-name "\/" " is defined at (BLANK PLACEHOLDER)\n"))
               (format "/%s/ is defined at %s\n" ref-name line-numbers-links))))
          (find-definitions)))

(defun linkify-line-numbers (line-numbers)
  (mapconcat #'identity
             (mapcar (lambda (line-number)
                       (format "[[file:::%s][%s]]" line-number line-number))
                     line-numbers)
             ", "))

(defun replace-regexp-in-entire-buffer (regexp0 replacement0)
  ;;(interactive)
  (beginning-of-buffer)
  (while (re-search-forward regexp0 nil t)
    (replace-match replacement0)))

(defun insert-usages ()
  ;;(interactive)
  (mapcar (lambda (tuple)
            (let* ((target-name (car tuple))
                   (line-numbers (cadr tuple))
                   (line-numbers-links (linkify-line-numbers line-numbers)))
              (mark-whole-buffer)
              (replace-regexp-in-entire-buffer
               (eval `(rx line-start "\/" ,@target-name "\/" " is used at (BLANK PLACEHOLDER)\n"))
               (format "/%s/ is used at %s\n" target-name line-numbers-links))))
          (find-usages)))

;; ORDER OF OPERATIONS IS IMPORTANT
;; 1. delete previously inserted definitions, and usages
;; 2. insert-blank-usages
;; 3. insert-blank-definitons
;; 4. insert-definitions: this regexp replaces blank definitions with real definitions
;; 4. insert-usages: this regexp replaces blank usages with real usages

(defun insert-usages-and-definitions ()
  ;;(interactive)
  (delete-all-definitions) (delete-all-usages) ;1
  (insert-blank-usages)                        ;2
  (insert-blank-definitions)                   ;3
  (insert-definitions) (insert-usages)         ;4
  )

(defun insert-literate-links0 ()
  (interactive)
  (display-line-numbers-mode)
  (insert-usages-and-definitions))

(defun anup/current-column-helper (point)
  (save-excursion
    (goto-char point)
    (current-column)))
(defun anup/current-column ()
  "Return column number at POINT."
  (save-excursion
    (goto-char (point))
    (current-column)))
(defun anup/length-of-current-line ()
  "Return length of the line on which the POINT lies"
  (- (line-end-position)
     (line-beginning-position)))

(defun anup/looking-at-literate-usage? ()
  "Return < <Some Target Name> > if cursor at some org target; Return nil otherwise"
  (let ((original-cursor-position (point))
        (closing-point0 (re-search-forward ">>" (line-end-position) t))
        (beginning-point0 (re-search-backward "<<" (line-beginning-position) t)))
    (goto-char original-cursor-position)
    (if (and closing-point0
             beginning-point0
             (< original-cursor-position closing-point0)
             (> original-cursor-position beginning-point0))
        (buffer-substring-no-properties beginning-point0 closing-point0)
      nil)))

(defalias 'anup/looking-at-literate-target? 'anup/looking-at-literate-usage?)

(defun anup/looking-at-literate-definition? ()
  "Return ':noweb-ref My Example Definition' if cursor at some named org src block; Return nil otherwise"
  ;;(interactive)
  (let ((original-cursor-position (point)))
    ;;move cursor to end of line was causing bugs when:-
    ;;- either refname wasnt quoted
    ;;- or src header contained more arguments after \:noweb-ref
    ;;(end-of-line)
    (end-of-line)
    (if (looking-back ref-name-regex (- (point) (anup/current-column)))
        (progn (goto-char original-cursor-position)
               (concat ":noweb-ref " (s-trim-right (match-string-no-properties 1))))
      (progn (goto-char original-cursor-position) nil))))

(defun anup/replace-in-string (what with in)
  (replace-regexp-in-string (regexp-quote what) with in nil 'literal))

(defun anup/goto-definition ()
  (interactive)
  "Move point to the location where this literate named block is being utilized"
  (if-let* ((target-name (anup/looking-at-literate-usage?))
            ;;trim angle brackets
            (_ref-name (substring target-name 2 -2))
            ;;spaces inside double quotes arent matched properly unless they are escaped
            (_ref-name (string-replace " " "\\ " _ref-name))
            ;;llly question mark also needs escaping
            (_ref-name (string-replace "?" "\\?" _ref-name))
            (definition-name (concat ":noweb-ref \"" _ref-name "\""))

            ;;;;;;disabled because it is OR is not working in counsel-ag
            ;;;;;;because sometimes noweb-ref names are written without double quotes in orgfiles
            ;;;;(definition-name-remove-double-quotes (anup/replace-in-string "\"" "" definition-name))
            ;;;;
            ;;;;;;because sometimes refnames may contain question marks
            ;;;;(definition-name-escape-question-mark (anup/replace-in-string "?" "\\?" definition-name))
            ;;;;
            ;;;;;;combine all the above cases into a query that can be sent into grep
            ;;;;(query (concat definition-name "|"
            ;;;;               definition-name-remove-double-quotes "|"
            ;;;;               definition-name-escape-question-mark))
            
            (file-name (file-name-nondirectory buffer-file-name))
            (directory-name (file-name-directory buffer-file-name)))
      ;;(helm-ag-this-file query)
      ;;(counsel-ag definition-name nil (format "-C 3 --norecurse --noheading --case-sensitive --file-search-regex %s$" file-name) "")

      ;; --no-heading isnt working
      ;;(consult-ripgrep (list (shell-quote-argument buffer-file-name))
      ;;                 (format "%s -- --case-sensitive --no-heading" definition-name))
      
      ;;The first argument limits the search to just the current file
      (consult-ripgrep (list (shell-quote-argument buffer-file-name)) (format "%s -- --case-sensitive" definition-name))))

(defun anup/goto-usage ()
  (interactive)
  "Move point to the location where this literate named block is defined"
  (if-let* ((definition-name (anup/looking-at-literate-definition?))
            (_ref-name (substring definition-name 11))
;;strip double quotes
(_ref-name (anup/replace-in-string "\""
                                   ""
                                   _ref-name))
;;escape spaces
(_ref-name (string-replace " " "\\ " _ref-name))
;;escape question mark
(_ref-name (string-replace "?" "\\?" _ref-name))
(target-name (concat "\<\<" _ref-name "\>\>")))

      ;;decided against helm-ag
      ;;(helm-ag-this-file target-name)

      ;;bug:- --nofilename isnt working
      ;;(counsel-ag target-name nil (format "-C 3 --norecurse --noheading --case-sensitive --file-search-regex %s$" file-name) "")

      ;; --no-line-number isnt working in the format string due to some bug
      (consult-ripgrep (list (shell-quote-argument buffer-file-name)) (format "%s -- --case-sensitive" target-name))))

(defun org-tree-to-indirect-buffer2 ()
  "Like org-tree-to-indirect-buffer, but always creates a new indirect buffer, wont close previous ones."
  (interactive)
  (let ((current-prefix-arg '(4)))
    (call-interactively 'org-tree-to-indirect-buffer)))

(setq org-indirect-buffer-display 'current-window)

(put 'org-tree-to-indirect-buffer 'disabled "Use org-tree-to-indirect-buffer2 instead0")
;;below was by default bound to the above disabled command, let me rebind it to improved version
(keymap-set org-mode-map "<remap> <org-tree-to-indirect-buffer>" 'org-tree-to-indirect-buffer2)

(setq xref-show-xrefs-function #'consult-xref
      xref-show-definitions-function #'consult-xref)

(use-package dumb-jump
  :config
  (add-hook 'xref-backend-functions #'dumb-jump-xref-activate)
  (setq dumb-jump-force-searcher 'ag)
  (defun dumb-jump-get-language-in-org ()
  "In org mode, always lie to dumbjump machine that you are NOT inside any src block"
  "org")
(let ((clojure-rules (cl-remove-if-not (lambda (rule)
                                          (string= (plist-get rule :language) "clojure"))
                                        dumb-jump-find-rules)))
    (setq dumb-jump-find-rules
          (append dumb-jump-find-rules
                  (mapcar (lambda (rule)
                            (plist-put (copy-tree rule) :language "org"))
                          clojure-rules)))))

(defun anoop/routed/goto ()
  (interactive)

  ;;push current location to xref history stack, so that xref-go-back use it to come back
  (xref-push-marker-stack)
  
  (cond ((anup/looking-at-literate-usage?) (call-interactively 'anup/goto-definition))
        ((anup/looking-at-literate-definition?) (call-interactively 'anup/goto-usage))
        (t (call-interactively 'xref-find-definitions))))

;;+(setq shell-command-switch "-ic")+

(setenv "BASH_ENV" (expand-file-name "~/.bashrc"))

(use-package org-sticky-header
  :hook
  ((org-mode . org-sticky-header-mode))
  :config
  (defun org-sticky-header--fetch-stickyline ()
  "Return string of Org heading or outline path for display in header line."
  (org-with-wide-buffer
   (goto-char (window-start))
   (if (org-before-first-heading-p)
       ""
       ;; No non-header lines above top displayed header
       (if (or org-sticky-header-always-show-header
               (not (org-at-heading-p)))
           (progn
             ;; Header should be shown
             (when (fboundp 'org-inlinetask-in-task-p)
               ;; Skip inline tasks
               (while (and (org-back-to-heading)
                           (org-inlinetask-in-task-p))
                 (forward-line -1)))
             (cond
              ;; TODO: Update minimum Emacs version and use `pcase'.
              ((null org-sticky-header-full-path)
               (concat (org-sticky-header--get-prefix)
                       (org-sticky-header--heading-string)))
              ((eq org-sticky-header-full-path 'full)
               (concat (org-sticky-header--get-prefix)
                       (mapconcat 'identity
                                  (nreverse
                                   (save-excursion
                                     (cl-loop collect (org-sticky-header--heading-string)
                                              while (org-up-heading-safe))))
                                  org-sticky-header-outline-path-separator)))
              ((eq org-sticky-header-full-path 'reversed)
               (let ((s (concat
                         (org-sticky-header--get-prefix)
                         (mapconcat 'identity
                                    (save-excursion
                                      (cl-loop collect (org-sticky-header--heading-string)
                                               while (org-up-heading-safe)))
                                    org-sticky-header-outline-path-reversed-separator))))
                 (if (> (string-width s) (window-width))
                     (concat (substring s 0 (- (window-width) 2))
                             "..")
                   s)))
              (t "")))
         
         ""))))
  (setq org-sticky-header-full-path 'reversed)
  (setq org-sticky-header-always-show-header nil)
(setq org-sticky-header-outline-path-reversed-separator " ⏪ ")
(setq org-sticky-header-heading-star " "))
