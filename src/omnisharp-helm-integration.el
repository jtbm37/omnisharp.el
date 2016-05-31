;; -*- mode: Emacs-Lisp; lexical-binding: t; -*-
(defcustom omnisharp-find-symbols-filter-async nil
  "Set to t if you want to let the server filter
  the symbols. This must be set to t if the project has
a lot of symbols which could make emacs freeze for several
seconds."
  :group 'omnisharp
  :type '(choice (const :tag "Yes" t)
                 (const :tag "No" nil)))

(when (require 'helm-grep nil 'noerror)
  ;;; Helm usages
  (defvar omnisharp-helm-usage-candidates nil)

  (defun omnisharp--helm-usage-transform-candidate (candidate)
    "Convert a quickfix entry into helm output"
    (cons
     (format "%s(%s): %s"
             (propertize (file-name-nondirectory
                          (cdr (assoc 'FileName candidate)))
                         'face 'helm-grep-file)
             (propertize (number-to-string (cdr (assoc 'Line candidate)))
                         'face 'helm-grep-lineno)
             (cdr (assoc 'Text candidate)))
     candidate))
  
  (defun omnisharp--helm-got-usages (quickfixes)
    (setq omnisharp-helm-usage-candidates (mapcar 'omnisharp--helm-usage-transform-candidate quickfixes))
    (helm :sources (helm-make-source "Omnisharp - Symbol Usages" 'helm-source-sync
                                     :candidates omnisharp-helm-usage-candidates
                                     :action 'omnisharp--helm-jump-to-candidate)
          :truncate-lines t
          :buffer omnisharp--find-usages-buffer-name))

  (defun omnisharp-helm-find-usages ()
    "Find usages for the symbol under point using Helm"
    (interactive)
    (message "Helm Finding usages...")
    (omnisharp--send-command-to-server
     "findusages"
     (->>
      (omnisharp--get-request-object)
      (cons '(ExcludeDefinition . "true")))
     (-lambda ((&alist 'QuickFixes quickfixes))
       (if (= (length quickfixes) 1)
           (omnisharp-go-to-file-line-and-column (elt quickfixes 0))
         (omnisharp--helm-got-usages quickfixes)))))

  (defun omnisharp--helm-jump-to-candidate (json-result)
    (omnisharp-go-to-file-line-and-column json-result)
    (helm-highlight-current-line nil nil nil nil t))

  ;;; Helm find symbols
  (defun omnisharp-helm-find-symbols ()
    (interactive)
    (helm :sources (helm-build-async-source "Omnisharp - Find Symbols"
                                     :action 'omnisharp--helm-jump-to-candidate
                                     :matchplugin nil
                                     :match '((lambda (candidate) (string-match-p
                                                                   helm-pattern
                                                                   (nth 1 (split-string
                                                                           candidate ":" t)))))
                                     :candidates-process 'omnisharp--helm-find-symbols-candidates)
          :input-idle-delay 0.5
          :buffer "*Omnisharp Symbols*"
          :truncate-lines t))

  (defun omnisharp--helm-find-symbols-candidates ()
    (when (or (and omnisharp-find-symbols-filter-async (not (string= helm-input "")))
              (not omnisharp-find-symbols-filter-async))
    (let (candidates)
      (message "fetching new candidates %s" helm-input)
      (omnisharp--send-command-to-server-sync
       "findsymbols"
       (cons `(Filter . ,helm-input)
             (omnisharp--get-request-object))
       (-lambda ((&alist 'QuickFixes quickfixes))
                (setq candidates
                      (-map 'omnisharp--helm-find-symbols-transform-candidate
                            quickfixes))))
      candidates)))

  (defun omnisharp--helm-find-symbols-transform-candidate (candidate)
    "Convert a quickfix entry into helm output"
    (cons
     (format "%s : %s"
             (propertize (cdr (assoc 'FileName candidate))
                         'face 'helm-grep-file)
             (nth 0 (split-string (cdr (assoc 'Text candidate)) "(")))
     candidate)))

(provide 'omnisharp-helm-integration)
