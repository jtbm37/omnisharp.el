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
       (cond ((= (length quickfixes) 1)
              (omnisharp-go-to-file-line-and-column (elt quickfixes 0)))
             ((> (length quickfixes) 1)
              (omnisharp--helm-got-usages quickfixes))
             (t (message "No usages found")))))))

  (defun omnisharp--helm-jump-to-candidate (json-result)
    (omnisharp-go-to-file-line-and-column json-result)
    (helm-highlight-current-line nil nil nil nil t))

  (defun omnisharp-find-symbols ()
    "Find symbols"
    (interactive)
    (ivy-read "find symbols:"
              #'omnisharp--find-symbols-candidates
              :dynamic-collection t
              :action (lambda (x)
                        (message "%s" (get-text-property 0 'property x))
                        (omnisharp-go-to-file-line-and-column (get-text-property 0 'property x)))
              :caller 'omnisharp-find-symbols
              ))

  (defun omnisharp--find-symbols-candidates (&optional input ok we)
    (if (<  (length input) 4)
        '("More chars...")
      (let (candidates)
        (omnisharp--send-command-to-server-sync
         "findsymbols"
         (cons `(Filter . ,input)
               (omnisharp--get-request-object))
         (-lambda ((&alist 'QuickFixes quickfixes))
           (setq candidates
                 (-map 'omnisharp--avy-find-symbols-transform-candidate
                       quickfixes))))
        candidates)))

  (defun omnisharp--avy-find-symbols-transform-candidate (candidate)
    "Convert a quickfix entry into ivy output"
    (propertize (format "%s: %s" (file-relative-name (cdr (assoc 'FileName candidate)) (projectile-project-root)) (cdr (assoc 'Text candidate))) 'property candidate))

;;; Helm list projects
(defun omnisharp-helm-projects ()
  (interactive)
  (helm :sources (helm-build-async-source "Omnisharp - Projects"
                   :action 'omnisharp--helm-jump-to-file
                   :matchplugin nil
                   :candidates-process 'omnisharp--helm-projects-candidates)
        :truncate-lines t))

(defun omnisharp--helm-jump-to-file (candidate)
  (find-file (cdr candidate)))

(defun omnisharp--helm-projects-candidates ()
  (let (candidates)
    (omnisharp--send-command-to-server-sync
     "projects"
     (->> (omnisharp--get-request-object)
          (cons '(ExcludeSourceFiles . t)))
     (lambda (data)
       (let ((projects (cdr (assoc 'Projects (cdr (assoc 'MsBuild data))))))
         (setq candidates (-map '(lambda (candidate) (cons (format "%s" (cdr (assoc 'AssemblyName candidate))) (list (assoc 'Path candidate)))) projects))
         ))
     nil)
    candidates))

(defun omnisharp-helm-current-file-members ()
  "Show a list of all members in the current file, and jump to the
selected member. With prefix argument, use another window."
  (interactive)
    (omnisharp--send-command-to-server
     "currentfilemembersasflat"
     (omnisharp--get-request-object)
     (lambda (quickfixes)
       (setq candidates (-map (lambda (q)
                                (cons  (cdr  (assoc 'Text q)) q)) (append quickfixes nil)))
       (helm :sources (helm-make-source "OmniSharp - File Members" 'helm-source-sync
                        :action 'omnisharp-go-to-file-line-and-column
                        :candidates candidates)
             ))))

(provide 'omnisharp-helm-integration)
