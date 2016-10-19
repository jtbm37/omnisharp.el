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
            :action 'omnisharp--avy-go-to-file-and-column
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

(defun omnisharp--avy-go-to-file-and-column (x)
  "Parses a string with extra properties and passes it to
`omnisharp-go-to-file-line-and-column'"
  (omnisharp-go-to-file-line-and-column (get-text-property 0 'property x)))

(defun omnisharp-projects ()
  "List all the projects in solution"
  (interactive)
  (omnisharp--avy-projects))

(cl-defun omnisharp--avy-projects (&optional (default-action 1))
  "Shows list of projects. `defaction' defines the default ivy action on submit."
  (let (candidates)
    (omnisharp--send-command-to-server-sync
     "projects"
     (->> (omnisharp--get-request-object)
          (cons '(ExcludeSourceFiles . t)))
     (lambda (data)
       (let ((projects (cdr (assoc 'Projects (cdr (assoc 'MsBuild data))))))
         (setq candidates (-map '(lambda (candidate) (propertize (cdr (assoc 'AssemblyName candidate)) 'property candidate)) projects))
         )))
    (ivy-read "projects: "
              candidates
              :action `(,default-action
                        ("o" (lambda (x) (omnisharp--jump-to-file (list (assoc 'Path (get-text-property 0 'property x))))))
                        ("b" (lambda (x) (omnisharp--build-project `(FileName . ,(cdr (assoc 'Path (get-text-property 0 'property x)))))) "build"))
              :caller 'omnisharp-projects
              )))

(defun omnisharp-build-project ()
  "Builds selected project"
  (interactive)
  (omnisharp--avy-projects 2))

(defun omnisharp--jump-to-file (candidate)
  (find-file (cdr candidate)))

(defun omnisharp-current-file-members ()
  "Show a list of all members in the current file, and jump to the
selected member. With prefix argument, use another window."
  (interactive)
  (let (candidates)
    (omnisharp--send-command-to-server
     "currentfilemembersasflat"
     (omnisharp--get-request-object)
     (lambda (quickfixes)
       (setq candidates (-map (lambda (q)
                                (propertize (cdr (assoc 'Text q)) 'property q)) quickfixes))))
    (ivy-read "file members:" candidates
              :action 'omnisharp--avy-go-to-file-and-column
              :caller 'omnisharp-current-file-members)))

(provide 'omnisharp-helm-integration)
