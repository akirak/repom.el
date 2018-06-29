;;; helm-repos-github.el --- Helm source and actions for GitHub repos -*- lexical-binding: t -*-

(require 'memoize)
(require 'helm)
(require 'all-the-icons nil t)

;;;; Variables
(defvar helm-repos-github-token-scopes nil
  "Needed to get a list of GitHub repos.")

(defvar helm-repos-github-user-repos-cache nil)
(defvar helm-repos-github-starred-repos-cache nil)

;;;; Custom variables
(defcustom helm-repos-github-browser nil
  "Browser used to browse a web page on GitHub.")

(defcustom helm-repos-github-repo-actions
  (helm-make-actions
   "Browse the web page"
   (lambda (data) (let-alist data
                    (helm-repos-github--browse-url .html_url)))
   "Clone and open in a new frame"
   (lambda (data) (let-alist data
                    (helm-repos-github--browse-url .html_url)))
   "Fork"
   (lambda (data) (let-alist data
                    (helm-repos-github--browse-url .html_url)))
   "Search code"
   (lambda (data) (let-alist data
                    (helm-repos-github--browse-url .html_url)))
   "Search issues"
   (lambda (data) (let-alist data
                    (helm-repos-github--browse-url .html_url))))
  "Alist of actions on a GitHub repository."
  :type '(alist :key-type string
                :value-type function)
  :group 'helm-repos)

;;;;; Icons
(defcustom helm-repos-github-use-all-the-icons
  (featurep 'all-the-icons)
  "Use icons from all-the-icons.el."
  :type 'boolean
  :group 'helm-repos)

(defcustom helm-repos-github-fork-symbol
  (when helm-repos-github-use-all-the-icons
    (concat " " (all-the-icons-octicon "repo-forked")))
  "Indicator to show a forked repository."
  :type 'string
  :group 'helm-repos)

(defcustom helm-repos-github-private-symbol
  (when helm-repos-github-use-all-the-icons
    (concat " " (all-the-icons-octicon "lock")))
  "Indicator to show a forked repository."
  :type 'string
  :group 'helm-repos)

;;;; Faces
;; TODO: Add a face for archived repos

;;;; Remote repos

(defclass helm-repos-github-source-user-repos-class (helm-source-in-buffer)
  ((candidates
    :initform
    (lambda () (helm-repos-github--repo-candidates
                (helm-repos-github--user-repos)
                :short-name t)))))

(defvar helm-repos-github-source-user-repos
  (helm-make-source "GitHub repos owned by the user"
      helm-repos-github-source-user-repos-class
    :action 'helm-repos-github-repo-actions))

(defun helm-repos-github--user-repos ()
  (or helm-repos-github-user-repos-cache
      (setq helm-repos-github-user-repos-cache
            (ghub-request "GET" "/user/repos"
                          '((visibility . "all"))
                          :unpaginate t
                          :auth 'helm-repos))))

(defclass helm-repos-github-source-starred-repos-class (helm-source-in-buffer)
  ((candidates
    :initform
    (lambda () (helm-repos-github--repo-candidates
                (helm-repos-github--starred-repos))))
   (action :initform 'helm-repos-github-repo-actions)))

(defvar helm-repos-github-source-starred-repos
  (helm-make-source "GitHub repos starred by the user"
      helm-repos-github-source-starred-repos-class
    :action 'helm-repos-github-repo-actions))

(defun helm-repos-github--starred-repos ()
  (or helm-repos-github-starred-repos-cache
      (setq helm-repos-github-starred-repos-cache
            (ghub-request "GET" "/user/starred" nil
                          :unpaginate t
                          :auth 'helm-repos))))

(defun helm-repos-github--repo-candidates (records &rest options)
  (mapcar (lambda (data)
            (cons (apply #'helm-repos-github--repo-record data options)
                  data))
          records))

(cl-defun helm-repos-github--repo-record (data &key short-name)
  "Format fields of a repository candidate from DATA."
  (let-alist data
    (format "%-30s %-15s %s %s"
            ;; TODO: Use icons from all-the-icons.el
            (concat (if short-name .name .full_name)
                    (and .private helm-repos-github-private-symbol)
                    (and .fork helm-repos-github-fork-symbol))
            (or .language "")
            (or .description "")
            (if-let ((fields (cl-delete nil
                                        (list
                                         (when .private "private")
                                         (when .fork "fork")
                                         (when .archived "archived")))))
                (format "[%s]" (string-join fields ","))
              ""))))

(defun helm-repos-github-clear-cache ()
  (interactive)
  (setq helm-repos-github-user-repos-cache nil
        helm-repos-github-starred-repos-cache nil))

(defun helm-repos-github--browse-url (url)
  (cl-typecase helm-repos-github-browser
    (null (browse-url url))))

(provide 'helm-repos-github)
;;; helm-repos-github.el ends here
