;;; repom-github.el --- Helm source and actions for GitHub repos -*- lexical-binding: t -*-

(require 'ghub)
(require 'repom)

(declare-function 'url-encode-url "url-util")

(defgroup repom-github nil
  "GitHub repositories"
  :group 'repom)

;;;; Variables
;;;;; Constants
(defconst repom-github-repo-code-search-url-template
  "https://github.com/%s/search?q=%s")

(defconst repom-github-repo-issue-search-url-template
  "https://github.com/%s/issues?q=%s")

;;;;; Custom variables
(defcustom repom-github-browser nil
  "Browser used to browse a web page on GitHub.

This should be a function that takes a URL as the argument."
  :type '(choice function (const "Default" nil))
  :group 'repom-github)

;;;;; Other variables
(defvar repom-github-token-scopes nil
  "Needed to get a list of GitHub repos.")

(defvar repom-github-user-repos-cache nil)
(defvar repom-github-starred-repos-cache nil)

;;;; Utility functions
(defun repom-github--browse-url (url)
  "Browse a URL using a browser function specific to GitHub."
  (let ((browser repom-github-browser))
    (cl-etypecase browser
      (null (browse-url url))
      (function (funcall browser url))
      (symbol (funcall browser url)))))

;;;; Operations on a repository

(defun repom-github--browse-repo-code-search (repo &optional query)
  "Browse a search result page for code in a repository."
  (let ((query (or query
                   (read-string (format "Search code in %s: " repo)))))
    (repom-github--browse-url
     (format repom-github-repo-code-search-url-template
             repo (repom--escape-query query)))))

(defun repom-github--browse-repo-issues-search (repo &optional query)
  "Browse a search result page for issues in a repository."
  (let ((query (or query
                   (read-string (format "Search issues in %s: " repo)))))
    (repom-github--browse-url
     (format repom-github-repo-issue-search-url-template
             repo (repom--escape-query query "is:open")))))

;;;; Retrieve information from the server

(defun repom-github--list-user-repos ()
  "Return a list of user repositories."
  (repom--with-cache-variable repom-github-user-repos-cache
    (ghub-request "GET" "/user/repos"
                  '((visibility . "all"))
                  :unpaginate t
                  :auth 'repom)))

(defun repom-github--list-starred-repos ()
  "Return a list of repositories starred by the user."
  (repom--with-cache-variable repom-github-starred-repos-cache
    (ghub-request "GET" "/user/starred" nil
                  :unpaginate t
                  :auth 'repom)))

;;;; Maintenance
(defun repom-github-clear-cache ()
  "Clear the cache."
  (interactive)
  (setq repom-github-user-repos-cache nil
        repom-github-starred-repos-cache nil))

(provide 'repom-github)
;;; repom-github.el ends here
