;;; repom-github.el --- Helm source and actions for GitHub repos -*- lexical-binding: t -*-

;; Copyright (C) 2018 Akira Komamura

;; Author: Akira Komamura <akira.komamura@gmail.com>
;; Version: 1.0-pre
;; Package-Requires: ((emacs "25.1") (ghub "2.0"))
;; Keywords: vc tools
;; URL: https://github.com/akirak/repom.el

;; This file is not part of GNU Emacs.

;;; License:

;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; GitHub layer for repom.

;;; Code:

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

(defcustom repom-github-user-repos-sort 'updated
  "How to sort the user repositories."
  :type '(choice (const created)
                 (const updated)
                 (const pushed)
                 (const full_name))
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

(repom--def-object-api github-repo-browse-html (html_url)
  "Browse the HTML page of the repository."
  (repom-github--browse-url html_url))

(repom--def-object-api github-repo-clone-and-edit (clone_url name)
  "Clone the repository and edit the copy."
  (repom-git-clone-for-editing clone_url name))

(repom--def-object-api github-repo-clone-and-view (clone_url name)
  "Clone the repository and view the copy."
  (repom-git-clone-for-viewing clone_url name))

(repom--def-object-api github-repo-search-code (full_name)
  "Search code in the repository."
  (repom-github--browse-repo-code-search full_name))

(repom--def-object-api github-repo-search-issues (full_name)
  "Search issues in the repository."
  (repom-github--browse-repo-issues-search full_name))

(defun repom-github--browse-repo-code-search (repo &optional query)
  "Browse a search result page for code in REPO with QUERY."
  (let ((query (or query
                   (read-string (format "Search code in %s: " repo)))))
    (repom-github--browse-url
     (format repom-github-repo-code-search-url-template
             repo (repom--escape-query query)))))

(defun repom-github--browse-repo-issues-search (repo &optional query)
  "Browse a search result page for issues in REPO with QUERY."
  (let ((query (or query
                   (read-string (format "Search issues in %s: " repo)))))
    (repom-github--browse-url
     (format repom-github-repo-issue-search-url-template
             repo (repom--escape-query query "is:open")))))

;;;; Retrieve information from the server

(defun repom-github--list-user-repos (&optional update)
  "Return a list of user repositories.

If UPDATE is non-nil, first clear the cache."
  (repom--with-cache-variable repom-github-user-repos-cache
    update
    (ghub-request "GET" "/user/repos"
                  `((visibility . "all")
                    (sort . ,(symbol-name repom-github-user-repos-sort)))
                  :unpaginate t
                  :auth 'repom)))

(defun repom-github--list-starred-repos (&optional update)
  "Return a list of repositories starred by the user.

If UPDATE is non-nil, first clear the cache."
  (repom--with-cache-variable repom-github-starred-repos-cache
    update
    (ghub-request "GET" "/user/starred" nil
                  :unpaginate t
                  :auth 'repom)))

(defun repom-github--search-repos (query)
  "Search repositories matching QUERY."
  ;; TODO: Allow specify the sorting method
  (ghub-request "GET" "/search/repositories"
                `((q . ,query))
                :auth 'repom))

;;;; Maintenance
;;;###autoload
(defun repom-github-clear-cache ()
  "Clear the cache."
  (interactive)
  (setq repom-github-user-repos-cache nil
        repom-github-starred-repos-cache nil))

;;;###autoload
(defun repom-github-fetch-repo-lists ()
  "Fetch lists of GitHub repositories and store them in the memory."
  (repom-github--list-user-repos t)
  (repom-github--list-starred-repos t))

(provide 'repom-github)
;;; repom-github.el ends here
