;;; helm-repom.el --- Helm for repository management -*- lexical-binding: t -*-

;; Copyright (C) 2018 Akira Komamura

;; Author: Akira Komamura <akira.komamura@gmail.com>
;; Version: 1.0-pre
;; Package-Requires: ((emacs "25.1") (helm "2.7.0") (helm-projectile "0.14.0") (all-the-icons "3.1"))
;; Keywords: vc
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

;; This is a Helm interface to features in repom.

;;; Code:

(require 'helm)
(require 'all-the-icons nil t)
(require 'helm-projectile)

(require 'repom)
(require 'repom-github)
(require 'repom-git)

(defgroup helm-repom nil
  "Helm interface."
  :group 'repom
  :prefix "helm-repom")

;;;; Custom variables
(defcustom helm-repom-sources
  '(helm-source-projectile-projects
    helm-repom-github-user-repos-source
    helm-repom-github-starred-repos-source
    ;; TODO: Add dummy source
    ;; helm-repom-git-source-dummy-url
    )
  "List of sources for `helm-repom'."
  :type '(repeat symbol)
  :group 'helm-repom)

;;;;; Icons
(defcustom helm-repom-use-all-the-icons
  (featurep 'all-the-icons)
  "Use icons from all-the-icons.el."
  :type 'boolean
  :group 'helm-repom)

(defcustom helm-repom-github-fork-indicator
  (when helm-repom-use-all-the-icons
    (concat " " (all-the-icons-octicon "repo-forked")))
  "Indicator to show a forked repository."
  :type 'string
  :group 'helm-repom)

(defcustom helm-repom-github-private-indicator
  (when helm-repom-use-all-the-icons
    (concat " " (all-the-icons-octicon "lock")))
  "Indicator to show a forked repository."
  :type 'string
  :group 'helm-repom)

;;;; Faces
;; TODO: Add a face for the name of a GitHub repo
;; TODO: Add a face for the name of a local repo
;; TODO: Add a face for archived repos

;;;; Actions
(defcustom helm-repom-github-repo-actions
  (helm-make-actions
   "Browse the web page"
   (pcase-lambda ((map html_url))
     (repom-github--browse-url html_url))
   "Edit locally"
   (pcase-lambda ((map clone_url name))
     (repom-git-clone-for-editing clone_url name))
   "View locally"
   (pcase-lambda ((map clone_url name))
     (repom-git-clone-for-viewing clone_url name))
   "Search code in the repository"
   (pcase-lambda ((map full_name))
     (repom-github--browse-repo-code-search full_name))
   "Search issues in the repository"
   (pcase-lambda ((map full_name))
     (repom-github--browse-repo-issues-search full_name)))
  "Alist of actions on a GitHub repository."
  :type '(alist :key-type string
                :value-type function)
  :group 'helm-repom
  :group 'repom-github)

;;;; Sources

;;;;; GitHub repos
(defclass helm-repom-github-repos-source-class (helm-source-sync)
  ((action :initform 'helm-repom-github-repo-actions)
   (candidate-transformer :initform #'helm-repom--github-repo-candidates)))

(defvar helm-repom-github-user-repos-source
  (helm-make-source "GitHub repos owned by the user"
      'helm-repom-github-repos-source-class
    :candidates #'repom-github--list-user-repos
    :candidate-transformer
    (lambda (candidates)
      (helm-repom--github-repo-candidates candidates :short-name t))))

(defvar helm-repom-github-starred-repos-source
  (helm-make-source "GitHub repos starred by the user"
      'helm-repom-github-repos-source-class
    :candidates #'repom-github--list-starred-repos))

;;;;; Utilities
(defun helm-repom--github-repo-candidates (records &rest options)
  "Format repository candidates from RECORDS with OPTIONS."
  (mapcar (lambda (data)
            (cons (apply #'helm-repom--github-repo-record data options)
                  data))
          records))

(cl-defun helm-repom--github-repo-record (data &key short-name)
  "Format fields of a repository candidate from DATA."
  (let-alist data
    (format "%-30s %-15s %s %s"
            ;; TODO: Use icons from all-the-icons.el
            (concat (if short-name .name .full_name)
                    (and .private helm-repom-github-private-indicator)
                    (and .fork helm-repom-github-fork-indicator))
            (or .language "")
            (or .description "")
            (if-let ((fields (cl-delete nil
                                        (list
                                         (when .private "private")
                                         (when .fork "fork")
                                         (when .archived "archived")))))
                (format "[%s]" (string-join fields ","))
              ""))))

;;;; Main entry point

;;;###autoload
(defun helm-repom (arg)
  "Run an operation on a repository via Helm.

If a universal prefix ARG is given, clear the repository cache."
  (interactive "P")
  (when arg
    (repom-github-clear-cache))
  (helm :prompt "Git repo: "
        :sources helm-repom-sources))

(provide 'helm-repom)
;;; helm-repom.el ends here
