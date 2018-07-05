;;; repom-git.el --- Git operations for repository management -*- lexical-binding: t -*-

;; Copyright (C) 2018 Akira Komamura

;; Author: Akira Komamura <akira.komamura@gmail.com>
;; Version: 1.0-pre
;; Package-Requires: ((emacs "25.1") (f "0.19"))
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

;; This library provides wrappers for Git commands.

;;; Code:


(require 'repom)
(require 'f)
(require 'magit)

(defgroup repom-git nil
  "Git"
  :prefix "repom-git-"
  :group 'repom)

;;;; Custom variables
(defconst repom-git-default-description
  "Unnamed repository; edit this file 'description' to name the repository.")

;;;; Clone repositories
(cl-defun repom-git--clone-internal (url dest &optional action)
  "Clone a Git repository from a remote url."
  (message "Cloning repository from %s to %s..." url dest)
  (let* ((default-directory (f-parent dest))
         (sentinel (lambda (_proc event)
                     (pcase event
                       ("finished\n" (when action
                                       (funcall action dest)))
                       ((pred (string-prefix-p "exited abnormally"))
                        (error event))
                       ((pred (string-prefix-p "failed"))
                        (error event)))))
         (proc (start-file-process "repom-git-clone" nil "git" "clone" url dest)))
    (set-process-sentinel proc sentinel)))

(cl-defun repom-git-clone-for-viewing (url name)
  "Clone a Git repository from URL and edit it.  Use NAME as the destination."
  (if-let ((existing-dest (repom--find-local-repo-of-name name)))
      (repom--view-project existing-dest)
    (repom-git--clone-internal url
                               (repom--default-view-location name)
                               #'repom--view-project)))

(cl-defun repom-git-clone-for-editing (url name)
  "Clone a Git repository from URL and edit it.  Use NAME as the destination."
  (if-let ((existing-dest (repom--find-local-repo-of-name name)))
      (repom--edit-project existing-dest)
    (repom-git--clone-internal url
                               (repom--default-edit-location name)
                               #'repom--edit-project)))

;;;; Repository statuses
(cl-defun repom-git-statuses (fields &optional repos no-save)
  "Get statuses of local Git repositories.

FIELDS is a list of symbols to specify which types of check to run.

`dirty'

Optionally, FIELDS can be a single symbol, which is converted to
a list containing the symbol.

Optional REPOS is a list of repositories to run checks. If it is nil,
then all repositories retrieved using `repom-discover-local-git-repos'
are checked.

By default, this function save modified buffers using
`save-some-buffers' before checking the statuses.
However, if NO-SAVE is non-nil, buffers are not saved.

The result is a list whose item takes a form of \"(REPO . STATUS)\".
REPO is the directory path, and STATUS is a list whose item takes
a form \"(FIELD . VALUE)\".  If a field contains no issue, the field
is not included in the result.  If a repository has no issue,
the repository is not included in the result."
  (unless no-save
    (save-some-buffers))
  (let ((repos (or repos (repom-discover-local-git-repos)))
        (fields (if (symbolp fields)
                    (list fields)
                  fields))
        result)
    (dolist (repo repos)
      (when-let ((status (repom-git--status fields repo)))
        (push (cons repo status) result)))
    (nreverse result)))

(defun repom-git--status (fields repo)
  "Check FIELDS of REPO."
  (let (result
        (file-statuses
         (when (memq 'modified fields)
           (repom-git--git-lines repo "status" "--porcelain"))))
    (dolist (field fields)
      (when-let
          ((value
            (cl-ecase field
              (modified (when-let
                            ((result (cl-remove-if-not
                                      (lambda (s)
                                        (string-match "^\\(M.\\|.M\\)" s))
                                      file-statuses)))
                          (length result))))))
        (push (cons field value) result)))
    (nreverse result)))

;;;; Git utilities
(defun repom-git--git-string (repo &rest args)
  "With REPO, run git with ARGS and return its output as a string."
  (let ((default-directory repo))
    (apply #'magit-git-string args)))

(defun repom-git--git-lines (repo &rest args)
  "With REPO, run git with ARGS and return its output as strings."
  (let ((default-directory repo))
    (apply #'magit-git-lines args)))

(provide 'repom-git)
;;; repom-git.el ends here
