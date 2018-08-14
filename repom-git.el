;;; repom-git.el --- Git operations for repository management -*- lexical-binding: t -*-

;; Copyright (C) 2018 Akira Komamura

;; Author: Akira Komamura <akira.komamura@gmail.com>
;; Version: 1.0-pre
;; Package-Requires: ((emacs "25.1") (f "0.19") (magit "2.12") (dash "2.10"))
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
(require 'dash)

(defgroup repom-git nil
  "Git"
  :prefix "repom-git-"
  :group 'repom)

(cl-defstruct repom-git-status summary count details)

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

;;;###autoload
(cl-defun repom-git-clone-for-viewing (url name)
  "Clone a Git repository from URL and edit it.  Use NAME as the destination."
  (if-let ((existing-dest (repom--find-local-repo-of-name name)))
      (repom--view-project existing-dest)
    (repom-git--clone-internal url
                               (repom--default-view-location name)
                               #'repom--view-project)))

;;;###autoload
(cl-defun repom-git-clone-for-editing (url name)
  "Clone a Git repository from URL and edit it.  Use NAME as the destination."
  (if-let ((existing-dest (repom--find-local-repo-of-name name)))
      (repom--edit-project existing-dest)
    (repom-git--clone-internal url
                               (repom--default-edit-location name)
                               #'repom--edit-project)))

;;;; Repository statuses

;;;###autoload
(cl-defun repom-git-statuses (fields &optional repos no-save)
  "Get statuses of local Git repositories.

FIELDS is a list of symbols to specify which types of check to run.
The following values are allowed:

dirty
  Any dirty states by \"git status --porcelain\" command, except for
  untracked files.
untracked
  untracked files in the repository.
stash
  Stashes in the repository.
unmerged <branch>
  Unmerged branches.
unpushed
  The number of unpushed commits.

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
  (let ((statuses
         (when (-intersection fields '(dirty untracked))
           (repom-git--git-lines repo "status" "--porcelain"))))
    (->> fields
         (mapcar
          (lambda (field)
            (pcase field
              ('dirty
               (when-let
                   ((result (--filter (not (string-prefix-p "??" it))
                                      statuses)))
                 (make-repom-git-status :summary (format "%d dirty files"
                                                         (length result))
                                        :count (length result)
                                        :details result)))
              ('untracked
               (when-let
                   ((result (--filter (string-prefix-p "??" it) statuses)))
                 (make-repom-git-status :summary (format "%d untracked files"
                                                         (length result))
                                        :count (length result)
                                        :details result)))
              ('stash
               (when-let
                   ((result (repom-git--git-lines repo "stash" "list")))
                 (make-repom-git-status :summary (format "%d stashes"
                                                         (length result))
                                        :count (length result)
                                        :details result)))
              ((or 'unmerged
                   `(unmerged ,ref))
               (when-let ((ref (or (bound-and-true-p ref) "HEAD"))
                          (result (repom-git--unmerged-branches repo ref)))
                 (make-repom-git-status :summary (format "Unmerged branches (%s): %s"
                                                         ref (string-join result ", "))
                                        :count (length result)
                                        :details result)))
              ('unpushed
               ;; TODO: Allow specifying a branch
               (let ((n (repom-git--unpushed-commits repo)))
                 (cond
                  ((null n)
                   (make-repom-git-status :summary "No push remote"))
                  ((> n 0)
                   (make-repom-git-status :summary (format "Unpushed commits: %d" n)
                                          :count n
                                          :details n))))))))
         (delq nil))))


;;;; Git utilities
(defun repom-git--git-string (repo &rest args)
  "With REPO, run git with ARGS and return its output as a string."
  (let ((default-directory repo))
    (apply #'magit-git-string args)))

(defun repom-git--git-lines (repo &rest args)
  "With REPO, run git with ARGS and return its output as strings."
  (let ((default-directory repo))
    (apply #'magit-git-lines args)))

(defun repom-git--run-git (repo &rest args)
  "Run a Git command in a given repository.

REPO is the path to the repository, and ARGS is a command line
arguments passed to Git."
  (let ((default-directory repo))
    (magit-run-git args)))

(defun repom-git--unmerged-branches (repo &optional ref)
  "List unmerged branches in a given repository.

REPO is the path to the repository, and REF is the branch to check
against."
  (mapcar #'repom-git--clean-branch
          (repom-git--git-lines repo "branch" "--no-merged"
                                (or ref "HEAD"))))

(defun repom-git--merged-branches (repo &optional ref)
  "List merged branches in a given repository.

REPO is the path to the repository, and REF is the branch to check
against."
  (mapcar #'repom-git--clean-branch
          (repom-git--git-lines repo "branch" "--merged"
                                (or ref "HEAD"))))

(defun repom-git--clean-branch (s)
  "Trim a prefix string from a branch entry.

This function strip a prefix string of each line in the result of
git-branch command."
  (string-trim-left s "\*?[\t\n\r ]+"))

(defun repom-git--delete-branch (repo branch)
  "Delete a branch in a Git repository.

REPO is the repository containing the branch, and BRANCH is the branch
to delete."
  (repom-git--run-git repo "branch" "-d" branch))

(defun repom-git--unpushed-commits (repo &optional branch)
  "Count the number of unpushed commits.

This function counts the number of unpushed commits in REPO.
If BRANCH is non-nil, compare the branch with its push branch.

If the branch doesn't have a push remote, it returns nil."
  (let ((default-directory repo))
    (when-let ((remote (magit-get-push-branch branch t)))
      (nth 1 (magit-rev-diff-count (or branch "HEAD") remote)))))

(provide 'repom-git)
;;; repom-git.el ends here
