;;; repom-git.el --- Git operations for repository management -*- lexical-binding: t -*-

;; Copyright (C) 2018 Akira Komamura

;; Author: Akira Komamura <akira.komamura@gmail.com>
;; Version: 1.0-pre
;; Package-Requires: ((emacs "25.1") (f "0.19") (magit "2.12") (dash "2.10") (tablist "0.70"))
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
(require 'tablist)

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

;;;; Branch list interfaces

(define-derived-mode repom-git-branch-list-mode tabulated-list-mode
  "Git Branches"
  "Major mode for displaying branches in local Git repositories."
  (tabulated-list-init-header)
  (tablist-minor-mode 1))

(defvar repom-git-branch-list-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map [?\s] 'repom-git-branch-list--show)
    (define-key map "k" nil)
    (define-key map "C" 'repom-git-branch-list--checkout)
    (define-key map "D" 'repom-git-branch-list--delete)
    map))

(defun repom-git-branch-list--show ()
  "Show a selected branch."
  (interactive)
  (let ((orig-window (selected-window)))
    (repom-git-branch-list--view)
    (select-window orig-window)))

(defun repom-git-branch-list--view ()
  "Show a selected branch."
  (interactive)
  (repom-git-branch-list--with-selected
      (lambda (branch)
        (magit-log (list branch) '("--graph" "--decorate")))))

(defun repom-git-branch-list--checkout ()
  "Check out a branch under the cursor and open `magit-status'."
  (interactive)
  (repom-git-branch-list--with-selected
      (lambda (branch)
        (magit-checkout branch)
        (call-interactively #'magit-status))))

(defun repom-git-branch-list--parse-id (id)
  "Parse the ID of an item in a branch list."
  (split-string (symbol-name id) "@"))

(defun repom-git-branch-list--with-selected (func)
  "Call FUNC with the selected branch as the argument."
  (declare (indent 1))
  (when-let ((id (tabulated-list-get-id)))
    (-let (((repo branch) (repom-git-branch-list--parse-id id)))
      (let ((default-directory repo))
        (funcall func branch)))))

(defun repom-git-branch-list--delete ()
  "Delete selected branches."
  (interactive)
  (let ((branches (--map (repom-git-branch-list--parse-id (car it))
                         (tablist-get-marked-items))))
    (when (yes-or-no-p (format "Delete the following %d branches?\n%s"
                               (length branches)
                               (mapconcat (lambda (cells)
                                            (format "%-30s:%s"
                                                    (abbreviate-file-name (car cells))
                                                    (nth 1 cells)))
                                          branches "\n")))
      (cl-loop for (repo branch) in branches
               do (repom-git--delete-branch repo branch))
      (tablist-do-kill-lines))))

(defcustom repom-git-deletable-branch-list-exclude-master t
  "Exclude master branches in `repom-git-deletable-branch-list'.

If this option is non-nil, \"master\" branches are excluded from the
items in `repom-git-deletable-branch-list' command."
  :group 'repom-git
  :type 'boolean)

;;;###autoload
(defun repom-git-deletable-branch-list ()
  "Display a table of deletable branches."
  (interactive)
  (with-current-buffer (get-buffer-create "*Deletable Git Branches*")
    (let ((inhibit-read-only t))
      (erase-buffer))
    (setq tabulated-list-format [("Group" 10 t)
                                 ("Repository" 20 t)
                                 ("Branch" 15 t)
                                 ("Status" 7 t)
                                 ("Destination" 15 t)
                                 ("Last commit" 20 t)]
          tabulated-list-padding 1)
    (repom-git-branch-list-mode)
    (add-hook 'tabulated-list-revert-hook
              #'repom-git-deletable-branch-list--get nil t)
    (tablist-revert)
    (pop-to-buffer-same-window (current-buffer))))

(defun repom-git-deletable-branch-list--get ()
  "Get a list of deletable branches in all repositories."
  (setq tabulated-list-entries
        (cl-loop for (repo branch) in (repom-git--all-repo-branches
                                       :exclude-head t)
                 unless (and repom-git-deletable-branch-list-exclude-master
                             (equal branch "master"))
                 for state = (repom-git--branch-deletable repo branch)
                 when state
                 collect (list (intern (concat repo "@" branch))
                               (vector (or (repom-identify-local-group repo)
                                           (abbreviate-file-name (f-parent repo)))
                                       (file-name-nondirectory
                                        (string-remove-suffix "/" repo))
                                       branch
                                       (car state)
                                       (cdr state)
                                       (repom-git--last-commit-relative-date repo branch))))))

;;;###autoload
(defun repom-git-unpushed-branch-list ()
  "Display a table of unpushed branches."
  (interactive)
  (with-current-buffer (get-buffer-create "*Unpushed Git Branches*")
    (let ((inhibit-read-only t))
      (erase-buffer))
    (setq tabulated-list-format [("Group" 10 t)
                                 ("Repository" 20 t)
                                 ("Branch" 15 t)
                                 ("Upstream" 15 t)
                                 ("Diff(A/B)" 8 t)
                                 ("Last commit" 20 t)]
          tabulated-list-padding 1)
    (repom-git-branch-list-mode)
    (add-hook 'tabulated-list-revert-hook
              #'repom-git-unpushed-branch-list--get nil t)
    (tablist-revert)
    (pop-to-buffer-same-window (current-buffer))))

(defun repom-git-unpushed-branch-list--get ()
  "Get a list of deletable branches in all repositories."
  (setq tabulated-list-entries
        (cl-loop for (repo branch) in (repom-git--all-repo-branches
                                       :exclude-head t)
                 for upstream = (repom-git--upstream-branch repo branch)
                 for upstream-diff = (when upstream
                                       (repom-git--compare-revs repo branch upstream))
                 ;; Skip if the branch doesn't have an upstream and it has been merged
                 unless (and (null upstream)
                             (repom-git--rev-merged repo branch))
                 unless (and upstream
                             (= 0 (car upstream-diff)))
                 collect (list (intern (concat repo "@" branch))
                               (vector (or (repom-identify-local-group repo)
                                           (abbreviate-file-name (f-parent repo)))
                                       (file-name-nondirectory
                                        (string-remove-suffix "/" repo))
                                       branch
                                       (or upstream "-")
                                       (if upstream
                                           (repom-git--show-diff-counts upstream-diff)
                                         "-")
                                       (repom-git--last-commit-relative-date repo branch))))))

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

(defun repom-git--head-branch (repo)
  "Get the head branch of REPO."
  (repom-git--git-string repo "symbolic-ref" "--short" "HEAD"))

(defun repom-git--upstream-branch (repo &optional branch)
  "Get an upstream branch of a branch in a repository.

REPO is the repository, and BRANCH is the branch."
  (let ((default-directory repo))
    (magit-get-upstream-branch branch t)))

(defun repom-git--last-commit-relative-date (repo branch)
  "Return the relative date of the last commit of a branch.

REPO is the repository, and BRANCH is the branch."
  (car (repom-git--git-lines repo
                             "show"
                             "--pretty=tformat:%cr"
                             branch)))

(cl-defun repom-git--all-repo-branches (&key exclude-head
                                             include-detached-head)
  "Return all branches in all local repositories.

The result is a list of (REPO BRANCH)."
  (-flatten-n
   1 (mapcar
      (lambda (repo)
        (let ((head (repom-git--head-branch repo)))
          (cl-loop for branch in (repom-git--branches repo)
                   unless (and exclude-head (equal head branch))
                   unless (and (string-prefix-p "(HEAD detached from " branch)
                               (not include-detached-head))
                   collect (list repo branch))))
      (repom-discover-local-git-repos))))

(defun repom-git--branches (repo)
  "List branches in a given repository REPO."
  (mapcar #'repom-git--clean-branch
          (repom-git--git-lines repo "branch")))

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

(defun repom-git--compare-revs (repo r1 r2)
  "Compare two revision in a repository.

REPO is the repository, and R1 and R2 are revisions (usuallly
branches).
The result is a list."
  (let ((default-directory repo))
    (magit-rev-diff-count r1 r2)))

(defun repom-git--show-diff-counts (r)
  "Format a result of `repom-git--compare-revs'.

R is a list returned from `repom-git--compare-revs'."
  (apply #'format "+%d/-%d" r))

(defun repom-git--rev-merged (repo rev)
  "Test if a revision/branch is merged into HEAD.

REPO is the repository, and REV is a revision (usually a branch)."
  (let ((default-directory repo))
    (= 0 (car (magit-rev-diff-count rev "HEAD")))))

(defun repom-git--clean-branch (s)
  "Trim a prefix string from a branch entry S.

This function strip a prefix string of each line in the result of
\"git-branch\" command."
  (string-trim-left (string-remove-prefix "*" s)))

(defun repom-git--delete-branch (repo branch)
  "Delete a branch in a Git repository.

REPO is the repository containing the branch, and BRANCH is the branch
to delete."
  (repom-git--run-git repo "branch" "-d" branch))

(defun repom-git--branch-deletable (repo branch)
  "Check if a branch if deletable.

This function returns non-nil if a branch in a repository is deletable.
REPO is the repository, and BRANCH is the branch.
The actual result is a cons cell if the repository is deletable.
The first item is a string indicating the status, and the second item
is either a remote branch or a local branch."
  (let ((default-directory repo)
        upstream)
    (cond
     ((setq upstream (magit-get-upstream-branch branch t))
      (when (= 0 (car (magit-rev-diff-count branch upstream)))
        (cons "Pushed" upstream)))
     ((repom-git--rev-merged repo branch)
      (cons "Merged" (repom-git--head-branch repo))))))

(cl-defun repom-git--unpushed-commits (repo &optional branch use-upstream
                                            &key include-name)
  "Count the number of unpushed commits.

This function counts the number of unpushed commits in REPO.
If BRANCH is non-nil, compare the branch with its push branch.

If USE-UPSTREAM is non-nil, compare the branch with its upstream
branch rather than the push remote.

If INCLUDE-NAME is non-nil, return (REMOTE . NUMBER) where REMOTE
is the name of an upstream/push branch.

If the branch doesn't have a push remote, it returns nil."
  (let ((default-directory repo))
    (when-let ((remote (if use-upstream
                           (magit-get-upstream-branch branch t)
                         (magit-get-push-branch branch t)))
               (n (car (magit-rev-diff-count (or branch "HEAD") remote))))
      (if include-name
          (cons remote n)
        n))))

(provide 'repom-git)
;;; repom-git.el ends here
