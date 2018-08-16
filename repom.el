;;; repom.el --- Repository manager -*- lexical-binding: t -*-

;; Copyright (C) 2018 Akira Komamura

;; Author: Akira Komamura <akira.komamura@gmail.com>
;; Version: 1.0-pre
;; Package-Requires: ((emacs "25.1") (dash "2.10") (projectile "0.14") (f "0.19"))
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

;; This module provides basic facilities for repom, a repository manager for
;; Emacs.

;;; Code:

(require 'dash)
(require 'projectile)
(require 'f)
(require 'generator)

(defgroup repom nil
  "Manage local and remote repositories."
  :prefix "repom-"
  :group 'vc)

(defgroup repom-local nil
  "Configuration for local repositories."
  :group 'repom)

(defconst repom-git-dir
  ;; TODO: Add support for Windows (maybe you can use `system-type' for check)
  ".git"
  "The file name of Git repositories on the platform.")

(defcustom repom-clone-destination-for-viewing "~/Downloads/"
  "Directory for repositories for viewing."
  :type 'string
  :group 'repom-local)

(defcustom repom-clone-destination-for-editing "~"
  "Directory for repositories for editing."
  :type 'string
  :group 'repom-local)

(defcustom repom-local-discovery-locations nil
  "List of extra local repository locations."
  :type '(repeat (list (string :tag "Directory")
                       (integer :tag "Level")
                       (plist :tag "Options" :inline t
                              :options
                              (((const :tag "Name to display in tables" :name)
                                string)))))
  :group 'repom-local)

(defcustom repom-edit-project-command
  #'projectile-switch-project-by-name
  "A command used to edit a project.

This function is called with the project directory as the argument."
  :type 'function
  :group 'repom-local)

(defcustom repom-view-project-command
  #'repom-project-find-file-other-window
  "A command used to view a project.

This function is called with the project directory as the argument."
  :type 'function
  :group 'repom-local)

;;;; Macros
(defmacro repom--with-cache-variable (var &optional force-update
                                          &rest form)
  "Retrieve a value and store the result into a variable.

If both VAR and FORCE-UPDATE is nil, first set it to the result of FORM.
Return the value of VAR."
  (declare (indent 1))
  `(if (and ,var (not ,force-update))
       ,var
     (setq ,var (progn ,@form))))

;;;; Operations on a repository
(defun repom--projectile-with-dir (dir func)
  "Run a projectile command at a given project directory.

DIR is the root of the project.

FUNC is a symbol which represents a projectile function to be run."
  (let* ((default-directory dir)
         (projectile-cached-project-root dir))
    (funcall func)))

(defun repom-project-find-file (dir)
  "Run `projectile-find-file' at DIR."
  (repom--projectile-with-dir dir #'projectile-find-file))

(defun repom-project-find-file-other-window (dir)
  "Run `projectile-find-file-other-window' at DIR."
  (repom--projectile-with-dir dir #'projectile-find-file-other-window))

(defun repom--view-project (&optional dir)
  "Browse a project for reading.

This function calls a function set as `repom-view-project-command` is run.
This is an alternative to `projectilep-switch-project' for different situations.

If DIR is set, the function is caled with `default-directory` set to it.
Otherwise, `default-directory' should be set to the root of the project
directory."
  (funcall repom-view-project-command (or dir default-directory)))

(defun repom--edit-project (&optional dir)
  "Browse a project for editing.

If DIR is set, the function is caled with `default-directory` set to it.
Otherwise, `default-directory' should be set to the root of the project
directory"
  (funcall repom-edit-project-command (or dir default-directory)))

;;;; Discover local repositories
(defun repom--local-discovery-locations ()
  "Return a list of local discovery locations, including clone destinations."
  (append repom-local-discovery-locations
          ;; Append the clone destinations
          (mapcar (lambda (path) (list path 1))
                  ;; Check if the paths are not included in the configured locations
                  (cl-remove-if
                   (lambda (path)
                     (eql 1 (cadr (cl-assoc path repom-local-discovery-locations
                                            :test #'f-same-p))))
                   (list repom-clone-destination-for-viewing
                         repom-clone-destination-for-editing)))))

(iter-defun repom--yield-projectile-projects ()
  "Generates known projectile projects in canonical paths."
  (dolist (item (cl-remove-duplicates
                 ;; Append slash for detecting duplicates
                 (mapcar #'f-slash (mapcar #'f-canonical
                                           projectile-known-projects))
                 :test #'string-equal))
    (when (f-directory-p item)
      (iter-yield item))))

(iter-defun repom--yield-directories-at-level (root level)
  "Generates directories inside ROOT at LEVEL."
  (if (= level 0)
      (when (f-directory-p root)
        (iter-yield (f-slash root)))
    (dolist (subdir (f-directories root))
      (if (= level 1)
          (iter-yield (f-slash subdir))
        (iter-yield-from (repom--yield-directories-at-level subdir
                                                            (1- level)))))))

(iter-defun repom--yield-directories ()
  "Generates local Git repositories."
  (let (sent)
    (iter-do (dir (repom--yield-projectile-projects))
      (push dir sent)
      (iter-yield dir))
    (cl-loop for (root level . _) in (repom--local-discovery-locations)
             do (iter-do (dir (repom--yield-directories-at-level (f-canonical root)
                                                                 level))
                  (unless (not (member root sent))
                    (iter-yield dir))))))

(iter-defun repom--yield-git-work-trees ()
  "Generates local Git repositories (working trees)."
  (iter-do (dir (repom--yield-directories))
    (when (f-directory-p (f-join dir repom-git-dir))
      (iter-yield dir))))

;;;###autoload
(defun repom-discover-local-git-repos ()
  "Discover local Git repositories.

This function discovers local Git repositories according to
`repom-local-discovery-locations' (which is similar to
`magit-repository-directories' in magit) as well as
`projectile-known-directories'.

The result is a list of directories.  Each item in the result is a canonicalized
path (i.e. symlink-resolved) and has a trailing path.  The result does not
contain duplicates."
  (let (result)
    (iter-do (dir (repom--yield-git-work-trees))
      (push dir result))
    result))

(defun repom-identify-local-group (repo)
  "Identify a group REPO belongs to.

This function finds a group of a repository as configured in
`repom-local-discovery-locations'.  If the group has :name option set,
this function returns the name."
  (setq repo (expand-file-name repo))
  (cl-loop for (root level . options) in repom-local-discovery-locations
           when (string-prefix-p (setq root (file-name-as-directory
                                             (expand-file-name root)))
                                 repo)
           when (eq level (length (f-split (f-relative repo root))))
           return (or (plist-get options :name) root)))

;;;; Utilities
(defmacro repom--def-object-api (name args doc &rest body)
  "Define a function to operate on an JSON object (or alist).

NAME is a suffix of the function, ARGS is a list of arguments (which
is actually \"map\" arguments in `pcase', and DOC is a docstring.
BODY is the body of the function."
  (declare (indent 2))
  (let ((name (concat "repom--api-" (symbol-name name))))
    `(defalias (quote ,(intern name))
       (pcase-lambda ((map ,@args)) ,@body)
       ,doc)))

(defsubst repom--escape-query (&rest query)
  "Escape a search QUERY."
  (string-join (mapcar #'url-hexify-string
                       (-flatten (mapcar #'split-string query)))
               "+"))

(defun repom--find-local-repo-of-name (name)
  "Find a local repository of NAME."
  (-find (lambda (dir) (string-equal name (f-filename dir)))
         projectile-known-projects))

(defsubst repom--existing-path (path)
  "Return PATH if it is an existing path."
  (when (f-exists? path)
    path))

(defsubst repom--default-view-location (name)
  "Return the default repository location of NAME for viewing."
  (f-join repom-clone-destination-for-viewing name))

(defsubst repom--default-edit-location (name)
  "Return the default repository location of NAME for editing."
  (f-join repom-clone-destination-for-editing name))

(provide 'repom)
;;; repom.el ends here
