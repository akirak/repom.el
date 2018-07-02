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

(defgroup repom nil
  "Manage local and remote repositories."
  :prefix "repom-"
  :group 'vc)

(defcustom repom-local nil
  "Configuration for local repositories."
  :group 'repom)

(defcustom repom-clone-destination-for-viewing "~/Downloads/"
  "Directory for repositories for viewing."
  :type 'string
  :group 'repom-local)

(defcustom repom-clone-destination-for-editing "~"
  "Directory for repositories for editing."
  :type 'string
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
(defmacro repom--with-cache-variable (var form)
  "If VAR is nil, first set it to the result of FORM.  Return the value of VAR."
  (declare (indent 1))
  `(if ,var
       ,var
     (setq ,var ,form)))

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

;;;; Utilities
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
