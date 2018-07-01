;;; repom-git.el --- Git operations for repository management -*- lexical-binding: t -*-

(require 'repom)

(require 'f)

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
         (sentinel (lambda (proc event)
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

(provide 'repom-git)
;;; repom-git.el ends here
