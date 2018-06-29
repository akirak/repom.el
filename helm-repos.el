;;; helm-repos.el --- Helm for repositories -*- lexical-binding: t -*-

(require 'helm-repos-github)
(require 'helm-projectile)

;;;###autoload
(defun helm-repos (arg)
  (interactive "P")
  (when arg
    (helm-repos-github-clear-cache))
  (helm :prompt "Git repo: "
        :sources
        '(helm-source-projectile-projects
          helm-repos-github-source-user-repos
          helm-repos-github-source-starred-repos
          ;; TODO: Add dummy source
          ;; helm-repos-git-source-dummy-url
          )))

(provide 'helm-repos)
;;; helm-repos.el ends here
