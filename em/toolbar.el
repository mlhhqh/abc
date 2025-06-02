(defun my/at-node-type-p (node-type)
  "Check if point is on a line containing a NODE-TYPE treesit node."
  (when (and (treesit-available-p)
             (treesit-parser-list))
    (save-excursion
      (beginning-of-line)
      (let ((line-end (line-end-position))
            (found nil))
        (while (and (not found) (<= (point) line-end))
          (when-let ((node (treesit-node-at (point))))
            (let ((current node))
              (while (and current (not found))
                (when (string-equal (treesit-node-type current) node-type)
                  (setq found t))
                (setq current (treesit-node-parent current)))))
          (forward-char 1))
        found))))

(defun my/interface-command ()
  "Command to execute for interface types."
  (interactive)
  (message "Executing interface command!"))

(defun my/method-command ()
  "Command to execute for method declarations."
  (interactive)
  (message "Executing method command!"))

(defun my/add-treesit-toolbar-buttons ()
  "Add toolbar buttons for treesit node types."
  (when (bound-and-true-p tool-bar-mode)
    ;; Interface button
    (define-key tool-bar-map [interface-action]
      `(menu-item "Interface" my/interface-command
                  :image ,(find-image '((:type xpm :file "connect.xpm")
                                       (:type pbm :file "connect.pbm")))
                  :help "Execute interface command"
                  :enable (my/at-node-type-p "interface_type")))
    
    ;; Method button
    (define-key tool-bar-map [method-action]
      `(menu-item "Method" my/method-command
                  :image ,(find-image '((:type xpm :file "connect.xpm")
                                       (:type pbm :file "connect.pbm")))
                  :help "Execute method command"
                  :enable (my/at-node-type-p "method_declaration")))))

;; Setup hook
(add-hook 'go-ts-mode-hook #'my/add-treesit-toolbar-buttons)
