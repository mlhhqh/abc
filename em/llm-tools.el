;; -*- lexical-binding: t; -*-


(gptel-make-tool
 :name "create_file"                    ; javascript-style  snake_case name
 :function (lambda (path filename content)   ; the function that runs
             (let ((full-path (expand-file-name filename path)))
               (with-temp-buffer
                 (insert content)
                 (write-file full-path))
               (format "Created file %s in %s" filename path)))
 :description "Create a new file with the specified content"
 :args (list '(:name "path"             ; a list of argument specifications
           :type string
           :description "The directory where to create the file")
             '(:name "filename"
           :type string
           :description "The name of the file to create")
             '(:name "content"
           :type string
           :description "The content to write to the file"))
 :category "filesystem")                ; An arbitrary label for grouping

(gptel-make-tool
 :name "edit_text"
 :function (lambda (path filename old-text new-text)
             (let ((full-path (expand-file-name filename path)))
               (if (file-exists-p full-path)
                   (with-temp-buffer
                     (insert-file-contents full-path)
                     (goto-char (point-min))
                     (if (search-forward old-text nil t)
                         (progn
                           (replace-match new-text)
                           (write-file full-path)
                           (format "Replaced '%s' with '%s' in %s" old-text new-text filename))
                       (format "Text '%s' not found in %s" old-text filename)))
                 (format "File %s does not exist in %s" filename path))))
 :description "Edit text in an existing file by replacing old text with new text"
 :args (list '(:name "path"
               :type string
               :description "The directory containing the file to edit")
             '(:name "filename"
               :type string
               :description "The name of the file to edit")
             '(:name "old-text"
               :type string
               :description "The text to find and replace")
             '(:name "new-text"
               :type string
               :description "The text to replace the old text with"))
 :category "filesystem")

(gptel-make-tool
 :name "read_file"
 :function (lambda (path filename &optional start-line end-line)
             (let ((full-path (expand-file-name filename path)))
               (if (file-exists-p full-path)
                   (with-temp-buffer
                     (insert-file-contents full-path)
                     (let* ((lines (split-string (buffer-string) "\n"))
                            (total-lines (length lines))
                            (start (or start-line 1))
                            (end (or end-line total-lines)))
                       (cond
                        ((or (< start 1) (> start total-lines))
                         (format "Start line %d is out of range (1-%d)" start total-lines))
                        ((or (< end start) (> end total-lines))
                         (format "End line %d is invalid (should be %d-%d)" end start total-lines))
                        (t
                         (let ((selected-lines (cl-subseq lines (1- start) end)))
                           (format "Lines %d-%d from %s:\n%s" 
                                   start end filename
                                   (string-join selected-lines "\n")))))))
                 (format "File %s does not exist in %s" filename path))))
 :description "Read all or part of a file by specifying line ranges"
 :args (list '(:name "path"
               :type string
               :description "The directory containing the file to read")
             '(:name "filename"
               :type string
               :description "The name of the file to read")
             '(:name "start-line"
               :type integer
               :optional t
               :description "The starting line number (1-indexed, defaults to 1)")
             '(:name "end-line"
               :type integer
               :optional t
               :description "The ending line number (inclusive, defaults to last line)"))
 :category "filesystem")

(gptel-make-tool
 :name "run_shell_command"
 :function (lambda (command &optional working-directory)
             (let ((default-directory (or working-directory default-directory))
                   (output-buffer "*shell-command-output*"))
               (with-output-to-string
                 (with-current-buffer (get-buffer-create output-buffer)
                   (erase-buffer)
                   (let ((exit-code (call-process-shell-command command nil t t)))
                     (let ((output (buffer-string)))
                       (kill-buffer output-buffer)
                       (if (zerop exit-code)
                           (format "Command executed successfully:\n%s" output)
                         (format "Command failed with exit code %d:\n%s" 
                                 exit-code output))))))))
 :description "Execute arbitrary shell commands and return their output"
 :args (list '(:name "command"
               :type string
               :description "The shell command to execute (can be multiline)")
             '(:name "working-directory"
               :type string
               :required nil
               :description "Optional working directory for the command (defaults to current directory)"))
 :category "system")
