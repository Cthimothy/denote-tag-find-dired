(defvar denote-tag-find-dired-search-dirs '("~/Org/" "~/Org/Journal/")
  "Directories to search for Denote files.")

(defun denote-tag-find-dired--denote-tags-from-filename (filename)
  "Extract tags from a Denote FILENAME."
  (when (string-match "__\\([a-z0-9_]+\\)\\.org$" filename)
    (split-string (match-string 1 filename) "_")))

(defun denote-tag-find-dired--collect-denote-tags ()
  "Collect all unique Denote tags from files in `denote-tag-find-dired-search-dirs`."
  (let ((all-tags '()))
    (dolist (dir denote-tag-find-dired-search-dirs)
      (when (file-directory-p dir)
        (dolist (file (directory-files-recursively dir "\\.org$"))
          (let ((tags (denote-tag-find-dired--denote-tags-from-filename file)))
            (dolist (tag tags)
              (add-to-list 'all-tags tag))))))
    (sort (delete-dups all-tags) #'string<)))

(defun denote-tag-find-dired--files-with-tag (tag)
  "Return a list of files that have TAG in their Denote filename."
  (let ((matched-files '()))
    (dolist (dir denote-tag-find-dired-search-dirs)
      (when (file-directory-p dir)
        (dolist (file (directory-files-recursively dir "\\.org$"))
          (let ((tags (denote-tag-find-dired--denote-tags-from-filename file)))
            (when (member tag tags)
              (push file matched-files))))))
    matched-files))

(defun denote-tag-find-dired-find-tagged-files ()
  "Prompt with Ivy for a Denote tag and open a Dired buffer of matching files."
  (interactive)
  (let* ((tags (denote-tag-find-dired--collect-denote-tags))
         (chosen-tag (ivy-completing-read "Tag: " tags)))
    (let ((files (denote-tag-find-dired--files-with-tag chosen-tag)))
      (if files
          (dired (cons "Matching Files" files))
        (message "No files found with tag: %s" chosen-tag)))))

(provide 'denote-tag-find-dired)
