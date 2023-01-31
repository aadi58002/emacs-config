;;; Compiled snippets and support files for `markdown-mode'
;;; Snippet definitions:
;;;
(yas-define-snippets 'markdown-mode
		     '(("<blog" "---\ntitle: ${1:`\n(string-join\n (mapcar #'capitalize\n         (split-string\n          (let (case-fold-search)\n            (replace-regexp-in-string\n             \"\\\\([[:lower:]]\\\\)\\\\([[:upper:]]\\\\)\" \"\\\\1 \\\\2\"\n             (replace-regexp-in-string \"\\\\([[:upper:]]\\\\)\\\\([[:upper:]][0-9[:lower:]]\\\\)\"\n                                       \"\\\\1 \\\\2\" (file-name-base(file-name-base buffer-file-name)))))\n          \"[^[:word:]0-9]+\"\n          )) \" \" )\n`}\n---" "Blog Creation Snippet" t nil nil "/home/aditya-yadav/.config/emacs/snippets/markdown-mode/__Blog" nil nil)))


;;; Do not edit! File generated at Sat Jan 28 16:42:52 2023
