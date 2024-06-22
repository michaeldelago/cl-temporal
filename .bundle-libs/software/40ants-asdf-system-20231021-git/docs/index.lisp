(uiop:define-package #:40ants-asdf-system-docs/index
  (:use #:cl)
  (:import-from #:pythonic-string-reader
                #:pythonic-string-syntax)
  (:import-from #:quicklisp)
  (:import-from #:named-readtables
                #:in-readtable)
  (:import-from #:40ants-doc
                #:defsection
                #:defsection-copy)
  (:import-from #:40ants-asdf-system-docs/changelog
                #:@changelog)
  (:import-from #:40ants-asdf-system
                #:@usage)
  (:import-from #:docs-config
                #:docs-config)
  (:export #:@index
           #:@readme
           #:@changelog))
(in-package #:40ants-asdf-system-docs/index)

(in-readtable pythonic-string-syntax)


(defmethod docs-config ((system (eql (asdf:find-system "40ants-asdf-system-docs"))))
  ;; 40ANTS-DOC-THEME-40ANTS system will bring
  ;; as dependency a full 40ANTS-DOC but we don't want
  ;; unnecessary dependencies here:
  (ql:quickload "40ants-doc-theme-40ants")
  (list :theme
        (find-symbol "40ANTS-THEME"
                     (find-package "40ANTS-DOC-THEME-40ANTS"))))


(defsection @index (:title "40ANTS-ASDF-SYSTEM - Base ASDF system for all 40Ants projects."
                    :ignore-words ("JSON"
                                   "HTTP"
                                   "REPL"
                                   "GIT"
                                   "BSD"
                                   "LOG4CL"
                                   "THIS-CONSOLE"
                                   "DAILY"
                                   "FILE"))
  (40ants-asdf-system system)
  "
[![](https://github-actions.40ants.com/40ants/40ants-asdf-system/matrix.svg?only=ci.run-tests)](https://github.com/40ants/40ants-asdf-system/actions)

![Quicklisp](http://quickdocs.org/badge/40ants-asdf-system.svg)
"
  (@installation section)
  (@usage section))


(defsection-copy @readme @index)


(defsection @installation (:title "Installation")
  """
You can install this library from Quicklisp, but you want to receive updates quickly, then install it from Ultralisp.org:

```
(ql-dist:install-dist "http://dist.ultralisp.org/"
                      :prompt nil)
(ql:quickload :40ants-asdf-system)
```
""")


(defsection @usage (:title "Usage"
                    :ignore-words ("ASDF:PACKAGE-INFERRED-SYSTEM"
                                   "ASDF"
                                   "40A"))
  "
This system provides a base class for ASDF systems of `40Ants`.
This ASDF system class inherits from ASDF:PACKAGE-INFERRED-SYSTEM class. This class
reads its version from the src/changelog.lisp file. Thus, ASDF system version
stored in the one place and always correct.

To use this class, mention it in the ASDF system definition like this:

```
(defsystem log4cl-extras
  :name \"log4cl-extras\"
  :author \"Alexander Artemenko\"
  :license \"BSD\"
  :class :40ants-asdf-system
  :defsystem-depends-on (\"40ants-asdf-system\")
  ...
```
 
"
  (asdf/interface::40ants-asdf-system system))
