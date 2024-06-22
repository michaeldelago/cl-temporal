(asdf:defsystem :eager-future2
  :name "eager-future2"
  :author "Vladimir Sedach <vas@oneofus.la>"
  :license "LGPL-3.0-or-later"
  :description "Parallel programming library providing the futures/promises synchronization mechanism"
  :serial t
  :components ((:file "package")
               (:file "scheduler")
               (:file "make-future")
               (:file "future")
               (:file "library"))
  :depends-on (:bordeaux-threads :trivial-garbage))
