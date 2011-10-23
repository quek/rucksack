;;; -*- lisp -*-

(asdf:defsystem rucksack-test
  :depends-on (:rucksack :bordeaux-threads)
  :components
    ((:file "lisp-unit")
     (:file "package")
     (:file "unit-tests"))
  :serial t)
