;;; $Id: rucksack.asd,v 1.22 2011/06/20 17:22:14 alemmens Exp $

(in-package :cl-user)

(asdf:defsystem :rucksack
  :version "0.1.21"
  :serial t
  :components ((:file "queue")
               (:file "package")
               (:file "errors")
               (:file "thread")
               (:file "mop")
               (:file "serialize" )
               (:file "heap")
               (:file "object-table")
               (:file "schema-table")
               (:file "garbage-collector")
               (:file "cache")
               (:file "objects")
               (:file "p-btrees")
               (:file "index")
               (:file "rucksack")
               (:file "transactions")
               (:file "import-export")))

