;;;; this file contains the definition for the ASDF package

(in-package :asdf-user)

(defsystem #:id3
  :description "my take at an ID3 tag parser"
  :version 0.0.2
  :author "bruhulance"
  :license "deez nuts"
  :serial t
  :components
  ((:file "src/package")
   (:file "src/test")
   (:file "src/main")
   (:file "src/utils")))
