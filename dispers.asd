;;;; dispers.asd

(asdf:defsystem #:dispers
  :description "Describe dispers here"
  :author "Your Name <your.name@example.com>"
  :license  "Specify license here"
  :version "0.0.1"
  :serial t
  :depends-on ("eazy-gnuplot" "lhstats")
  :components ((:file "package")
               (:file "dispers")))
