(defsystem "markr"
  :version "0.1.0"
  :author "Madhu Surisetti"
  :license "MIT"
  :depends-on ("clog" "cells")
  :components ((:module "src"
                :components
                ((:file "package")
                 (:file "parser")
                 (:file "render")
                 (:file "main"))))
  :description ""
  :in-order-to ((test-op (test-op "markr/tests"))))

(defsystem "markr/tests"
  :author "Madhu Surisetti"
  :license "MIT"
  :depends-on ("markr"
               "rove")
  :components ((:module "tests"
                :components
                ((:file "main"))))
  :description "Test system for markr"
  :perform (test-op (op c) (symbol-call :rove :run c)))
