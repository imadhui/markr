(defsystem "understat"
  :version "0.1.0"
  :author "Madhu Surisetti"
  :license "MIT"
  :depends-on ("clack"
               "cl-annot")
  :components ((:module "src"
                :components
                ((:file "main"))))
  :description "A text editor that just rocks!!!"
  :in-order-to ((test-op (test-op "understat/tests"))))

(defsystem "understat/tests"
  :author "Madhu Surisetti"
  :license "MIT"
  :depends-on ("understat"
               "rove")
  :components ((:module "tests"
                :components
                ((:file "main"))))
  :description "Test system for understat"
  :perform (test-op (op c) (symbol-call :rove :run c)))
