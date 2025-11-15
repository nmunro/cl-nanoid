(defsystem "cl-nanoid"
  :version "0.0.1"
  :author "nmunro"
  :license "BSD3-Clause"
  :description ""
  :depends-on (:ironclad)
  :components ((:module "src"
                :components
                ((:file "main"))))
  :in-order-to ((test-op (test-op "cl-nanoid/tests"))))

(defsystem "cl-nanoid/tests"
  :author "nmunro"
  :license "BSD3-Clause"
  :depends-on ("cl-nanoid"
               :rove)
  :components ((:module "tests"
                :components
                ((:file "main"))))
  :description "Test system for cl-nanoid"
  :perform (test-op (op c) (symbol-call :rove :run c)))
