(defpackage cl-nanoid/tests/main
  (:use :cl
        :cl-nanoid
        :rove))
(in-package :cl-nanoid/tests/main)

;; NOTE: To run this test file, execute `(asdf:test-system :cl-nanoid)` in your Lisp.

(deftest test-target-1
  (testing "should (= 1 1) to be true"
  (format t "Testing~%")
    (ok (= 1 1))))
