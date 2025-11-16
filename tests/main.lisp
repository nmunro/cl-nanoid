(defpackage cl-nanoid/tests/main
  (:use :cl
        :cl-nanoid
        :rove))
(in-package :cl-nanoid/tests/main)

;; NOTE: To run this test file, execute `(asdf:test-system :cl-nanoid)` in your Lisp.

(defun chars-subset-of (string alphabet)
  "Return T if every char in STRING is present in ALPHABET."
  (loop :for ch :across string :always (find ch alphabet :test #'char=)))

(defun make-deterministic-rng ()
  "Return a function (N) -> simple-array (unsigned-byte 8) of length N,
   filled deterministically with 0..255 cycling. Useful for predictable tests."
  (let ((counter 0))
    (lambda (n)
      (let ((v (make-array n :element-type '(unsigned-byte 8))))
        (dotimes (i n)
          (setf (aref v i) (mod (+ counter i) 256)))
        (incf counter n)
        v))))

(deftest default-length-and-charset
  (testing "Default size is 21 and characters are from the default alphabet"
    (let ((id (cl-nanoid:generate)))
      (ok (= (length id) 21))
      (ok (chars-subset-of id cl-nanoid:+alphabet+)))))

(deftest custom-size
  (testing "Custom size respected"
    (let ((n 7)
          (id (cl-nanoid:generate :size 7)))
      (ok (= (length id) n)))))

(deftest custom-alphabet-basic
  (testing "Custom alphabet respected"
    (let* ((alphabet "abcXYZ_")
           (id (cl-nanoid:generate :alphabet alphabet :size 32)))
      (ok (= (length id) 32))
      (ok (chars-subset-of id alphabet)))))

(deftest deterministic-with-mock-rng
  (testing "Deterministic output with a deterministic RNG"
    (let* ((rng (make-deterministic-rng))
           (alphabet "abcdef")
           (id1 (cl-nanoid:generate :alphabet alphabet :size 16 :random-bytes-fn rng))
           (id2 (cl-nanoid:generate :alphabet alphabet :size 16 :random-bytes-fn (make-deterministic-rng))))
      (ok (string= id1 id2))
      (ok (= (length id1) 16))
      (ok (chars-subset-of id1 alphabet)))))

(deftest rejection-path-when-mask-exceeds-alphabet
  (testing "Indices that fall outside alphabet-length are rejected and final length is still correct"
    (let* ((alphabet "abc")
           (rng (make-deterministic-rng))
           (id (cl-nanoid:generate :alphabet alphabet :size 48 :random-bytes-fn rng)))
      (ok (= (length id) 48))
      (ok (chars-subset-of id alphabet)))))

(deftest many-generations-no-obvious-collisions
  (testing "Multiple generations produce unique IDs (probabilistic sanity check)"
    (let ((seen (make-hash-table :test 'equal)))
      (loop :for i :from 1 :to 200 :do
           (let ((id (cl-nanoid:generate)))
             (ok (null (gethash id seen)))
             (setf (gethash id seen) t))))))

(deftest validation-bad-alphabet-type
  (testing "Non-string alphabet should signal an error"
    (ok (signals (cl-nanoid:generate :alphabet '(#\a #\b #\c)) 'error))))

(deftest validation-duplicate-chars
  (testing "Alphabet with duplicate chars should signal an error"
    (ok (signals (cl-nanoid:generate :alphabet "aabc") 'error))))

(deftest validation-alphabet-length-bounds
  (testing "Alphabet must have between 2 and 255 chars"
    (ok (signals (cl-nanoid:generate :alphabet "a") 'error))                                      ;; too short
    (ok (signals (cl-nanoid:generate :alphabet (make-string 256 :initial-element #\x)) 'error)))) ;; too long

(deftest validation-size-positive
  (testing "Size must be positive"
    (ok (signals (cl-nanoid:generate :size 0) 'error))
    (ok (signals (cl-nanoid:generate :size -5) 'error))))
