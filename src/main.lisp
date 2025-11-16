(defpackage cl-nanoid
  (:use :cl)
  (:export #:generate
           #:+alphabet+))

(in-package cl-nanoid)

(defparameter +alphabet+ "_-0123456789abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ")
(defparameter +size+ 21)

(defun compute-mask (alphabet)
  (let ((power 1)
        (alphabet-length (length alphabet)))
    (loop :while (< power alphabet-length) :do (setf power (* 2 power)))
    (1- power)))

(defun default-random-bytes (n)
  "Return a vector of N cryptographically secure random bytes (0..255) using Ironclad's random-data."
  (ironclad:random-data n))
 
(defun generate (&key (alphabet +alphabet+) (size +size+) (random-bytes-fn #'default-random-bytes))
  "Generate a nanoid-style ID string of length SIZE using ALPHABET.
   RANDOM-BYTES-FN should be a function (N) -> vector of N bytes."

  (unless (stringp alphabet)
    (error "ALPHABET must be a string, got ~S." (type-of alphabet)))

  (let ((alphabet-length (length alphabet)))
    (when (or (< alphabet-length 2) (> alphabet-length 255))
        (error "ALPHABET must contain between 2 and 255 characters, got ~S." alphabet-length))

    (when (<= size 0)
        (error "SIZE must be positive, got ~S." size))

    (let ((seen (make-hash-table :test 'eql)))
        (dotimes (i alphabet-length)
            (let ((ch (aref alphabet i)))
                (when (gethash ch seen)
                    (error "ALPHABET contains duplicate character ~S." ch))
                    (setf (gethash ch seen) t))))

    (let* ((mask (compute-mask alphabet))
            (step (ceiling (* 1.6 mask size) alphabet-length))
            (result (make-string size))
            (pos 0))
        (loop
            (let ((bytes (funcall random-bytes-fn step)))
                (dotimes (i step)
                    (when (< pos size)
                        (let* ((byte (aref bytes i))
                               (index (logand byte mask)))
                            (when (< index alphabet-length)
                                (setf (aref result pos) (aref alphabet index))
                                (incf pos)
                                (when (= pos size)
                                    (return-from generate result)))))))))))
