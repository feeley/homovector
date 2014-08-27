;;;============================================================================

;;; File: "homovector.scm"

;;; Copyright (c) 2006-2014 by Marc Feeley, All Rights Reserved.

;;;============================================================================

;; Provides procedures to operate on homogeneous vectors.

;;;============================================================================

(declare
 (standard-bindings)
 (extended-bindings)
 (fixnum)
 (not safe))

;;;----------------------------------------------------------------------------

(define (ISO-8859-1-substring->u8vector str start end)
  (let* ((len (- end start))
         (u8vect (make-u8vector len)))
    (let loop ((i 0))
      (if (< i len)
          (begin
            (u8vector-set!
             u8vect
             i
             (char->integer (string-ref str (+ start i))))
            (loop (+ i 1)))
          u8vect))))

(define (ISO-8859-1-string->u8vector str)
  (ISO-8859-1-substring->u8vector
   str
   0
   (string-length str)))

(define (subu8vector->ISO-8859-1-string u8vect start end)
  (let* ((len (- end start))
         (str (make-string len)))
    (let loop ((i 0))
      (if (< i len)
          (begin
            (string-set!
             str
             i
             (integer->char (u8vector-ref u8vect (+ start i))))
            (loop (+ i 1)))
          str))))

(define (u8vector->ISO-8859-1-string u8vect)
  (subu8vector->ISO-8859-1-string
   u8vect
   0
   (u8vector-length u8vect)))

(define (hex-substring->u8vector str start end)

    (define (char->digit c)
      (cond ((and (char>=? c #\0) (char<=? c #\9))
             (- (char->integer c) (char->integer #\0)))
            ((and (char>=? c #\a) (char<=? c #\f))
             (+ 10 (- (char->integer c) (char->integer #\a))))
            ((and (char>=? c #\A) (char<=? c #\F))
             (+ 10 (- (char->integer c) (char->integer #\A))))
            (else
             #f)))

  (let ((n (- end start)))
    (if (odd? n)
        (error "string length must be even")
        (let* ((len (quotient n 2))
               (u8vect (make-u8vector len)))
          (let loop ((i 0) (j (- len 1)))
            (if (>= j 0)
                (let ((hi4 (char->digit (string-ref str i)))
                      (lo4 (char->digit (string-ref str (+ i 1)))))
                  (if (or (not hi4)
                          (not lo4))
                      (error "string must contain hex digits only")
                      (begin
                        (u8vector-set!
                         u8vect
                         j
                         (+ (* 16 hi4) lo4))
                        (loop (+ i 2) (- j 1)))))
                u8vect))))))

(define (hex-string->u8vector str)
  (hex-substring->u8vector
   str
   0
   (string-length str)))

(define (subu8vector->hex-string u8vect start end)

  (define (digit->char d)
    (string-ref "0123456789abcdef" d))

  (let* ((len (- end start))
         (n (* len 2))
         (str (make-string n)))
    (let loop ((i 0) (j (- len 1)))
      (if (>= j 0)
          (let ((x (u8vector-ref u8vect j)))
            (string-set! str i (digit->char (quotient x 16)))
            (string-set! str (+ i 1) (digit->char (modulo x 16)))
            (loop (+ i 2) (- j 1)))
          str))))

(define (u8vector->hex-string u8vect)
  (subu8vector->hex-string
   u8vect
   0
   (u8vector-length u8vect)))

;;;============================================================================
