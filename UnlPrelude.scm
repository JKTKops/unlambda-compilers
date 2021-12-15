(define-record-type promise
  (fields thunk))

(define-syntax promise
  (syntax-rules ()
    ((_ F) (make-promise (lambda () F)))))

(define (force f)
  (if (promise? f)
      ((promise-thunk f))
      f))

(define-syntax force-apply
  (syntax-rules ()
    ((_ F G)
     (let ((f F) (g-p (promise G)))
       (if (eq? f d)
           g-p
           ; in this case, we have to force g _before_ we force f
           ; per the unlambda semantics. It's kinda weird.
           (let ((g (force g-p)))
             ((force f) g)))))))

(define-syntax dot
  (syntax-rules ()
    ((dot c) (lambda (x) (write-char c) x))))

(define i (lambda (x) x))
(define k (lambda (x) (lambda (_) x)))
(define s
  (lambda (x)
    (lambda (y)
      (lambda (z)
        (force-apply (force-apply x z) (force-apply y z))))))
(define v (lambda (_) v))
; Bind d to an identity function; this can matter with e.g.
; ```dd`.ai, where the promise `dd is forced after only after
; having checked that it did not evaluate to d (it didn't!)
; and is then forced to d and applied. The technical semantics
; of this is to create a promise for the value of the argument.
; But the argument is already evaluated, so we can just return
; it directly instead.
(define d (lambda (x) x))
(define c
  (lambda (f)
    (call/cc (lambda (k)
               ((force f) k)))))
(define r (dot #\newline))
(define e (lambda (_) (exit)))

(define current-character #f)
(define @ 
  (lambda (x)
    (let ((c (read-char)))
      (if (eof-object? c)
          (begin (set! current-character #f)
                 (force-apply x v))
          (begin (set! current-character c)
                 (force-apply x i))))))

(define (? c)
  (lambda (x)
    (if (eqv? c current-character)
        (force-apply x i)
        (force-apply x v))))

;;; we can't call the function `|` because Gambit interprets
;;; |...| as a verbatim symbol and we would be leaving it
;;; unclosed.
(define (pipe x) (force-apply x (dot current-character)))