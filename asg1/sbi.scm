#!/afs/cats.ucsc.edu/courses/cmps112-wm/usr/racket/bin/mzscheme -qr
;; CruzID: 1601119
;; $Id: sbi.scm,v 1.4 2018-04-11 16:31:36-07 - - $
;;
;; NAME
;;    sbi.scm - silly basic interpreter
;;
;; SYNOPSIS
;;    sbi.scm filename.sbir
;;
;; DESCRIPTION
;;    The file mentioned in argv[1] is read and assumed to be an SBIR
;;    program, which is the executed.  Currently it is only printed.
;;

(define *stdin* (current-input-port))
(define *stdout* (current-output-port))
(define *stderr* (current-error-port))

(define *run-file*
    (let-values
        (((dirpath basepath root?)
            (split-path (find-system-path 'run-file))))
        (path->string basepath))
)

(define (die list)
    (for-each (lambda (item) (display item *stderr*)) list)
    (newline *stderr*)
    (exit 1)
)

(define (usage-exit)
    (die `("Usage: " ,*run-file* " filename"))
)

(define (readlist-from-inputfile filename)
    (let ((inputfile (open-input-file filename)))
         (if (not (input-port? inputfile))
             (die `(,*run-file* ": " ,filename ": open failed"))
             (let ((program (read inputfile)))
                  (close-input-port inputfile)
                         program))))

(define (write-program-by-line filename program)
    (printf "==================================================~n")
    (printf "~a: ~s~n" *run-file* filename)
    (printf "==================================================~n")
    (printf "(~n")
    (map (lambda (line) (printf "~s~n" line)) program)
    (printf ")~n"))

(define *symbol-table* (make-hash))
(define *variable-table* (make-hash))
(define *label-table* (make-hash))
(define *label-table-line* (make-hash))

(define (symbol-get key) (hash-ref *symbol-table* key))
(define (variable-get key) (hash-ref *variable-table* key))
(define (label-get key) (hash-ref *label-table* key))
(define (label-line-get key) (hash-ref *label-table-line* key))

(hash-set! *variable-table* "pi" 
    3.141592653589793238462643383279502884197169399)
(hash-set! *variable-table* "e" 
    2.718281828459045235360287471352662497757247093)
(hash-set! *variable-table* "log10_2" 
    0.301029995663981195213738894724493026768189881)
(hash-set! *variable-table* "sqrt_2" 
    1.414213562373095048801688724209698078569671875)


(define (symbol-put! key value) (hash-set! *symbol-table* key value))

(for-each
    (lambda (pair)
        (symbol-put! (car pair) (cadr pair)))
    `(
        (pi, 3.141592653589793238462643383279502884197169399)
        (e, 2.718281828459045235360287471352662497757247093)
        (% ,(lambda (x y) (- x (* (/ x y) y))))
        (/ ,(lambda (x y) (/ (+ x 0.0) (+ y 0.0))))
        (+ ,+)
        (-, -)
        (*, *)
        (=, equal?)
        (<, <)
        (<>, (lambda (x y) (not (equal? x y))))
        (>=, >=)
        (<=, <=)
        (^ ,expt)
        (exp ,exp)
        (floor ,floor)
        (ceil, ceiling)
        (round, round)
        (log ,log)
        (sqrt ,sqrt)
        (log , (lambda (x) (log (+ x 0.0))))
        (log10 ,(lambda (x) (/ (log x) (log 10.0))))
        (floor, floor)
        (atan , atan)
        (sin, sin)
        (cos, cos)
        (tan, tan)
        (acos, acos)
        (asin, asin)
        (atan, atan)
        (abs, abs)

 ))

(define (evaluate-expr expr)
  (cond ((number? expr)
              expr)
        ((symbol? expr)
              (symbol-get expr))
        ((pair? expr)
            (if (hash-has-key? *symbol-table* (car expr))
                (begin
                (let ((sym (symbol-get (car expr)))
                    (tail (cdr expr)))
                    (apply sym (map evaluate-expr tail))))
                (if (hash-has-key? *variable-table* (car expr))
                    (vector-ref (variable-get (car expr)) 
                          (- (evaluate-expr (cadr expr)) 1))
                    (display "error"))))))

(define (interpret-print toPrint) 
    (when (not (null? toPrint))
        (if (string? (car toPrint))
                (display (car toPrint))
                (display (evaluate-expr (car toPrint))))
        (when (not (null? (cdr toPrint)))
            (interpret-print (cdr toPrint)))))

(define (interpret-let variable expr)
    (if (symbol? variable)
        (hash-set! *symbol-table* variable (evaluate-expr (car expr)))
        (when (pair? variable)
            (vector-set! (variable-get (car variable)) 
                         (-(evaluate-expr (car (cdr variable))) 1) 
                         (evaluate-expr (car expr))))))

(define (interpret-dim value size)
    (hash-set! *variable-table* value 
               (make-vector (evaluate-expr (car size)))))

(define (interpret-goto label program line-num)
    (when (hash-has-key? *label-table* (car label))
        (if (null? (label-get (car label)))
            (void)
            (interpret-statement
            (list-ref program (- (label-line-get (car label)) 1)) 
             program (label-line-get (car label))))
        (interpret-program program (+ (label-line-get (car label)) 1))))

(define (interpret-if condition label program line-num)
    (begin
    (when (eqv? (car condition) '=)
        (when (= (evaluate-expr (car (cdr condition))) 
                 (evaluate-expr (car (cdr (cdr condition)))))
            (interpret-goto label program line-num)))
    (when (eqv? (car condition) '<)
        (when (< (evaluate-expr (car (cdr condition))) 
                 (evaluate-expr (car (cdr (cdr condition)))))
            (interpret-goto label program line-num)))
    (when (eqv? (car condition) '<=)
        (when (<= (evaluate-expr (car (cdr condition))) 
                  (evaluate-expr (car (cdr (cdr condition)))))
            (interpret-goto label program line-num)))
    (when (eqv? (car condition) '>=)
        (when (>= (evaluate-expr (car (cdr condition))) 
                  (evaluate-expr (car (cdr (cdr condition)))))
            (interpret-goto label program line-num)))
    (when (eqv? (car condition) '>)
        (when (> (evaluate-expr (car (cdr condition))) 
                 (evaluate-expr (car (cdr (cdr condition)))))
            (interpret-goto label program line-num)))
    (when (eqv? (car condition) '<>)
        (when (not (= (evaluate-expr (car (cdr condition))) 
                   (evaluate-expr (car (cdr (cdr condition))))))
          (interpret-goto label program line-num)))))

(define (increment value)
    (let ((value (+ value 1))) value))

(define (interpret-input variable x)
  (begin
  (let ((in (read))) 
     (if (number? in)
       (begin
       (hash-set! *symbol-table* (car variable) in)
       (if (= in -1)
           (hash-set! *symbol-table* 'inputcount -1)
           (begin
           (hash-set! *symbol-table* 'inputcount (increment x))
                (if (not (null? (cdr variable)))
                   (interpret-input (cdr variable) (increment x))
                   (void)))))
       (void)))))

(define (interpret-statement line program line-num)
  (when (not(null? (cdr line)))
      (if (pair? (car (cdr line)))
          (begin
          (when (eqv? (car (cadr line)) 'print)
              (interpret-print (cdr (cadr line)))
              (printf "~n"))
          (when (eqv? (car (cadr line)) 'let)
              (interpret-let (car (cdr (cadr line))) 
                             (cdr (cdr (cadr line))))) 
          (when (eqv? (car (cadr line)) 'dim)
              (interpret-dim (car (car (cdr (cadr line)))) 
                             (cdr (car (cdr (cadr line)))))) 
          (when (eqv? (car (cadr line)) 'goto)
              (interpret-goto (cdr (cadr line)) program line-num))
          (when (eqv? (car (cadr line)) 'if)
              (interpret-if (car (cdr (car (cdr line)))) 
                      (cdr (cdr (car (cdr line)))) program line-num))
          (when (eqv? (car (cadr line)) 'input)
              (interpret-input (cdr (cadr line)) 0)))
          (begin
          (if (null? (cdr (cdr line)))
              (void)
              (begin
              (when (eqv? (car (car (cdr (cdr line)))) 'print)
                  (interpret-print (cdr (car (cdr (cdr line)))))
                  (printf "~n"))
              (when (eqv? (car (car (cdr (cdr line)))) 'let)
                  (interpret-let (car (cdr (car (cdr (cdr line))))) 
                                 (cdr (cdr (car (cdr (cdr line))))))) 
              (when (eqv? (car (cdr (cdr line))) 'dim)
                  (interpret-dim (car (cdr (cdr (cdr line)))) 
                                 (cdr (cdr (cdr (cddr line)))))) 
              (when (eqv? (car (cdr (cdr line))) 'goto)
                  (interpret-goto (cdr (cdr (cdr line))) 
                  program line-num))
              (when (eqv? (car (car (cdr (cdr line)))) 'if)
                  (interpret-if (car (cdr (car (cdr (cdr line))))) 
                        (cdr (cdr (car (cdr (cdr line))))) 
                        program line-num))
              (when (eqv? (car (car (cdr (cdr line)))) 'input)
                (interpret-input (cdr (car (cdr (cdr line)))) 0))))))))


(define (interpret-program program line-num) 
    (if ( <= line-num (length program))
        (begin
        (interpret-statement (list-ref program (- line-num 1))
                              program line-num)
        (interpret-program program (increment line-num)))
        (exit 0)))

(define (make-label-table program counter program-length)
  (when (<= counter program-length)
    (when (not (null? (cdr (list-ref program (- counter 1)))))
       (when (not (pair? (car (cdr (list-ref program (- counter 1)))))) 
          (if (null? (cdr (cdr (list-ref program (- counter 1)))))
              (hash-set! *label-table* 
              (car (cdr (list-ref program (- counter 1)))) null)
           
              (hash-set! *label-table* 
              (car (cdr (list-ref program (- counter 1)))) 
              ( cdr (cdr (list-ref program (- counter 1))))))

            (hash-set! *label-table-line* 
            (car (cdr (list-ref program (- counter 1)))) counter)))

        (make-label-table program (increment counter) program-length)))

(define (main arglist)
    (if (or (null? arglist) (not (null? (cdr arglist))))
        (usage-exit)
        (let* ((sbprogfile (car arglist))
              (program (readlist-from-inputfile sbprogfile)))
              (make-label-table program 1 (length program))
              (interpret-program program 1))))

;;(when (terminal-port? *stdin*)
      (main (vector->list (current-command-line-arguments)))


