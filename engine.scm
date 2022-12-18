

(define start-timer #f)
(define stop-timer #f)
(define decrement-timer #f)

(let ((clock 0)
      (handler #f))
  (set! start-timer
	(lambda (ticks new-handler)
	  (set! handler new-handler)
	  (set! clock ticks)
	  ))
  (set! stop-timer
	(lambda ()
	  (let ((time-left clock))
	    (set! clock 0)
	    time-left)))
  (set! decrement-timer
	(lambda ()
	  (if (> clock 0)
	      (begin
		(set! clock (- clock 1))
		(if (= clock 0) (handler)))))))
;;
;;  proc : () -> X
;;  complete : ticks -> X -> A
;;  expire : engine -> B
;;
;;  thunk : () -> X
;;  make-engine: thunk -> (ticks -> complete -> expire -> A|B)
;;  resume : ticks -> A
;;  engine: ticks -> complete -> expire -> A|B
;;
;;  escape : (() -> A|B) ) -> A|B
;;  do-complete : ticks -> X ->  A
;;  do-expire : (ticks -> A|B)  -> B
;;
;;  new-engine: (ticks -> A)  -> (ticks -> complete -> expire -> A|B )
;;

(define make-engine
  (let ((do-complete #f)
	(do-expire #f))
    (define timer-handler
      (lambda ()
	(start-timer (call/cc do-expire)
		     timer-handler)))
    (define new-engine
      (lambda (resume)
	(lambda (ticks complete expire)
	  ((call/cc
	    (lambda (escape)
	      (set! do-complete
		    (lambda (ticks value)
		      (escape (lambda () (complete ticks value)))))
	      (set! do-expire
		    (lambda (resume)
		      (escape (lambda () (expire (new-engine resume))))))
	      (resume ticks)))))))
    
    (lambda (proc)
      (new-engine
       (lambda (ticks)
	 (start-timer ticks timer-handler)
	 (let ((value (proc)))
	   (let ((ticks (stop-timer)))
	     (do-complete ticks value))))))))


(define fibonacci
  (lambda (n)
    (decrement-timer)
    (if (< n 2)
	n
	(+ (fibonacci (- n 1))
	   (fibonacci (- n 2))))))

(define-syntax timed-lambda
  (syntax-rules ()
    ((_ formals exp1 exp2 ...)
     (lambda formals (decrement-timer) exp1 exp2 ...))))


(define fibonacci2
  (timed-lambda
   (n)
   (if (< n 2)
       n
       (+ (fibonacci (- n 1))
	  (fibonacci (- n 2))))))

(define mileage
  (lambda (thunk)
    (let loop ((eng (make-engine thunk))
	       (total-ticks 0))
      (eng 50
	   (lambda (ticks value)
	     (+ total-ticks (- 50 ticks)))
	   (lambda (new-eng)
	     (loop new-eng (+ total-ticks 50))))
      )
    ))

(define round-robin
  (lambda (engs)
    (if (null? engs)
	'()
	((car engs) 1
	 (lambda (ticks value)
	   (cons value (round-robin (cdr engs))))
	 (lambda (eng)
	   (round-robin
	    (append (cdr engs) (list eng)))))
	)
    )
  )
  

(define test1
  (lambda ()
    (round-robin
     (map (lambda (x)
	    (make-engine
	     (lambda () (fibonacci2 x))))
	  '(4 5 2 8 10 15 3 5 6 2)))
    ))
