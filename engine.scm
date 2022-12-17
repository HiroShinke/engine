

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
	   

		
		  
		
   
