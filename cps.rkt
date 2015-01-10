#lang racket
;;Continuation Passing Style Intermediate Representation
;;; Values are unit, pair, function, projection
(define (cps-pair car cdr) (list 'pair car cdr))
(define (cps-proj proj-num cons) (list 'proj cons proj-num))
(define (cps-function cont-var var body) (list 'func cont-var var body))
(define (cps-unit) (list 'unit))
(define (cps-tag tag exp) (list 'tag tag exp))

;;;; CPS Terms

(define (term-type term) (car term))
(define (new-term type val) (cons type val))

(define (letval var val in) (new-term 'var (list var val in)))
(define (letcont cont-name cont-var val in) (new-term 'cont (list cont-name cont-var val in)))
(define (letproj var proj cons term) (new-term 'proj (list var cons term)))
(define (app f k x) (new-term 'app (list f k x)))
(define (cont-app k x) (new-term 'cont-app (list k x)))
(define (case x consequent alternative) (new-term 'case (list x consequent alternative)))


;; Source Language
(define (new-source-term type term) (cons 'source (cons type term)))
(define (source-term-type term) (cond ((not (pair? term)) 'var)
				      ((eq? (car term) 'source) (cadr term))
				      (else (error "NOT A SOURCE TERM" term))))
(define (source-unit) (new-source-term 'unit '()))

(define (source-var name) (new-source-term 'var (list name)))
(define (source-var-name var) (caddr var))

(define (source-app func-exp applicant-exp) (new-source-term 'app (list func-exp applicant-exp)))
(define (source-func-name source-term) (caddr source-term))
(define (source-applicant source-term) (cadddr source-term))

(define (source-func var exp) (new-source-term 'func (list var exp)))
(define (source-func-var term) (caddr term))
(define (source-func-body term) (cadddr term))

(define (source-pair car cdr) (new-source-term 'pair (list car cdr)))
(define (source-car term) (caddr term))
(define (source-cdr term) (cadddr term))

(define (source-proj num cons) (new-source-term 'proj (list num cons)))
(define (source-proj-num term) (caddr term))
(define (source-proj-exp term) (cadddr term))

(define (source-let-val var exp in) (new-source-term 'let-val (list var exp in)))
(define (source-let-val-var term) (list-ref term 2))
(define (source-let-val-exp term) (list-ref term 3))
(define (source-let-val-in term) (list-ref term 4))

(define (source-tag pair tag)  (new-source-term 'tag (list exp (if (eq? num 1) 1 2))))
(define (source-tag-tag term) (list-ref term 3))
(define (source-tag-exp term) (list-ref term 2))

(define (source-case inj var1 consq var2 alt) (new-source-term 'case (list inj var1 var2 consq alt)))
(define (case-exp term) (list-ref term 2))
(define (case-var1 term) (list-ref term 3))
(define (case-var2 term) (list-ref term 4))
(define (case-consq1 term) (list-ref term 5))
(define (case-consq2 term) (list-ref term 6))

(define (transform source-term context)
  (display source-term)
  (newline)
  (cond ((eq? (source-term-type source-term) 'var)
	 (context source-term))
	((eq? (source-term-type source-term) 'unit)
	 (let ((name (gensym))) (letval name (cps-unit) (context name))))
	((eq? (source-term-type source-term) 'app)
	 (transform (source-func-name source-term)
		    (lambda (f)
		      (transform (source-applicant source-term)
				 (lambda (x)
				   (let ((cont-name (gensym))
					 (cont-var (gensym)))
				     (letcont cont-name cont-var (context cont-var) (app f cont-name x))))))))
	((eq? (source-term-type source-term) 'pair)
	 (transform
	  (source-car source-term)
	  (lambda (car)
	    (transform (source-cdr source-term)
		       (lambda (cdr)
			 (let ((x (gensym)))
			   (letval x (cps-pair car cdr) (context x))))))))
	((eq? (source-term-type source-term) 'tag)
	 (transform (source-term-tag-exp source-term) (lambda (z) (let ((x (gensys)))
								    (letval x (cps-tag (source-term-tag-tag source-term) z) (context x))))))
	((eq? (source-term-type source-term) 'proj)
	 (transform (source-term-proj-exp source-term)
		    (lambda (z) (let ((x (gensys)))
				  (letproj x (cps-proj x (source-term-proj-num source-term) z) (context x))))))
	((eq? (source-term-type source-term) 'func)
	 (let ((f (gensym))
	       (k (gensym))) (letval f
				     (cps-function k
						   (source-func-var source-term)
						   (transform (source-func-body source-term)
							      (lambda (var) (cont-app k var))))
				     (context f))))
	((eq? (source-term-type source-term) 'let-val)
	 (let ((k (gensym)))
	   (letcont k
		    (source-let-val-var source-term)
		    (transform (source-let-val-in source-term) context)
		    (transform (source-let-val-exp source-term) (lambda (v) (cont-app k v))))))
	((eq? (source-term-type source-term) 'case)
	 (transform (case-exp source-term)
		    (lambda (z)
		      (let ((k1 (gensym)))
			(letcont k1 (case-var1 source-term) (transform (case-consq1 source-term) context)
				 (let ((k2 (gensym)))
				   (letcont k2 (case-var2 source-term) (transform (case-consq2 source-term) context)
					    (case z k1 k2))))))))))

;;(transform (source-func 'y (source-pair 'y 'y)) (lambda (x) x))

;;(transform (source-func 'x (source-app 'f (source-pair 'x 'y))) (lambda (x) x))
(transform (source-app 'f (source-case 'x 'x1 'e1 'x2 'e2)) (lambda (x) x))
