#!/usr/bin/clisp

; size of c types in bytes
(defparameter *B-char-size* 1)

; size of c types in bits
; if we switch platforms this has to be changed
(defparameter *int-size* (* 8 4))
(defparameter *float-size* (* 8 4))
(defparameter *char-size* (* 8 *B-char-size*))
(defparameter *actiontype-size* (* 8 4))
(defparameter *card-size* (* *char-size* 2))

; This is bad
(defvar *stream* nil)

; TODO endianess ?

(defun repeat-call (fn times)
;	(cond
;	 ((= times 1) (cons (funcall fn) nil))
;	 ((> times 1) (cons (funcall fn) (repeat-call fn (- times 1)))) )

	(labels ((repeat-call-aux (times acc)
														(cond

														 ; Building lists in inverse order
														 ; and then reverse them is better
														 ; than appending
														 ; http://stackoverflow.com/questions/6439972/what-is-the-cons-to-add-an-item-to-the-end-of-the-list
														 ((= times 0) (nreverse acc))

														 ((>= times 1)
															(let* ((r (funcall fn))
																		 (acc2 (cons r acc))
																		 (times2 (- times 1)) )
																(repeat-call-aux times2 acc2)
																))

														 (t (error "dont give me negative times!"))
														 )))
		(repeat-call-aux times '())
		) )

(compile 'repeat-call)

(defun mk-buffer (size)
	(make-array size :element-type '(unsigned-byte 8)))

;; Scalars

(defun read-id ()
		(read-integer *stream* `(unsigned-byte ,*int-size*)))

(defun read-action-type ()
		(let ((value (read-integer *stream* `(unsigned-byte ,*actiontype-size*))))
			(cond
			 ((= value 0) 'fold)
			 ((= value 1) 'call)
			 ((= value 2) 'check)
			 ((= value 3) 'raise)
			 ((= value 4) 'bet)
			 ((= value 5) 'small-blind)
			 ((= value 6) 'big-blind)
			 (t (error "Unexpected action type"))
			 )))

(defun read-value ()
	(read-float *stream* 'single-float))

;; sizes are single chars.
;; Implicit maximum of 256 chars for strings and arrays
(defun read-size ()
	(let ((buf (mk-buffer *B-char-size*)))
			(read-sequence buf *stream*)
			(reduce (lambda (tot i) (+ i (ash tot 8))) buf :initial-value 0)))

(defun read-bool ()
	(if (= (read-size) 0)
			nil
		t))

; C String

(defun read-cstring ()
	(let* ((size (read-size))
				 (buf (mk-buffer (* size *B-char-size*))))
		(read-sequence buf *stream*)
		(map 'string #'code-char buf)
		))

;; Action

(defun read-action ()
	(cons
	 (read-cstring)
	 (read-action-type)))

(defun read-actions ()
	(let ((size (read-size)))
				;(print size)
				(repeat-call #'read-action size)
				))

;; Card

(defun read-card ()
	(cons (code-char (read-byte *stream*))
				(code-char (read-byte *stream*))))

(defun read-cards-n (n)
	(repeat-call #'read-card n))

;; Player

(defun read-player ()
	(list (read-cstring)
				(read-value)
				(read-bool)
				(repeat-call #'read-card 2)))

(defun read-players ()
	(let ((size (read-size)))
		(repeat-call #'read-player size)))

; hash:
;   id - int
;   sb - float
;   bb - float
;   preflop - alist('cards -> nil, 'actions -> ...)
;   flop - alist('cards -> nil, 'actions -> ...)
;   turn - alist('cards -> ..., 'actions -> ...)
;   river - alist('cards -> ..., 'actions -> ...)
;   players - hash:
;               <name> - stack, isbutton, cards
;               ...
(defun make-hand (id sb bb pf-actions f-actions f-cards
										 t-actions t-cards r-actions r-cards
										 players)
	(let ((h (make-hash-table)))
		(setf (gethash 'id h) id)
		(setf (gethash 'sb h) sb)
		(setf (gethash 'bb h) bb)
		(setf (gethash 'preflop h)
					; a-list here, for now...
					`((cards . nil) (actions . ,pf-actions)) )
		(setf (gethash 'flop h) 
					; a-list here, for now...
					`((cards . ,f-cards) (actions . ,f-actions)) )
		(setf (gethash 'turn h) 
					; a-list here, for now...
					`((cards . ,t-cards) (actions . ,t-actions)) )
		(setf (gethash 'river h) 
					; a-list here, for now...
					`((cards . ,r-cards) (actions . ,r-actions)) )
		(setf (gethash 'players h) 
					; create hash of hashes from the lists
					; player-name (car of the list) is the key
					(reduce (lambda (tot x)
										(setf (gethash (car x) tot) 
													; Create another hash, with the player's data
													(let ((data (cdr x)) 
																(h-data (make-hash-table)))
														(setf (gethash 'stack h-data) (car data))
														(setf (gethash 'is-button h-data) (cadr data))
														(setf (gethash 'cards h-data) (caddr data))
														h-data
														))
										tot)
									players
									:initial-value (make-hash-table))
					)
		h	
		)
	)

(compile 'make-hand)

; Read a hand from a stream
(defun read-hand (callback)
	(funcall callback (make-hand
						    ; hand-id
										 (read-id)
           			; small blind
										 (read-value)
           			; big blind
										 (read-value)
           			; actions preflop
										 (read-actions)
           			; actions flop
										 (read-actions)
           			; cards flop
										 (read-cards-n 3)
           			; actions turn
										 (read-actions)
           			; cards turn
										 (read-cards-n 1)
           			; actions river
										 (read-actions)
			          ; cards river
										 (read-cards-n 1)
			          ; players
										 (read-players) ) ) )

(defun parse-hand (hand)
	;(print
	 ;(cdr (assoc 'flop hand))
	 ;)

	;(print
	 ;(cdr (assoc 'cards (cdr (assoc 'flop hand))))
	 ;)
	(print hand)
	)

; Read a file, call read-hand until we reach the end of the file ...
; TODO Make this a lazy list?
(defun read-file (filename)
	(with-open-file (*stream* filename
													:direction :input
													:element-type '(unsigned-byte 8))
									;(print (read-hand))
									;(print (call-until-eof #'read-hand))))
									(let ((len (file-length *stream*)))
										  ; 1.a
										  ; Recursive with tail-recursion
										  ; to actually work we need to compile the
										  ; read-file function. Check comment 1.b
											(labels ((read-all ()
																				 ;(print len)
																				 ;(print (file-position *stream*))
																				 (if (< (file-position *stream*) len)
																						 (progn
																							 (read-hand #'parse-hand)
																							 (read-all))
																					 nil)))
												(read-all)
												)
										)))

; 1.b
; Compile read-file so we can enjoy
; tail-recursion optimization
; Check comment 1.a
(compile 'read-file)

(defun main()
	(read-file "in/000.phb"))

(main)
