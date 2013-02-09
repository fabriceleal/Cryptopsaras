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

; TODO endianess ?

(defun repeat-call (fn times)
	(cond
	 ((= times 1) (cons (funcall fn) nil))
	 ((> times 1) (cons (funcall fn) (repeat-call fn (- times 1)))) ))

(defun mk-buffer (size)
	(make-array size :element-type '(unsigned-byte 8)))


(defun read-id (stream)
		(read-integer stream `(unsigned-byte ,*int-size*)))

(defun read-value (stream)
	(read-float stream 'single-float))

; sizes are single chars. 
; Implicit maximum of 256 chars for strings and arrays
(defun read-size (stream)
	(let ((buf (mk-buffer *B-char-size*))) 
			(read-sequence buf stream)
			(reduce (lambda (tot i) (+ i (ash tot 8))) buf :initial-value 0)))

(defun read-cstring (stream)
	(let* ((size (read-size stream)) 
				 (buf (mk-buffer (* size *B-char-size*))))
		(read-sequence buf stream)
		(print buf)
		buf
		))

(defun read-action (stream)
	(cons 
	 (read-cstring stream)
	 (read-id stream)))

; Read a hand from a stream
(defun read-hand (stream)
	(let ((id (mk-buffer *int-size*)))
      ; hand-id
			(print (read-id stream))
			; small blind
			(print (read-value stream))
			; big blind
			(print (read-value stream))
			; actions preflop
			(let ((preflop-size (read-size stream))) 
				(print preflop-size)
				(print (repeat-call (lambda () (read-action stream)) preflop-size)) 
				)
			; actions flop
			;(let ((flop-size (read-size stream)))
			;	(print flop-size)
			;	(print (repeat-call (lambda () (read-action stream)) flop-size)) )
			))

; Read a file, call read-hand until we reach the end of the file ...
; TODO Make this a lazy list?
(defun read-file (filename)
	(with-open-file (stream filename :direction :input :element-type '(unsigned-byte 8))
									(read-hand stream)))

(defun main() 
	(read-file "in/0.phb"))


(main)