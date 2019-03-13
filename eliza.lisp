  ;;PAIP Chap.5


(defun pat-match (pattern input)
  (if (variable-p pattern)
	t
	(if (or (atom pattern ) (atom input))
	  (e  pattern input)
	  (and (pat-match (car pattern) (car input))
		   (pat-match (cdr pattern) (cdr input))))))

(defun variable-p (x)
  (and (symbolp x) (equal (char (symbol-name x) 0) #\?)))

(defconstant fail nil)
(defconstant no-bindings '((t . t)))

(defun get-binding (var bindings)
  (assoc var bindings))

(defun binding-val (binding)
  (cdr binding))


(defun match-variable (var input bindings)
  (let ((binding (get-binding var bindings)))
	(cond ((not binding) (extend-bindings var input bindings))
		  ((equal input (binding-val binding)) bindings)
		  (t fail))))


(defun extend-bindings (var val bindings)
  (cons (cons var val)
		(if (eq bindings no-bindings)
		  nil
		  bindings)))

(defun pat-match (pattern input &optional (bindings no-bindings))
  (cond ((eq bindings fail) fail)
		((variable-p pattern)
		 (match-variable pattern input bindings))
		((eql pattern input) bindings)
		((segment-pattern-p pattern)
		 (segment-match pattern input bindings))
		((and (consp pattern) (consp input))
		 (pat-match (cdr pattern) (cdr input)
					(pat-match (car pattern) (car input) bindings)))
		(t fail)))

(defun segment-pattern-p (pattern)
  (and (consp pattern)
	   (starts-with (car pattern) '?*)))

(defun starts-with (lst symb)
  (if (consp lst)
	(eql (car lst) symb)
	(eql lst symb)))

(defun segment-match (pattern input bindings &optional (start 0))
  (let ((var (cadr (car pattern)))
		(pat (cdr pattern)))
	(if (null pat)
	  (match-variable var input bindings)
	  (let ((pos (position (car pat) input :start start :test #'equal)))
		(if (null pos)
		  fail
		  (let ((b2 (pat-match
					  pat (subseq input pos)
					  (match-variable var (subseq input 0 pos)
									  bindings))))
			(if (eq b2 fail)
			  (segment-match pattern input bindings (1+ pos))
			  b2)))))))

(defun rule-pattern (rule) (car rule))
(defun rule-responses (rule) (cdr rule))


;;; ==============================

(defparameter *eliza-rules*
 '((((?* ?x) hello (?* ?z))
    (How do you do.  Please state your problem. ?z))

   (((?* ?x))
    (Very interesting) (I am not sure I understand you fully)
    (What does that suggest to you?) (Please continue) (Go on)
(Do you feel strongly about discussing such things?))))

(defun eliza ()
  (load-db "funcionaowo")
  (loop
	(print 'eliza>)
	(write (readInstruction (read)  ;;PAIP Chap.5


(defun pat-match (pattern input)
  (if (variable-p pattern)
	t
	(if (or (atom pattern ) (atom input))
	  (e  pattern input)
	  (and (pat-match (car pattern) (car input))
		   (pat-match (cdr pattern) (cdr input))))))

(defun variable-p (x)
  (and (symbolp x) (equal (char (symbol-name x) 0) #\?)))

(defconstant fail nil)
(defconstant no-bindings '((t . t)))

(defun get-binding (var bindings)
  (assoc var bindings))

(defun binding-val (binding)
  (cdr binding))


(defun match-variable (var input bindings)
  (let ((binding (get-binding var bindings)))
	(cond ((not binding) (extend-bindings var input bindings))
		  ((equal input (binding-val binding)) bindings)
		  (t fail))))


(defun extend-bindings (var val bindings)
  (cons (cons var val)
		(if (eq bindings no-bindings)
		  nil
		  bindings)))

(defun pat-match (pattern input &optional (bindings no-bindings))
  (cond ((eq bindings fail) fail)
		((variable-p pattern)
		 (match-variable pattern input bindings))
		((eql pattern input) bindings)
		((segment-pattern-p pattern)
		 (segment-match pattern input bindings))
		((and (consp pattern) (consp input))
		 (pat-match (cdr pattern) (cdr input)
					(pat-match (car pattern) (car input) bindings)))
		(t fail)))

(defun segment-pattern-p (pattern)
  (and (consp pattern)
	   (starts-with (car pattern) '?*)))

(defun starts-with (lst symb)
  (if (consp lst)
	(eql (car lst) symb)
	(eql lst symb)))

(defun segment-match (pattern input bindings &optional (start 0))
  (let ((var (cadr (car pattern)))
		(pat (cdr pattern)))
	(if (null pat)
	  (match-variable var input bindings)
	  (let ((pos (position (car pat) input :start start :test #'equal)))
		(if (null pos)
		  fail
		  (let ((b2 (pat-match
					  pat (subseq input pos)
					  (match-variable var (subseq input 0 pos)
									  bindings))))
			(if (eq b2 fail)
			  (segment-match pattern input bindings (1+ pos))
			  b2)))))))

(defun rule-pattern (rule) (car rule))
(defun rule-responses (rule) (cdr rule))


;;; ==============================

(defparameter *eliza-rules*
 '((((?* ?x) hello (?* ?z))
    (How do you do.  Please state your problem. ?z))

   (((?* ?x))
    (Very interesting) (I am not sure I understand you fully)
    (What does that suggest to you?) (Please continue) (Go on)
(Do you feel strongly about discussing such things?))))

(defun eliza ()
  (load-db "funcionaowo")
  (loop
	(print 'eliza>)
	(write (readInstruction (read)) :pretty t)))

(defun readInstruction (input)
  "Documentation for readInstruction."
  (cond
    ((equal input "inserta")
      (format t "por favor ingresa la pregunta: ~&" )
      (setq listasimple  (list (read)))
      (setq listadentro  (respuestas))
      (setq nuevalista (append  listasimple listadentro))
       ;(push  listasimple listadentro)
      (write nuevalista)
      (push nuevalista *eliza-rules*)
      ;y-or-n-p "Another? [y/n]: "
      (return-from readInstruction "pregunta(s) insertada(s)")
    )
    ((equal input "guarda")
      (save-db (read))
      (return-from readInstruction "archivo generado")
    )
    ((equal input "lee")
      (load-db (read))
      (return-from readInstruction "arhivo cargado")
    )
    ((equal input "ver")
      (dump-db)
      (return-from readInstruction "")
    )
    ((equal input "adios")
      (save-db "funcionaowo")
      (return-from readInstruction "archivo generado")
    )

  )
  (return-from readInstruction (flattern (use-eliza-rule input)))
)

(defun respuestas ()
  "Documentation for preguntas.
  permite al usuario definir más de una respuesta para una pregunta
  regresa una lista de respuestas"

    (setq respuesta ())
    (loop
      (print 'eliza>)
      (format t "por favor ingresa una respuesta: ~&" )
      (push (read) respuesta )
      (if (not (y-or-n-p "Deseas ingresar otra respuesta?"))
      (return-from respuestas respuesta))
    ))

(defun use-eliza-rule (input)
  (some #'(lambda (rule)
			(let ((result (pat-match (rule-pattern rule) input)))
			  (if (not (eq result fail))
				(sublis (switch-viewpoint result)
						(random-elt (rule-responses rule))))))
		*eliza-rules*))
(defun switch-viewpoint (words)
  (sublis '((yo . tu) (tu . yo) (mi . tu) (soy . estas))
		  words))

(defun random-elt (lst)
  (elt lst (random (length lst))))

(defun mappend (fn lst)
  (apply #'append (mapcar fn lst)))

(defun flattern (lst)
  (mappend #'mklist lst))

(defun mklist (x)
  (if (listp x)
	x
	(list x)))

(defun save-db (filename)
  (with-open-file (out filename
  :direction :output
  :if-exists :supersede)
  (with-standard-io-syntax
  (print *eliza-rules* out))))

(defun load-db (filename)
  (with-open-file (in filename)
  (with-standard-io-syntax
  (setf *eliza-rules* (read in)))))

(defun dump-db ()
  (format t "~{~{~a~&~2t~}~&~}" *eliza-rules*))
(eliza)
) :pretty t)))

(defun readInstruction (input)
  "Documentation for readInstruction."
  (cond
    ((equal input "inserta")
      (format t "por favor ingresa la pregunta: ~&" )
      (setq listasimple  (list (read)))
      (setq listadentro  (respuestas))
      (setq nuevalista (append  listasimple listadentro))
       ;(push  listasimple listadentro)
      (write nuevalista)
      (push nuevalista *eliza-rules*)
      ;y-or-n-p "Another? [y/n]: "
      (return-from readInstruction "pregunta(s) insertada(s)")
    )
    ((equal input "guarda")
      (save-db (read))
      (return-from readInstruction "archivo generado")
    )
    ((equal input "lee")
      (load-db (read))
      (return-from readInstruction "arhivo cargado")
    )
    ((equal input "ver")
      (dump-db)
      (return-from readInstruction "")
    )

  )
  (return-from readInstruction (flattern (use-eliza-rule input)))
)

(defun respuestas ()
  "Documentation for preguntas."

    (setq respuesta ())
    (loop
      (print 'eliza>)
      (format t "por favor ingresa una respuesta: ~&" )
      (push (read) respuesta )
      (if (not (y-or-n-p "Deseas ingresar otra respuesta?"))
      (return-from respuestas respuesta))
    )

    )

(defun use-eliza-rule (input)
  (some #'(lambda (rule)
			(let ((result (pat-match (rule-pattern rule) input)))
			  (if (not (eq result fail))
				(sublis (switch-viewpoint result)
						(random-elt (rule-responses rule))))))
		*eliza-rules*))
(defun switch-viewpoint (words)
  (sublis '((yo . tu) (tu . yo) (mi . tu) (soy . estas))
		  words))

(defun random-elt (lst)
  (elt lst (random (length lst))))

(defun mappend (fn lst)
  (apply #'append (mapcar fn lst)))

(defun flattern (lst)
  (mappend #'mklist lst))

(defun mklist (x)
  (if (listp x)
	x
	(list x)))

(defun save-db (filename)
  (with-open-file (out filename
  :direction :output
  :if-exists :supersede)
  (with-standard-io-syntax
  (print *eliza-rules* out))))

(defun load-db (filename)
  (with-open-file (in filename)
  (with-standard-io-syntax
  (setf *eliza-rules* (read in)))))

(defun dump-db ()
  (format t "~{~{~a~&~2t~}~&~}" *eliza-rules*))
(eliza)
