
(defparameter *node-index* 0)
(defmacro new-index ()
  `(incf *node-index*))

(defmacro ast-node (num label)
  `(format t "  astnode_~a[label=\"~a\"];~%" ,num ,label))
(defmacro connect-nodes (no ni)
  `(format t "  astnode_~a -> astnode_~a;~%" ,no ,ni))


(defun basic-program (file-name &rest members)
  (let ((cnum (new-index)))
	(format t "~%digraph G {~%")
	(dolist (mi members)
	  (connect-nodes cnum mi))
	(format t "}~%~%")
	cnum))

(defun basic-subroutine (name params body)
  (let ((cnum (new-index)))
	(format t " subgraph cluster_~a {~%" name)
	(ast-node cnum "SUBROUTINE")
	(connect-nodes cnum body)
	(format t "}~%")
	cnum))

(defun basic-sequence (&rest items)
  (let ((cnum (new-index)))
	(ast-node cnum "SEQUENCE")
	(dolist (si items)
	  (connect-nodes cnum si))
	cnum))

(defun basic-let (var value)
  (let ((cnum (new-index)))
	(ast-node cnum "LET")
	(connect-nodes cnum var)
	(connect-nodes cnum value)
	cnum))

(defun basic-input (prompt var)
  (let ((cnum (new-index)))
	(ast-node cnum "INPUT")
	(connect-nodes cnum prompt)
	(connect-nodes cnum var)
	cnum))

(defun basic-print (expr)
  (let ((cnum (new-index)))
	(ast-node cnum "PRINT")
	(connect-nodes cnum expr)
	cnum))

(defun basic-if (condition decision alternative)
  (let ((cnum (new-index)))
	(ast-node cnum "WHILE")
	(connect-nodes cnum condition)
	(connect-nodes cnum decision)
	(connect-nodes cnum alternative)
	cnum))

(defun basic-while (condition body)
  (let ((cnum (new-index)))
	(ast-node cnum "WHILE")
	(connect-nodes cnum condition)
	(connect-nodes cnum body)
	cnum))

(defun basic-for (parameter begin end step body)
  (let ((cnum (new-index)))
	(ast-node cnum "FOR")
	(connect-nodes cnum parameter)
	(connect-nodes cnum begin)
	(connect-nodes cnum end)
	(connect-nodes cnum step)
	(connect-nodes cnum body)
	cnum))

(defun basic-call (subr &rest args)
  (let ((cnum (new-index))
		(pnum (new-index)))
	(new-node cnum "CALL")
	(new-node pnum subr)
	(connect-nodes cnum pnum)
	(dolist (ai args)
	  (connect-nodex cnum ai))
	cnum))

(defun basic-apply (subr &rest args)
  (let ((cnum (new-index))
		(pnum (new-index)))
	(new-node cnum "APPLY")
	(new-node pnum subr)
	(connect-nodes cnum pnum)
	(dolist (ai args)
	  (connect-nodex cnum ai))
	cnum))

(defun basic-unary (opcode subexpro subexpri)
  (let ((cnum (new-index)))
	(ast-node cnum (format nil "'~a'" opcode))
	(connect-nodes cnum subexpro)
	(connect-nodes cnum subexpri)
	cnum))

(defun basic-unary (opcode subexpr)
  (let ((cnum (new-index)))
	(ast-node cnum (format nil "'~a'" opcode))
	(connect-nodes cnum subexpr)
	cnum))

(defun basic-variable (name)
  (let ((cnum (new-index)))
	(ast-node cnum (format nil "VARIABLE: ~a" name))
	cnum))

(defun basic-text (value)
  (let ((cnum (new-index)))
	(ast-node cnum (format nil "TEXT: '~a'" name))
	cnum))

(defun basic-number (value)
  (let ((cnum (new-index)))
	(ast-node cnum (format nil "NUMBER: ~a" value))
	cnum))


