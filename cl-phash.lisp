(in-package #:cl-phash)

(defun fixnum-as-bit-vector (n &key size (offset 0) write-into)
  (do* ((bit-length (or (and write-into (length write-into))
                        size
                        (* 8 (integer-length n))))
        (bit-vector (or write-into
                        (make-array bit-length :element-type 'bit)))
        (bitv-i (1- bit-length) (1- bitv-i))
        (n-i offset (1+ n-i)))
       ((< bitv-i 0) bit-vector)
    (setf (aref bit-vector bitv-i)
          (if (logbitp n-i n) 1 0))))

(defun bit-at (bitv idx)
  (aref bitv (- (length bitv) 1 idx)))

(defun bit-hash (val)
  (fixnum-as-bit-vector val :size 35))

(defun level-index (val level)
  (let ((ret 0))
    (dotimes (i 5 ret)
      (if (> (bit-at val (+ i level)) 0)
          (setf ret (+ ret (expt (* 2 (bit-at val (+ i level))) i))))
    )))

(defun make-array-node ()
  (list :array-node (make-array 32 :initial-element nil)))

(defun clone-array-node (node)
  (list :array-node (copy-seq (cadr node))))

(defun array-node-p (node)
  (eq :array-node (car node)))

(defun make-value-node (key value)
  (list :value-node key value (bit-hash key)))

(defun value-node-p (node)
  (eq :value-node (car node)))

(defun empty-map () (make-array-node))

(defun m-get (m key)
  (m-get-level m key (bit-hash key) 0))

(defun m-get-level (node key bits level)
  (let* ((idx (level-index bits level))
         (sub-node (aref (cadr node) idx)))
    (if sub-node
        (cond
          ((value-node-p sub-node)
           (if (equal key (cadr sub-node))
               (caddr sub-node)
               nil))
          ((array-node-p sub-node)
           (m-get-level sub-node key bits (+ level 5)))
          (t (format nil "should not happen ~a~%" key)))
        nil)))

(defun m-set (m key value)
  (m-set-level m key (bit-hash key) value 0))

(defun m-set-level (node key bits value level)
  (let* ((idx (level-index bits level))
         (sub-node (aref (cadr node) idx))
         )
    (if sub-node
        (cond
          ((array-node-p sub-node)
           (let ((clone (clone-array-node node)))
             (setf (aref (cadr clone) idx) (m-set-level sub-node key bits value (+ level 5)))
             clone
             ))

          ((value-node-p sub-node)
           (let ((clone (clone-array-node node)))
             (if (equal (cadr sub-node) key)
                 (progn
                   (setf (aref (cadr clone) idx) (make-value-node key value))
                   clone
                   )
                 (let ((new-arr-node (make-array-node)))
                   (setf (aref (cadr new-arr-node) (level-index (cadddr sub-node) (+ level 5))) sub-node)
                   (setf (aref (cadr clone) idx) new-arr-node)
                   (setf (aref (cadr clone) idx) (m-set-level new-arr-node key bits value (+ level 5)))
                   clone
                   ))))
          (t "cannot happen")
          )
        (let ((clone (clone-array-node node)))
          (setf (aref (cadr clone) idx) (make-value-node key value))
          clone
          ))))

(defun m-del (m key)
  (m-del-level m key (bit-hash key) 0))

(defun m-del-level (node key bits level)
  (let* ((idx (level-index bits level))
         (sub-node (aref (cadr node) idx))
         )
    (if sub-node
        (cond
          ((array-node-p sub-node)
           (let ((clone (clone-array-node node)))
             (setf (aref (cadr clone) idx) (m-del-level sub-node key bits (+ level 5)))
             clone
             ))

          ((value-node-p sub-node)
           (if (equal (cadr sub-node) key)
               (let ((clone (clone-array-node node)))
                   (setf (aref (cadr clone) idx) nil)
                   clone)
                 node))
          (t "cannot happen")
          )
        node)))

(defun m-keys (m)
  (apply #'concatenate 'list
         (map 'list #'(lambda (node)
                          (cond
                            ((eq node nil) nil)
                            ((value-node-p node) (list (cadr node)))
                            (t (m-keys node))
                            ))
              (cadr m))))

(defun m-count (m)
  (length (m-keys m)))
