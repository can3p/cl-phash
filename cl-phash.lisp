;;;; cl-journal.lisp

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

#|
(defun hash (val) val)

(defun mask (val level)
  (int<-
   (bit-and (>> (bits<- val) level)
            (bits<- "1f"))))

(defun align-to-length (val length)
  (let* ((l (length val))
         (to-add (- length l))
         (addon (bit-not (bit- (<< 1 to-add) 1))))
    (concatenate 'bit-vector addon val)))

(defun bitpos (val level)
  (align-to-length (<< 1 (mask val level))
                   32
                   ))

(defun empty-bitmap () (make-array 32 :element-type 'bit))

(defun index (bitmask bit)
  (reduce '+
          (bit-and bitmask
                   (align-to-length (bit- bit 1) 32))))

(defun indexed-node ()
  (list :indexed (empty-bitmap) (make-array 32 :fill-pointer 0)))

(defun indexed-node-val (bitmask arr)
  (list :indexed bitmask arr))

(defun map-> (m key)
  (map-hash-> m (hash key) 0))

(defun map<- (m key value)
  (map-hash<- m key (hash key) value 0))

(defun map-hash<- (node key h-key value level)
  (let ((bit (bitpos h-key level))
        (bitmask (cadr node)))
    (if (> (bits->int (bit-and bit bitmask)) 0)
        "overwrite existing value"
        (let ((idx (* 2 (index bitmask bit)))
              (new-bitmask (bit-ior bitmask bit))
              (new-vector (copy-seq (caddr node))))
          (setf (elt new-vector idx) key)
          (setf (elt new-vector (1+ idx)) value)
          (indexed-node-val new-bitmask new-vector)))))

|#
