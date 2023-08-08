
(let ((parser (make-tag-parser 7)))
  (assert= '((video "$")) (parser 'video)))

(let ((parser (make-tag-parser 7)))
  (assert= '((video "X")) (parser 'video=X)))

(let ((parser (make-tag-parser 7)))
  (assert= '((video "X" "Y")) (parser 'video=X,Y)))

(let ((parser (make-tag-parser 7)))
  (assert= '((video "X" "Y" "Z" "W")) (parser 'video=X,Y,Z,W)))

(let ((parser (make-tag-parser 7)))
  (assert= '((video "$")) (parser 'video=$)))

(let ((parser (make-tag-parser 7)))
  (assert= '((video "$" "Y")) (parser 'video=$,Y)))

(let ((parser (make-tag-parser 7)))
  (assert= '((video "X" "$")) (parser 'video=X,$)))

(let ((parser (make-tag-parser 7)))
  (assert= '((video "X" "$" "Y")) (parser 'video=X,$,Y)))

(let ((parser (make-tag-parser 7)))
  (assert= '((video "X" "Y") (video "Z" "W")) (parser 'video=X,Y=Z,W)))

(let ((parser (make-tag-parser 7)))
  (assert= '((video "X" "Y") (video "Z" "W")) (parser 'video=X,Y=Z,W)))

(let ((parser (make-tag-parser 7)))
  (assert= '((video "X") (video "X")) (parser 'video=X=X)))

(assert-throw
 'type-error
 (let ((parser (make-tag-parser 7)))
   (assert= '((video)) (parser 'video=))))

(assert-throw
 'type-error
 (let ((parser (make-tag-parser 7)))
   (assert= '((video)) (parser 'video=X=))))

(assert-throw
 'type-error
 (let ((parser (make-tag-parser 7)))
   (assert= '((video)) (parser 'video==))))

(assert-throw
 'type-error
 (let ((parser (make-tag-parser 7)))
   (assert= '((video)) (parser 'video==X))))

(assert-throw
 'type-error
 (let ((parser (make-tag-parser 7)))
   (assert= '((video)) (parser '=X))))
