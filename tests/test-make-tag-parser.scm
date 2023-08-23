
(define (test1 input expected-output)
  (define parser (make-tag-parser 7))
  (assert= expected-output (parser input)))

(define (test-error input error-type)
  (define parser (make-tag-parser 4))
  (assert-throw
   'type-error
   (parser input)))

(test1 'video `((video "$")))

(test1 'video=X '((video "X")))

(test1 'video=X,Y '((video "X" "Y")))

(test1 'video=X,Y,Z,W '((video "X" "Y" "Z" "W")))

(test1 'video=$ '((video "$")))

(test1 'video=$,Y '((video "$" "Y")))

(test1 'video=X,$ '((video "X" "$")))

(test1 'video=X,$,Y '((video "X" "$" "Y")))

(test1 'video=X,Y=Z,W '((video "X" "Y") (video "Z" "W")))

(test1 'video=X,Y=Z,W '((video "X" "Y") (video "Z" "W")))

(test1 'video=X=X '((video "X") (video "X")))

(test-error 'video= 'type-error)
(test-error 'video=X= 'type-error)
(test-error 'video== 'type-error)
(test-error 'video==X 'type-error)
(test-error '=X 'type-error)
(test-error '=X,Y 'type-error)
(test-error '=X=Y 'type-error)
(test-error 'video=X, 'type-error)
(test-error 'video=, 'type-error)
(test-error 'video,, 'type-error)
(test-error 'video,= 'type-error)

(test1 "video" `((video "$")))
(test1 "video=X" '((video "X")))
(test1 "video=X,Y" '((video "X" "Y")))
