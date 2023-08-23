
(define (test1 input expected-output)
  (define parser (make-term-parser 7))
  (assert= expected-output (parser input)))

(define (test-error input error-type)
  (define parser (make-term-parser 4))
  (assert-throw error-type (parser input)))

(test1 'video `((video "$")))
(test1 'video=X '((video X)))
(test1 'video=X,Y '((video X Y)))
(test1 'video=X,Y,Z,W '((video X Y Z W)))
(test1 'video=$ '((video "$")))
(test1 'video=$,Y '((video "$" Y)))
(test1 'video=X,$ '((video X "$")))
(test1 'video=X,$,Y '((video X "$" Y)))
(test1 'video=X,Y=Z,W '((video X Y) (video Z W)))
(test1 'video=X,Y=Z,W '((video X Y) (video Z W)))
(test1 'video=X=X '((video X) (video X)))
(test1 'video=X,Y=X,W '((video X Y) (video X W)))

(test1 'video:X '((video X)))
(test1 'video:X,Y '((video X Y)))
(test1 'video:X,Y,Z,W '((video X Y Z W)))
(test1 'video:$ '((video "$")))
(test1 'video:$,Y '((video "$" Y)))
(test1 'video:X,$ '((video X "$")))
(test1 'video:X,$,Y '((video X "$" Y)))
(test1 'video:X,Y:Z,W '((video X Y) (video Z W)))
(test1 'video:X,Y:Z,W '((video X Y) (video Z W)))
(test1 'video:X:X '((video X) (video X)))
(test1 'video:X,Y:X,W '((video X Y) (video X W)))

(test1 'video=X '((video X)))
(test1 'video=X+Y '((video X Y)))
(test1 'video=X+Y+Z+W '((video X Y Z W)))
(test1 'video=$ '((video "$")))
(test1 'video=$+Y '((video "$" Y)))
(test1 'video=X+$ '((video X "$")))
(test1 'video=X+$+Y '((video X "$" Y)))
(test1 'video=X+Y=Z+W '((video X Y) (video Z W)))
(test1 'video=X+Y=Z+W '((video X Y) (video Z W)))
(test1 'video=X=X '((video X) (video X)))
(test1 'video=X+Y=X+W '((video X Y) (video X W)))

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
(test-error 'video,= 'type-error)

(test1 "video" `((video "$")))
(test1 "video=X" '((video X)))
(test1 "video=X,Y" '((video X Y)))
(test1 "video=X,Y=Z,W" '((video X Y) (video Z W)))
(test1 "video=$" '((video "$")))

(test-error "vid eo" 'type-error)
(test-error "vid eo=X" 'type-error)
(test-error "vid=Y eo=X" 'type-error)
(test-error "vid\neo" 'type-error)

(test1 "\"vid eo\"" `((#{"vid eo"}# "$")))
(test1 "\"vid eo\"=X" `((#{"vid eo"}# X)))
(test1 "\"vid eo=X\"" `((#{"vid eo=X"}# "$")))
(test1 "\"vid=Y eo\"=X" `((#{"vid=Y eo"}# X)))
(test1 "\"vid\neo\"" `((#{"vid\xa;eo"}# "$")))

(test-error "\"vid eo=\"X" 'type-error)

(test1 "video" `((video "$")))
(test1 "video=\"X\"" '((video "X")))
(test1 "video=\"X\",Y" '((video "X" Y)))
(test1 "video=\"X,Y\"" '((video "X,Y")))
(test1 "video=X,\"Y\"" '((video X "Y")))
(test1 "video=X,\"Y\"=Z,W" '((video X "Y") (video Z W)))
(test1 "video=X,\"Y=Z\",W" '((video X "Y=Z" W)))
(test1 "video=$" '((video "$")))

(test-error "video=\"X\"Y" 'type-error)

(test1 '(video) `((video "$")))
(test1 '(video X) '((video X)))
(test1 '(video X Y) '((video X Y)))
(test1 '(video X Y Z W) '((video X Y Z W)))
(test1 '(video $) '((video "$")))
(test1 '(video "$") '((video "$")))
(test1 '(video $ Y) '((video "$" Y)))
(test1 '(video X $) '((video X "$")))
(test1 '(video X $ Y) '((video X "$" Y)))

(test1 '("video") `((video "$")))
(test1 '("video" X) '((video X)))
(test1 '("video" X Y) '((video X Y)))
(test1 '("video" X Y Z W) '((video X Y Z W)))
(test1 '("video" $) '((video "$")))
(test1 '("video" "$") '((video "$")))
(test1 '("video" $ Y) '((video "$" Y)))
(test1 '("video" X $) '((video X "$")))
(test1 '("video" X $ Y) '((video X "$" Y)))