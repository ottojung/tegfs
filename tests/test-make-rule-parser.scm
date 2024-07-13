
(define (test1 input expected-output)
  (define parser (make-rule-parser 7))
  (assert= expected-output (parser input)))

(define (test-error input error-type)
  (define parser (make-rule-parser 4))
  (assert-throw error-type (parser input)))

(test1 "video image => audio text"
       `(((audio "$") (video "$") (image "$"))
         ((text "$") (video "$") (image "$"))))

(test1 "video image => audio text"
       `(((audio "$") (video "$") (image "$"))
         ((text "$") (video "$") (image "$"))))

(test1 "video      image => audio text"
       `(((audio "$") (video "$") (image "$"))
         ((text "$") (video "$") (image "$"))))

(test1 "video      image => audio     text"
       `(((audio "$") (video "$") (image "$"))
         ((text "$") (video "$") (image "$"))))

(test1 "video      image              =>   audio         text"
       `(((audio "$") (video "$") (image "$"))
         ((text "$") (video "$") (image "$"))))

(test1 "video (image $) => audio text"
       `(((audio "$") (video "$") (image "$"))
         ((text "$") (video "$") (image "$"))))

(test1 "video (image X Y \"Z\" W) => audio text"
       `(((audio "$") (video "$") (image X Y "Z" W))
         ((text "$") (video "$") (image X Y "Z" W))))

(test-error "    video    =>      audio"
            'type-error)

(test-error "video    =>      audio          "
            'type-error)

(test-error "    video    =>      audio     "
            'type-error)

(test-error "    video   image    =>      audio   text     "
            'type-error)
