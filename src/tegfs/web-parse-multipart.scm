;;;; Copyright (C) 2022  Otto Jung
;;;;
;;;; This program is free software: you can redistribute it and/or modify
;;;; it under the terms of the GNU Affero General Public License as published
;;;; by the Free Software Foundation, either version 3 of the License, or
;;;; (at your option) any later version.
;;;;
;;;; This program is distributed in the hope that it will be useful,
;;;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;;;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;;;; GNU Affero General Public License for more details.
;;;;
;;;; You should have received a copy of the GNU Affero General Public License
;;;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

(cond-expand
 (guile
  (define-module (tegfs web-parse-multipart)
    :export (parse-multipart/from-data parse-multipart-as-hashmap parse-multipart->hashmap-by-name)
    :use-module ((euphrates comp) :select (comp))
    :use-module ((euphrates fn-cons) :select (fn-cons))
    :use-module ((euphrates hashmap) :select (hashmap-set! make-hashmap))
    :use-module ((euphrates raisu) :select (raisu))
    :use-module ((euphrates string-split-3) :select (string-split-3))
    :use-module ((euphrates string-strip) :select (string-strip))
    :use-module ((euphrates string-to-lines) :select (string->lines))
    :use-module ((euphrates tilda-a) :select (~a))
    :use-module ((euphrates un-tilda-s) :select (un~s)))))



(cond-expand
 (guile

  (use-modules (web uri))
  (use-modules (rnrs bytevectors))
  (use-modules (ice-9 iconv))

  ))

(define (split-semicolons str0)
  (define str (string->list str0))

  (let loop ((str str) (buf '()) (cur "") (escaped? #f) (in-string? #f))
    (if (null? str)
        (if (string-null? cur)
            (reverse buf)
            (reverse (cons cur buf)))
        (let* ((x (car str)))
          (define (regular-char)
            (loop (cdr str)
                  buf
                  (string-append cur (list->string (list x)))
                  #f
                  in-string?))

          (cond
           ((equal? x #\;)
            (if in-string?
                (regular-char)
                (loop (cdr str)
                      (cons cur buf)
                      ""
                      #f
                      in-string?)))
           ((equal? x #\\)
            (if in-string?
                (loop (cdr str)
                      buf
                      (string-append cur (list->string (list x)))
                      (not escaped?)
                      in-string?)
                (regular-char)))
           ((equal? x #\")
            (if escaped?
                (regular-char)
                (loop (cdr str)
                      buf
                      (string-append cur (list->string (list x)))
                      #f
                      (not in-string?))))
           (else
            (regular-char)))))))

(define (parse-content-disposition key/str value)
  (define key (string->symbol key/str))

  (define parts
    (map string-strip (split-semicolons value)))

  (define head
    (car parts))

  (define args
    (cdr parts))

  (define key-values-0
    (map (lambda (part)
           (define-values (pre it post) (string-split-3 #\= part))
           (when (string-null? it)
             (raisu 'key-value-is-wrong part value))
           (cons pre post))
         args))

  (define key-values
    (cons (cons "head" head) key-values-0))

  (cons
   (cons key value)
   (map (fn-cons (comp (string-append key/str ":") string->symbol) (comp un~s ~a))
        key-values)))

(define (parse-multipart-type type-chunk)
  (define lines
    (filter (negate string-null?)
            (map string-strip (string->lines type-chunk))))

  (define split1
    (apply
     append
     (map
      (lambda (line)
        (define-values (pre0 it post0) (string-split-3 #\: line))
        (when (string-null? it)
          (throw 'type-chunk-line-must-contain-a-column line))

        (define pre1 (string-strip pre0))
        (define pre (string->symbol pre1))
        (define post (string-strip post0))

        (case pre
          ((Content-Disposition) (parse-content-disposition pre1 post))
          (else (list (cons pre post)))))
      lines)))

  split1)

;; TODO: contribute this procedure to guile/module/web ?
(define (parse-multipart/from-data data)
  (define len (bytevector-length data))
  (define header-size
    (let loop ((i 0))
      (if (>= i len) (raisu 'multipart-data-header-not-found len)
          (let ((cur (array-ref data i)))
            (if (equal? 13 cur) i
                (loop (+ 1 i)))))))

  (define (bytevector-crop start end/non-inclusive source)
    (let* ((size (- end/non-inclusive start))
           (vec (make-bytevector size 0)))
      (bytevector-copy! source start vec 0 size)
      vec))

  (define (bytevector-prefix? prefix-vector original-vector start)
    (define len (bytevector-length prefix-vector))
    (define olen (- (bytevector-length original-vector) start))
    (and (<= len olen)
         (let loop ((i 0))
           (or (>= i len)
               (and (equal? (array-ref prefix-vector i)
                            (array-ref original-vector (+ start i)))
                    (loop (+ 1 i)))))))

  (define (bytevector-append a b)
    (define len-a (bytevector-length a))
    (define len-b (bytevector-length b))
    (define len-ret (+ len-a len-b))
    (define ret (make-bytevector len-ret 0))
    (bytevector-copy! a 0 ret 0 len-a)
    (bytevector-copy! b 0 ret len-a len-b)
    ret)

  (define data-newline #vu8(13 10))
  (define block-data-separator #vu8(13 10 13 10))
  (define block-data-separator-length (bytevector-length block-data-separator))
  (define block-data-start #vu8(45 45))

  (define header
    (bytevector-append
     data-newline (bytevector-crop 0 header-size data)))

  (define header-length
    (bytevector-length header))

  (define header/string
    (bytevector->string header "latin1"))

  (define (split-block start end/non-inclusive)

    (define separator-index
      (let loop ((i start))
        (if (>= i end/non-inclusive) #f
            (if (bytevector-prefix? block-data-separator data i)
                i
                (loop (+ 1 i))))))

    (define crop-index
      (or separator-index end/non-inclusive))

    (define type-chunk
      (bytevector->string
       (bytevector-crop start crop-index data) "utf-8"))

    ;; TODO: should require data?
    (define data-chunk
      (if separator-index
          (bytevector-crop
           (+ separator-index block-data-separator-length)
           end/non-inclusive
           data)
          #vu8()))

    (cons type-chunk data-chunk))

  (define blocks
    (let loop ((i header-size)
               (block-start header-size)
               (buf '()))
      (if (>= i len)
          ;; TODO: should throw an exception? last block must end explicitly by mentioning separator that ends with --
          (reverse
           (cons (split-block block-start (min len i)) buf))
          (if (bytevector-prefix? header data i)
              (let ((new-start (+ i header-length)))
                (if (bytevector-prefix? block-data-start data new-start)
                    (reverse
                     (cons (split-block block-start i) buf))
                    (loop new-start new-start
                          (cons (split-block block-start i) buf))))
              (loop (+ 1 i) block-start buf)))))

  (define parsed-blocks
    (map (lambda (p)
           (cons
            (cons 'data (cdr p))
            (parse-multipart-type (car p))))
         blocks))

  parsed-blocks)

(define (parsed-multipart->hashmap-by-name parsed)
  (define H (make-hashmap))
  (for-each
   (lambda (block)
     (define name (assoc 'Content-Disposition:name block))
     (unless name
       (raisu 'parsed-block-does-not-have-a-name block))
     (hashmap-set! H (cdr name) block))
   parsed)
  H)

(define (parse-multipart-as-hashmap body/bytes)
  (define body/parsed (parse-multipart/from-data body/bytes))
  (define body/hash (parsed-multipart->hashmap-by-name body/parsed))
  body/hash)
