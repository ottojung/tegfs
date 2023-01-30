;;;; Copyright (C) 2023  Otto Jung
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

;; TODO: move to a separate "tegfs-downloader" project.
;; TODO: import euphrates functions directly.

(use-modules (ice-9 format))

(define (check-dependency program-name)
  (define command
    (with-output-to-string
      (lambda _
        (display "command -v ")
        (write program-name)
        (display " >/dev/null 2>/dev/null"))))
  (define errormsg
    (with-output-to-string
      (lambda _
        (display "Missing dependency ")
        (write program-name)
        (display ". Please install it first if you want to use the download-manager plugin"))))

  (define status (system command))
  (define code (status:exit-val status))
  (unless (= 0 code)
    (throw 'plugin-initialization-failed errormsg)))

(define (a-weblink? string)
  (or (string-prefix? "blob:http" string)
      (string-prefix? "http://" string)
      (string-prefix? "https://" string)))

(define (list-prefix? prefix-lst target-lst)
  (let loop ((prefix-lst prefix-lst)
             (target-lst target-lst))
    (cond
     ((null? prefix-lst) #t)
     ((null? target-lst) #f)
     (else
      (and (equal? (car prefix-lst) (car target-lst))
           (loop (cdr prefix-lst) (cdr target-lst)))))))

(define (list-drop-n n lst)
  (let loop ((n n) (lst lst))
    (if (or (>= 0 n) (null? lst))
        lst
        (loop (- n 1) (cdr lst)))))

(define-syntax list-find-first
  (syntax-rules ()
    ((_ f lst)
     (list-find-first
      f (throw 'no-first-element-to-satisfy-predicate f)
      lst))
    ((_ f0 default lst0)
     (let ((f f0) (lst lst0))
       (let loop ((lst lst))
         (if (null? lst) default
             (let ((x (car lst)))
               (if (f x) x
                   (loop (cdr lst))))))))))

(define (string-split-3 delimiter str)
  (define (string-split-first/lambda predicate str)
    (define lst (string->list str))
    (let loop ((buf lst) (ret '()))
      (if (null? buf) (values str "" "")
          (let ((cur (car buf)))
            (if (predicate cur)
                (values (list->string (reverse ret))
                        (list->string (list cur))
                        (list->string (cdr buf)))
                (loop (cdr buf) (cons cur ret)))))))

  (define (string-split-first/string delimiter str)
    (define lst (string->list str))
    (define len (length delimiter))
    (let loop ((buf lst) (ret '()))
      (if (null? buf) (values str "" "")
          (if (list-prefix? delimiter buf)
              (values (list->string (reverse ret))
                      (list->string delimiter)
                      (list->string (list-drop-n len buf)))
              (loop (cdr buf) (cons (car buf) ret))))))

  (cond
   ((string? delimiter) (string-split-first/string (string->list delimiter) str))
   ((char? delimiter) (string-split-first/string (list delimiter) str))
   ((procedure? delimiter) (string-split-first/lambda delimiter str))
   (else (throw 'delimiter-must-be-string-or-char-or-procedure delimiter))))

;; Parse a URL into 5 components:
;;    <scheme>://<netloc>/<path>?<query>#<fragment>
;;
;; Returns a vector of those 5 values, or #f if the argument is not an URL
;;
;; Inspired from:
;; https://github.com/python/cpython/blob/259dd71c32a42708a2800c72898e2664a33fda9c/Lib/urllib/parse.py#L365
;; this function is the `urlsplit` from the above source code
;; except that we don't care about the encoding
;;
;; The <scheme> is also called "protocol"
;; The <netloc> is hostname+port
(define (url-decompose str)
  (define-values (protocol0 protocol-sep rest0)
    (string-split-3 "://" str))
  (define protocol
    (if (string-null? protocol-sep) ""
        protocol0))
  (define rest1
    (if (string-null? protocol-sep)
        str
        rest0))

  (define-values (netloc netloc-sep rest2)
    (if (string-null? protocol-sep)
        (values "" "" rest1)
        (string-split-3 "/" rest1)))

  (define first-after-netloc
    (list-find-first
     (lambda (c) (case c ((#\? #\#) #t) (else #f))) #f
     (string->list rest2)))

  (define-values (path path-sep rest3)
    (if first-after-netloc
        (string-split-3 first-after-netloc rest2)
        (values rest2 "" "")))

  (define-values (query query-sep rest4)
    (if (equal? #\? first-after-netloc)
        (string-split-3 #\# rest3)
        (values #f "" rest3)))

  (define fragment
    (if (or (equal? "#" query-sep)
            (equal? "#" path-sep))
        rest4
        #f))

  (vector protocol netloc (string-append netloc-sep path) query fragment))

(use-modules (ice-9 match))

(define (url-get-path url)
  (let ((decomp (url-decompose url)))
    (and decomp (vector-ref decomp 2))))

(define (url-get-hostname-and-port url)
  (let ((decomp (url-decompose url)))
    (and decomp (vector-ref decomp 1))))

(define (url-get-fragment url)
  (let ((decomp (url-decompose url)))
    (and decomp (vector-ref decomp 4))))

(define [catch-any body handler]
  (catch #t body
         (lambda err (handler err))))

(define-syntax with-ignore-errors!*
  (syntax-rules ()
    ((_ . bodies)
     (catch-any
      (lambda _ . bodies)
      (lambda errors #f)))))

(define read-all-port
  (case-lambda
   ((port)
    (read-all-port port read-char))
   ((port readf)
    (read-all-port port readf list->string))
   ((port readf collect)
    (let loop ((result '()) (chr (readf port)))
      (if (eof-object? chr)
          (collect (reverse result))
          (loop (cons chr result) (readf port)))))))

(define (string-trim-chars str chars-arg direction)
  (define chars (if (string? chars-arg)
                    (string->list chars-arg)
                    chars-arg))
  (define (pred c)
    (memv c chars))
  (case direction
    ((left) (string-trim str pred))
    ((right) (string-trim-right str pred))
    ((both) (string-trim-both str pred))))

(define string-strip
  (case-lambda
   ((str) (string-trim-chars str "\r\n \t" 'both))
   ((str chars) (string-trim-chars str chars 'both))))

(define (read-string-file path)
  (let* ((in (open-file path "r"))
         (text (read-all-port in read-char))
         (go (close-port in)))
    text))

(define (debug fmt . args)
  (apply format (cons #t (cons fmt args)))
  (newline))

(define (stringf fmt . args)
  (with-output-to-string
    (lambda _
      (apply format (cons #t (cons fmt args))))))

;; Example `target' handled by this function:
;; https://boards.4chan.org/r/thread/18729837#p18729841
(define (download-4chan-media config root target current-alist)
  (define path (url-get-path target))
  (define split (string-split path #\/))
  (define board (list-ref split 1))
  (define thread-id (list-ref split 3))
  (define comment-id/0 (url-get-fragment target))
  (define comment-id (and comment-id/0 (substring comment-id/0 1)))
  (define thread-json-link
    (stringf "https://a.4cdn.org/~a/thread/~a.json" board thread-id))
  (define json-path
    (begin
      (with-ignore-errors!* (mkdir root))
      (with-ignore-errors!* (mkdir (string-append root "/tmp")))
      (string-append root "/tmp/" "4chan.json")))
  (define _2
    (let ((s (system* "wget" thread-json-link "-O" json-path)))
      (unless (= 0 (status:exit-val s))
        (throw 'download-failed))))
  (define jq-post-path
    (stringf "~a/tmp/4chan.post.jq" root))
  (define jq-select-thread-command
    (if comment-id
        (stringf ".posts[] | select(.no == ~a)" comment-id)
        ".posts[0]"))
  (define _1
    (let ((s (system (stringf "jq ~s ~s > ~s" jq-select-thread-command json-path jq-post-path))))
      (unless (= 0 (status:exit-val s))
        (throw 'jq-failed))))
  (define jq-tmp-path
    (stringf "~a/tmp/4chan.jq" root))
  (define (get-post-field name)
    (let ((s (system (stringf "jq .~a ~s > ~s" name jq-post-path jq-tmp-path))))
      (unless (= 0 (status:exit-val s))
        (throw 'jq-failed)))
    (string-strip (read-string-file jq-tmp-path)))
  (define (unescape escaped)
    (with-input-from-string escaped (lambda _ (read))))
  (define tim (get-post-field "tim"))
  (define filename (unescape (get-post-field "filename")))
  (define ext (unescape (get-post-field "ext")))
  (define note (unescape (get-post-field "com")))
  (define download-link
    (stringf "https://i.4cdn.org/~a/~a~a" board tim ext))

  (define -temporary-file
    (stringf "~a/tmp/4chan-file~a" root ext))

  (let ((s (system* "wget" download-link "-O" -temporary-file)))
    (unless (= 0 (status:exit-val s))
      (throw 'download-failed)))

  `((-temporary-file . ,-temporary-file)
    (target-basename . ,filename)
    (target-extension . ,ext)
    (note . ,note)
    (download? . yes)))

(define (download-youtube-media config root target current-alist)
  (throw 'not-implemented))

(define (handle config root current-alist target)
  (define site
    (and target (url-get-hostname-and-port target)))
  (catch-any
   (lambda _
     (cond
      ((equal? "boards.4chan.org" site)
       (download-4chan-media config root target current-alist))
      ((equal? "youtube.com" site)
       (download-youtube-media config root target current-alist))
      (else #f)))
   (lambda errors
     (display "Download manager failed to handle a recognized link.")
     #f)))

(define (main config root current-alist)
  (define target
    (cdr (or (assq '-text-content current-alist) (cons #f #f))))
  (define temp
    (cdr (or (assq '-temporary-file current-alist) (cons #f #f))))

  (or
   (and (not temp)
        (string? target)
        (a-weblink? target)
        (handle config root current-alist target))
   '()))

(check-dependency "jq")
(check-dependency "wget")

main
