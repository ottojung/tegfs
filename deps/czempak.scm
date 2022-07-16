

(use-modules (ice-9 format))

(use-modules (ice-9 binary-ports))

(use-modules (ice-9 hash-table))

(use-modules (ice-9 threads))

(use-modules (ice-9 popen))

(use-modules (ice-9 ftw))

(use-modules (ice-9 match))

(use-modules (ice-9 atomic))

(use-modules (ice-9 rdelim))

(use-modules (srfi srfi-1))

(use-modules (srfi srfi-9))

(use-modules (srfi srfi-11))

(use-modules (srfi srfi-13))

(use-modules (srfi srfi-16))

(use-modules (srfi srfi-18))

(use-modules (srfi srfi-19))

(use-modules (srfi srfi-42))

(use-modules (srfi srfi-111))

(define null (list))


(define add1 #{1+}#)


(define sub1 #{1-}#)


(define dynamic-thread-mutex-make-p
  (make-parameter (@ (srfi srfi-18) make-mutex)))


(define dynamic-thread-mutex-lock!-p
  (make-parameter mutex-lock!))


(define dynamic-thread-mutex-unlock!-p
  (make-parameter mutex-unlock!))


(define (atomic-box-compare-and-set!
         box
         expected
         desired)
  (let ((ret (atomic-box-compare-and-swap!
               box
               expected
               desired)))
    (eq? ret expected)))


(define hash-has-key? hash-get-handle)


(define (hash-empty? h)
  (= 0 (hash-count (lambda _ 0) h)))


(define (hash-table->alist h)
  (hash-map->list cons h))


(define (hash-copy h)
  (let ((ret (make-hash-table)))
    (hash-for-each
      (lambda (key value) (hash-set! ret key value))
      h)
    ret))


(define (hash-table-foreach h procedure)
  (hash-for-each procedure h))


(define (catch-any body handler)
  (catch #t body (lambda err (handler err))))


(define printf
  (lambda args (apply guile-printf args)))


(define (~a x)
  (with-output-to-string (lambda _ (display x))))


(define (~s x)
  (with-output-to-string (lambda _ (write x))))


(define time-get-monotonic-nanoseconds-timestamp
  (let ((time-to-nanoseconds
          (lambda (time)
            (+ (time-nanosecond time)
               (* 1000000000 (time-second time))))))
    (lambda ()
      (time-to-nanoseconds
        ((@ (srfi srfi-19) current-time) time-monotonic)))))


(define #{string-split#simple}# string-split)


(define string-pad-left string-pad)


(define (words str)
  (filter
    (compose not string-null?)
    (string-split
      str
      (lambda (c)
        (case c ((#\newline #\space #\tab) #t) (else #f))))))


(define big-random-int
  (let ((initialized? #f))
    (lambda (max)
      (unless
        initialized?
        (set! initialized? #t)
        (set! *random-state*
          (random-state-from-platform)))
      (random max))))


(define-syntax-rule
  (#{with-output-to-file#clear}# file . bodies)
  (with-output-to-file file (lambda () . bodies)))


(define-syntax-rule
  (define-eval-namespace name)
  (define name 'not-used))


(define (eval-string-in-namespace str namespace)
  (eval-string str))


(define-syntax-rule
  (load-file-in-namespace filepath namespace)
  (load filepath))


(define get-command-line-arguments
  (make-parameter
    (let ((ret (command-line)))
      (if (< (length ret) 2) (list) (cdr ret)))))


(define (get-current-program-path)
  (let ((ret (command-line)))
    (if (null? ret)
      'unknown-current-program-path
      (car ret))))


(define-syntax-rule
  (get-current-source-file-path)
  (cdr (assq 'filename (current-source-location))))


(define-syntax-rule
  (get-current-source-info)
  (current-source-location))


(define find-first find)


(define-syntax format-id-base
  (lambda (stx1)
    (syntax-case stx1 ()
      ((format-id stx fmt args)
       (with-syntax ((ret (syntax
                            (datum->syntax
                              stx
                              (string->symbol
                                (with-output-to-string
                                  (lambda ()
                                    (apply format (cons* #t fmt args)))))))))
         (syntax ret))))))


(define-syntax-rule
  (format-id stx fmt . args)
  (format-id-base stx fmt (list . args)))


(define-syntax-rule
  (begin-for-syntax . args)
  (begin . args))


(define first car)


(define (second x) (list-ref x 1))


(define (third x) (list-ref x 2))


(define (fourth x) (list-ref x 3))


(define (fifth x) (list-ref x 4))


(define file-or-directory-exists? file-exists?)


(define (file-mtime filepath)
  (stat:mtime (stat filepath)))


(define remove-stat
  (match-lambda
    ((name stat) name)
    ((name stat children ...)
     (list name (map remove-stat children)))))


(define (directory-tree directory)
  "Returns object like this:\n   '((dir1 (dir1/file1 dir1/file2))\n     (dir2)\n     (dir3 (dir3/dir2 ..\n  "
  (remove-stat (file-system-tree directory)))


(define directory-files
  (case-lambda
    ((directory) (directory-files directory #f))
    ((directory include-directories?)
     (define (enter? name stat result)
       (string=? name directory))
     (define (leaf name stat result)
       (cons (list name (basename name)) result))
     (define (down name stat result) result)
     (define (up name stat result) result)
     (define (skip name stat result)
       (if include-directories?
         (cons (list name (basename name)) result)
         result))
     (define (error name stat errno result) result)
     (file-system-fold
       enter?
       leaf
       down
       up
       skip
       error
       '()
       directory))))


(define (directory-files-rec directory)
  "Returns object like this:\n   ((fullname name dirname1 dirname2 dirname3...\n    (fullname name ....\n\n   Where dirname1 is the parent dir of the file\n  "
  (define (enter? name stat result) #t)
  (define current '())
  (define (leaf name stat result)
    (cons (cons* name (basename name) current)
          result))
  (define (down name stat result)
    (set! current (cons name current))
    result)
  (define (up name stat result)
    (set! current (cdr current))
    result)
  (define (skip name stat result) result)
  (define (error name stat errno result) result)
  (file-system-fold
    enter?
    leaf
    down
    up
    skip
    error
    '()
    directory))


(define path-parent-directory dirname)


(define make-directory mkdir)


(define (make-temporary-fileport)
  (let ((port (mkstemp! (string-copy "/tmp/myfile-XXXXXX"))))
    (chmod port (logand 438 (lognot (umask))))
    (values port (port-filename port))))


(define-syntax rec-fields
  (lambda (stx)
    (syntax-case stx ()
      ((rec-fields fiii name buf export-buf)
       (with-syntax ((type (datum->syntax
                             (syntax name)
                             (symbol-append
                               'define-rec:
                               (syntax->datum (syntax name)))))
                     (predi (datum->syntax
                              (syntax name)
                              (symbol-append
                                (syntax->datum (syntax name))
                                '?))))
         (syntax
           (begin
             (define-record-type
               type
               (name . fiii)
               predi
               .
               buf)
             (export name predi . export-buf)))))
      ((rec-fields
         fiii
         name
         buf
         export-buf
         field
         .
         fields)
       (with-syntax ((gname (datum->syntax
                              (syntax field)
                              (symbol-append
                                (syntax->datum (syntax name))
                                '-
                                (syntax->datum (syntax field)))))
                     (sname (datum->syntax
                              (syntax field)
                              (symbol-append
                                'set-
                                (syntax->datum (syntax name))
                                '-
                                (syntax->datum (syntax field))
                                '!))))
         (syntax
           (rec-fields
             fiii
             name
             ((field gname sname) . buf)
             (gname sname . export-buf)
             .
             fields)))))))


(define-syntax-rule
  (define-rec name . fields)
  (rec-fields fields name () () . fields))


(define-rec
  sys-thread
  handle
  cancel-scheduled?
  cancel-enabled?)


(define sys-thread-current
  (make-parameter (sys-thread #f #f #f)))


(define (sys-thread-enable-cancel)
  (let ((me (sys-thread-current)))
    (set-sys-thread-cancel-enabled?! me #t)))


(define (sys-thread-disable-cancel)
  (let ((me (sys-thread-current)))
    (set-sys-thread-cancel-enabled?! me #f)))


(define (sys-thread-spawn thunk)
  (let ((th (sys-thread #f #f #t)))
    (set-sys-thread-handle!
      th
      (call-with-new-thread thunk))
    th))


(define (sys-thread-cancel th)
  (set-sys-thread-cancel-scheduled?! th #t))


(define (sys-thread-exited? th)
  (thread-exited? (sys-thread-handle th)))


(define (sys-thread-yield)
  (let ((me (sys-thread-current)))
    (when (and (sys-thread-cancel-scheduled? me)
               (sys-thread-cancel-enabled? me))
          (throw dynamic-thread-cancel-tag))))


(define (sys-thread-sleep us)
  (usleep us)
  (sys-thread-yield))


(define-rec
  comprocess
  command
  args
  pipe
  pid
  status
  exited?)


(define (#{run-comprocess#p-default}# command . args)
  "Run process in background\n   Input port is represented by `comprocess-pipe'\n   NOTE: in guile p-stdout == p-stderr doesn't work!\n\n   type ::= output-port? -> output-port? -> string -> list of string -> process\n  "
  (define p-stdout0
    (or (comprocess-stdout) (current-output-port)))
  (define-values
    (p-stdout p-stdout-file)
    (if (file-port? p-stdout0)
      (values p-stdout0 #f)
      (make-temporary-fileport)))
  (define p-stderr0
    (or (comprocess-stdout) (current-error-port)))
  (define-values
    (p-stderr p-stderr-file)
    (if (file-port? p-stderr0)
      (values p-stderr0 #f)
      (make-temporary-fileport)))
  (define-syntax-rule
    (with-ignore-errors! . bodies)
    (catch-any (lambda _ . bodies) (lambda errors 0)))
  (define cleanup
    (lambda _
      (when p-stdout-file
            (with-ignore-errors! (close-port p-stdout))
            (with-ignore-errors!
              (display
                (read-string-file p-stdout-file)
                p-stdout0))
            (with-ignore-errors! (delete-file p-stdout-file)))
      (when p-stderr-file
            (with-ignore-errors! (close-port p-stderr))
            (with-ignore-errors!
              (display
                (read-string-file p-stderr-file)
                p-stderr0))
            (with-ignore-errors! (delete-file p-stderr-file)))))
  (define (#{waitpid#no-throw#no-hang}# pid)
    (catch-any
      (lambda ()
        (let* ((w (waitpid pid WNOHANG))
               (ret-pid (car w))
               (status (cdr w)))
          (case ret-pid
            ((0) 'running)
            (else (status:exit-val status)))))
      (lambda errors 'not-available)))
  (let ((p (comprocess command args #f #f #f #f)))
    (parameterize
      ((current-output-port p-stdout)
       (current-error-port p-stderr))
      (let* ((pipe (apply open-pipe*
                          (cons* OPEN_WRITE
                                 (comprocess-command p)
                                 (comprocess-args p))))
             (pid (hashq-ref port/pid-table pipe))
             (re-status #f))
        (set-comprocess-pipe! p pipe)
        (set-comprocess-pid! p pid)
        (dynamic-thread-spawn
          (lambda _
            (let ((sleep (dynamic-thread-get-delay-procedure)))
              (call-with-finally
                (lambda _
                  (let lp ()
                    (let ((status (#{waitpid#no-throw#no-hang}# pid)))
                      (case status
                        ((running) (sleep) (lp))
                        (else (set! re-status status))))))
                (lambda _
                  (cleanup)
                  (set-comprocess-status! p re-status)
                  (set-comprocess-exited?! p #t)
                  (with-ignore-errors! (close-pipe pipe)))))))))
    p))


(define (#{kill-comprocess#p-default}# p force?)
  (kill (comprocess-pid p)
        (if force? SIGKILL SIGTERM)))


(define system*/exit-code system*)


(define euphrates-version
  "5d5de16f261648f8de4a17e21446da27f13a88dc")


(define-syntax define-smacro
  (lambda (stx)
    (syntax-case stx ()
      ((_ (f . args) body ...)
       (syntax (define-macro f (lambda args body ...))))
      ((_ f gen)
       (syntax
         (define-syntax f
           (lambda (y)
             (syntax-case y ()
               ((_ . args)
                (let ((v (syntax->datum (syntax args))))
                  (datum->syntax y (apply gen v))))))))))))


(define-syntax letin
  (syntax-rules ()
    ((letin ((a . as) b))
     (let-values (((a . as) b)) (values a . as)))
    ((letin ((a . as) b) . bodies)
     (let-values (((a . as) b)) (letin . bodies)))
    ((letin (a b)) (let ((a b)) a))
    ((letin (a b) . bodies)
     (let ((a b)) (letin . bodies)))))


(define-syntax defloop
  (lambda (stx)
    (syntax-case stx ()
      ((defloop lambda-list . body)
       (with-syntax ((name (datum->syntax (syntax body) 'loop)))
         (syntax
           (letrec ((name (lambda lambda-list . body)))
             name)))))))


(define-syntax-rule
  (apploop args argv . body)
  ((defloop args . body) . argv))


(define-syntax reversed-args-buf
  (syntax-rules ()
    ((_ (x . xs) buf)
     (reversed-args-buf xs (x . buf)))
    ((_ () buf) buf)))


(define-syntax-rule
  (reversed-args . args)
  (reversed-args-buf args ()))


(define-syntax reversed-args-f-buf
  (syntax-rules ()
    ((_ f (x . xs) buf)
     (reversed-args-f-buf f xs (x . buf)))
    ((_ f () buf) (f . buf))))


(define-syntax-rule
  (reversed-args-f f . args)
  (reversed-args-f-buf f args ()))


(define-syntax reversed-lambda
  (syntax-rules ()
    ((reversed-lambda body args) (lambda () body))
    ((reversed-lambda body args next)
     (lambda (next . args) body))
    ((reversed-lambda body args x next ...)
     (reversed-lambda body (x . args) next ...))))


(define-syntax fn-start
  (syntax-rules ()
    ((fn-start args body)
     (reversed-lambda body () . args))
    ((fn-start args x body ...)
     (fn-start (x . args) body ...))))


(define-syntax-rule
  (fn . argv)
  (fn-start () . argv))


(define-syntax fn-list-g
  (syntax-rules ()
    ((fn-list-g lst body) body)
    ((fn-list-g lst x body ...)
     (let ((x (car lst)))
       (fn-list-g (cdr lst) body ...)))))


(define-syntax-rule
  (fn-list . args)
  (lambda (lst) (fn-list-g lst . args)))


(define-syntax with-return
  (lambda (stx)
    (syntax-case stx ()
      ((define-job . bodies)
       (with-syntax ((return (datum->syntax (syntax bodies) 'return)))
         (syntax
           (call/cc (lambda (return) (begin . bodies)))))))))


(define-syntax monoids*
  (syntax-rules ()
    ((monoids* x) x)
    ((monoids* op x) (op x))
    ((monoids* a op b next-op ...)
     (monoids* (op a b) next-op ...))))


(define-syntax monoids-r*
  (syntax-rules ()
    ((monoids-r* x) x)
    ((monoids-r* op x) (op x))
    ((monoids-r* a op b next-op ...)
     (op a (monoids-r* b next-op ...)))))


(define-syntax monoid-r*
  (syntax-rules ()
    ((monoid-r* op a) a)
    ((monoid-r* op a b ...)
     (op a (monoid-r* op b ...)))))


(define-syntax monoid*
  (syntax-rules ()
    ((monoid* op a) a)
    ((monoid* op a b c ...)
     (monoid* op (op a b) c ...))))


(begin-for-syntax
  (define-syntax-rule
    (generate-prefixed-name prefix name)
    (format-id
      name
      "~a~a"
      prefix
      (syntax->datum name))))


(define-syntax-rule
  (#{catch-any#as-pair}# . bodies)
  (let* ((maybe-error #f)
         (result
           (catch-any
             (lambda () . bodies)
             (lambda (error) (set! maybe-error error)))))
    (values result maybe-error)))


(define-syntax-rule
  (intermezzo bind-name action . bodies)
  (let ((bind-name (begin . bodies)))
    action
    bind-name))


(define-syntax partial-apply1-helper
  (syntax-rules ()
    ((_ f buf () last)
     (reversed-args-f f last . buf))
    ((_ f buf (a . args) last)
     (partial-apply1-helper f (a . buf) args last))))


(define-syntax-rule
  (partial-apply1 f . args)
  (lambda (x) (partial-apply1-helper f () args x)))


(define-syntax partial-apply-helper
  (syntax-rules ()
    ((_ f buf () last)
     (apply f (reversed-args-f cons* last . buf)))
    ((_ f buf (a . args) last)
     (partial-apply-helper f (a . buf) args last))))


(define-syntax-rule
  (partial-apply f . args)
  (lambda xs (partial-apply-helper f () args xs)))


(define-syntax compose-under-helper
  (syntax-rules ()
    ((_ args op buf ())
     (lambda args (reversed-args-f op . buf)))
    ((_ args op buf (f . fs))
     (compose-under-helper
       args
       op
       ((apply f args) . buf)
       fs))))


(define-syntax-rule
  (compose-under operation . composites)
  (compose-under-helper
    args
    operation
    ()
    composites))


(define-syntax %comp-helper
  (syntax-rules ()
    ((_ buf ()) (compose . buf))
    ((_ buf ((x . xs) . y))
     (%comp-helper ((partial-apply1 x . xs) . buf) y))
    ((_ buf (x . y)) (%comp-helper (x . buf) y))))


(define-syntax-rule
  (comp . xs)
  (%comp-helper () xs))


(define-syntax %lcomp-helper
  (syntax-rules ()
    ((_ ()) identity)
    ((_ (((x . xs)) . y))
     (lambda (input)
       ((%lcomp-helper y)
        ((partial-apply1 x . xs) input))))
    ((_ (((x . xs) . names) . y))
     (lambda (input)
       (let-values
         ((names ((partial-apply1 x . xs) input)))
         ((%lcomp-helper y) input))))
    ((_ ((x . names) . y))
     (lambda (input)
       (let-values
         ((names (x input)))
         ((%lcomp-helper y) input))))
    ((_ ((x) . y))
     (lambda (input) ((%lcomp-helper y) (x input))))))


(define-syntax-rule
  (lcomp . xs)
  (%lcomp-helper xs))


(define-syntax-rule
  (appcomp x . xs)
  ((comp . xs) x))


(define-syntax-rule
  (applcomp x . xs)
  ((lcomp . xs) x))


(define (string-null-or-whitespace? str)
  (let loop ((i (sub1 (string-length str))))
    (if (< i 0)
      #t
      (case (string-ref str i)
        ((#\space #\tab #\newline) (loop (sub1 i)))
        (else #f)))))


(define (list->hash-set lst)
  (let ((H (make-hash-table (length lst))))
    (let loop ((lst lst))
      (unless
        (null? lst)
        (hash-set! H (car lst) #t)
        (loop (cdr lst))))
    H))


(define read-list
  (case-lambda
    (() (read-list (current-input-port)))
    ((input)
     (let ((p (if (string? input)
                (open-input-string input)
                input)))
       (let lp ()
         (let ((r (read p)))
           (if (eof-object? r)
             (begin
               (when (string? input) (close-port p))
               (list))
             (cons r (lp)))))))))


(define list-ref-or
  (case-lambda
    ((lst ref) (list-ref-or lst ref #f))
    ((lst ref default)
     (let lp ((lst lst) (ref ref))
       (if (null? lst)
         default
         (if (= 0 ref)
           (car lst)
           (lp (cdr lst) (#{1-}# ref))))))))


(define (list-partition predicate lst)
  (let lp ((buf lst) (false (list)) (true (list)))
    (if (null? buf)
      (values (reverse false) (reverse true))
      (if (predicate (car buf))
        (lp (cdr buf) false (cons (car buf) true))
        (lp (cdr buf) (cons (car buf) false) true)))))


(define (list-split-on predicate lst)
  (let loop ((lst lst) (buf (list)) (ret (list)))
    (cond ((null? lst)
           (if (null? buf)
             (reverse ret)
             (reverse (cons (reverse buf) ret))))
          ((predicate (car lst))
           (loop (cdr lst)
                 (list)
                 (if (null? buf) ret (cons (reverse buf) ret))))
          (else (loop (cdr lst) (cons (car lst) buf) ret)))))


(define curry-if
  (case-lambda
    ((test-function then-function)
     (curry-if test-function then-function identity))
    ((test-function then-function else-function)
     (lambda (x)
       (if (test-function x)
         (then-function x)
         (else-function x))))))


(define list-deduplicate
  (case-lambda
    ((lst) (list-deduplicate lst equal?))
    ((lst pred)
     (let ((H (make-hash-table (length lst))))
       (let lp ((buf lst) (mem (list)))
         (cond ((null? buf) mem)
               ((hash-ref H (car buf) #f) (lp (cdr buf) mem))
               (else
                (hash-set! H (car buf) #t)
                (lp (cdr buf) (cons (car buf) mem)))))))))


(define (cartesian-map function a b)
  (let lp1 ((ai a))
    (if (null? ai)
      (list)
      (let ((av (car ai)))
        (let lp2 ((bi b))
          (if (null? bi)
            (lp1 (cdr ai))
            (cons (function av (car bi)) (lp2 (cdr bi)))))))))


(define (cartesian-each function a b)
  (let lp ((ai a))
    (unless
      (null? ai)
      (let ((av (car ai)))
        (let lp ((bi b))
          (unless
            (null? bi)
            (function av (car bi))
            (lp (cdr bi)))))
      (lp (cdr ai)))))


(define (take-common-prefix a b)
  (list->string
    (let loop ((as (string->list a)) (bs (string->list b)))
      (if (or (null? as) (null? bs))
        (list)
        (if (char=? (car as) (car bs))
          (cons (car as) (loop (cdr as) (cdr bs)))
          (list))))))


(define (remove-common-prefix a b)
  (list->string
    (let loop ((as (string->list a)) (bs (string->list b)))
      (cond ((null? as) as)
            ((null? bs) as)
            ((eq? (car as) (car bs))
             (loop (cdr as) (cdr bs)))
            (else as)))))


(define (string-trim-chars str chars-arg direction)
  (define chars
    (if (string? chars-arg)
      (string->list chars-arg)
      chars-arg))
  (define (pred c) (memq c chars))
  (case direction
    ((left) (string-trim str pred))
    ((right) (string-trim-right str pred))
    ((both) (string-trim-both str pred))))


(define (lines str)
  (#{string-split#simple}# str #\newline))


(define (unlines lns) (string-join lns "\n"))


(define (unwords lns) (string-join lns " "))


(define (list-intersperse element lst)
  (let lp ((buf lst))
    (if (pair? buf)
      (let ((rest (cdr buf)))
        (if (null? rest)
          buf
          (cons* (car buf) element (lp rest))))
      null)))


(define list-traverse
  (case-lambda
    ((lst chooser) (list-traverse lst #f chooser))
    ((lst default chooser)
     (let lp ((rest lst))
       (if (null? rest)
         default
         (let* ((head (car rest)) (tail (cdr rest)))
           (let-values
             (((continue? return) (chooser head tail)))
             (if continue? (lp return) return))))))))


(define (list->tree lst divider)
  (define (recur tag rest)
    (define droped (list))
    (define taken
      (let lp ((lst rest))
        (if (null? lst)
          (list)
          (let* ((x (car lst)) (xs (cdr lst)))
            (let-values
              (((action d) (divider x xs)))
              (case action
                ((open)
                 (let-values
                   (((sub right) (recur x xs)))
                   (cons (append d sub) (lp right))))
                ((close) (set! droped xs) d)
                ((turn) (lp d))
                (else (cons x (lp xs)))))))))
    (values taken droped))
  (let-values (((pre post) (recur 'root lst))) pre))


(define-syntax assert
  (syntax-rules ()
    ((assert test)
     (unless
       test
       (throw 'assertion-fail `(test: ,'test))))
    ((assert test . printf-args)
     (unless
       test
       (throw 'assertion-fail
              `(test: ,'test)
              `(description: ,(stringf . printf-args)))))))


(define-syntax assert-norm-buf
  (syntax-rules ()
    ((_ orig buf (last-r))
     (let ((last last-r))
       (unless
         (reversed-args last . buf)
         (throw 'assertion-fail
                `(test: ,'orig)
                `(test!: ,(reversed-args-f list last . buf))))))
    ((_ orig buf (last-r) . printf-args)
     (let ((last last-r))
       (unless
         (reversed-args last . buf)
         (throw 'assertion-fail
                `(test: ,'orig)
                `(test!: ,(reversed-args-f list last . buf))
                `(description: ,(stringf . printf-args))))))
    ((_ orig buf (x-r . xs-r) . printf-args)
     (let ((x x-r))
       (assert-norm-buf
         orig
         (x . buf)
         xs-r
         .
         printf-args)))))


(define-syntax assert-norm
  (syntax-rules ()
    ((_ (x . xs) . printf-args)
     (assert-norm-buf
       (x . xs)
       ()
       (x . xs)
       .
       printf-args))
    ((_ test . printf-args)
     (assert test . printf-args))))


(define-syntax-rule
  (assert-equal a b . printf-args)
  (assert-norm (equal? a b) . printf-args))


(define range
  (case-lambda
    ((start count)
     (if (> count 0)
       (cons start
             (range (#{1+}# start) (#{1-}# count)))
       (list)))
    ((count) (range 0 count))))


(define (list-init lst)
  (take lst (#{1-}# (length lst))))


(define (normal->micro@unit s) (* 1000000 s))


(define (micro->nano@unit ms) (* 1000 ms))


(define (normal->nano@unit s)
  (micro->nano@unit (normal->micro@unit s)))


(define (nano->micro@unit ns) (quotient ns 1000))


(define (micro->normal@unit u)
  (quotient u 1000000))


(define (nano->normal@unit n)
  (quotient n (* 1000 1000000)))


(define (make-unique)
  "Returns procedure that returns #t if applied to itself, #f otherwise"
  (let ((euphrates-unique #f))
    (set! euphrates-unique
      (lambda (other) (eq? other euphrates-unique)))
    euphrates-unique))


(define (generic-fold
         first-f
         rest-f
         stop-predicate
         initial
         collection
         function)
  (let lp ((acc initial) (rest collection))
    (if (stop-predicate rest)
      acc
      (lp (function acc (first-f rest) rest)
          (rest-f rest)))))


(define-smacro
  (generic-fold-macro
    first-f
    rest-f
    stop-predicate
    initial
    collection
    .
    body)
  `(generic-fold
     ,first-f
     ,rest-f
     ,stop-predicate
     ,initial
     ,collection
     (lambda (acc x rest) ,@body)))


(define (list-fold initial lst function)
  (generic-fold
    car
    cdr
    null?
    initial
    lst
    (lambda (acc x rest) (function acc x))))


(define (list-fold/rest initial lst function)
  (generic-fold car cdr null? initial lst function))


(define-smacro
  (lfold initial lst . body)
  `(generic-fold-macro
     car
     cdr
     null?
     ,initial
     ,lst
     ,@body))


(define (simplify-posix-path path)
  (let* ((splits (#{string-split#simple}# path #\/))
         (norm (let lp ((buf (list)) (rest splits))
                 (if (null? rest)
                   (reverse buf)
                   (let ((cur (car rest)))
                     (if (string=? cur ".")
                       (lp buf (cdr rest))
                       (if (string=? cur "..")
                         (lp (cond ((null? buf) (cons cur buf))
                                   ((string=? "" (car buf)) buf)
                                   ((string=? ".." (car buf)) (cons cur buf))
                                   (else (cdr buf)))
                             (cdr rest))
                         (lp (cons cur buf) (cdr rest))))))))
         (ret (string-join norm "/")))
    ret))


(define (absolute-posix-path? path)
  (and (string? path)
       (char=? (string-ref path 0) #\/)))


(define (append-posix-path2 a b)
  (if (= (string-length a) 0)
    b
    (let ((b (if (absolute-posix-path? b)
               (let ((cl (remove-common-prefix b a)))
                 (if (equal? cl b)
                   (throw 'append-posix-path-disjoint
                          `(args: ,a ,b))
                   cl))
               b)))
      (if (char=?
            #\/
            (string-ref a (#{1-}# (string-length a))))
        (string-append a b)
        (string-append a "/" b)))))


(define (append-posix-path . paths)
  (list-fold "" paths append-posix-path2))


(define (path-without-extension str)
  (let ((index (string-index-right str #\.)))
    (string-take str index)))


(define (path-replace-extension str new-ext)
  (let ((stripped (path-without-extension str)))
    (string-append stripped new-ext)))


(define #{base64#alphabet}#
  #(#\A
    #\B
    #\C
    #\D
    #\E
    #\F
    #\G
    #\H
    #\I
    #\J
    #\K
    #\L
    #\M
    #\N
    #\O
    #\P
    #\Q
    #\R
    #\S
    #\T
    #\U
    #\V
    #\W
    #\X
    #\Y
    #\Z
    #\a
    #\b
    #\c
    #\d
    #\e
    #\f
    #\g
    #\h
    #\i
    #\j
    #\k
    #\l
    #\m
    #\n
    #\o
    #\p
    #\q
    #\r
    #\s
    #\t
    #\u
    #\v
    #\w
    #\x
    #\y
    #\z
    #\0
    #\1
    #\2
    #\3
    #\4
    #\5
    #\6
    #\7
    #\8
    #\9
    #\-
    #\_))


(define #{alphanum#alphabet}#
  #(#\0
    #\1
    #\2
    #\3
    #\4
    #\5
    #\6
    #\7
    #\8
    #\9
    #\a
    #\b
    #\c
    #\d
    #\e
    #\f
    #\g
    #\h
    #\i
    #\j
    #\k
    #\l
    #\m
    #\n
    #\o
    #\p
    #\q
    #\r
    #\s
    #\t
    #\u
    #\v
    #\w
    #\x
    #\y
    #\z
    #\A
    #\B
    #\C
    #\D
    #\E
    #\F
    #\G
    #\H
    #\I
    #\J
    #\K
    #\L
    #\M
    #\N
    #\O
    #\P
    #\Q
    #\R
    #\S
    #\T
    #\U
    #\V
    #\W
    #\X
    #\Y
    #\Z))


(define #{alpha#alphabet}#
  #(#\a
    #\b
    #\c
    #\d
    #\e
    #\f
    #\g
    #\h
    #\i
    #\j
    #\k
    #\l
    #\m
    #\n
    #\o
    #\p
    #\q
    #\r
    #\s
    #\t
    #\u
    #\v
    #\w
    #\x
    #\y
    #\z
    #\A
    #\B
    #\C
    #\D
    #\E
    #\F
    #\G
    #\H
    #\I
    #\J
    #\K
    #\L
    #\M
    #\N
    #\O
    #\P
    #\Q
    #\R
    #\S
    #\T
    #\U
    #\V
    #\W
    #\X
    #\Y
    #\Z))


(define #{printable#alphabet}#
  #(#\0
    #\1
    #\2
    #\3
    #\4
    #\5
    #\6
    #\7
    #\8
    #\9
    #\a
    #\b
    #\c
    #\d
    #\e
    #\f
    #\g
    #\h
    #\i
    #\j
    #\k
    #\l
    #\m
    #\n
    #\o
    #\p
    #\q
    #\r
    #\s
    #\t
    #\u
    #\v
    #\w
    #\x
    #\y
    #\z
    #\A
    #\B
    #\C
    #\D
    #\E
    #\F
    #\G
    #\H
    #\I
    #\J
    #\K
    #\L
    #\M
    #\N
    #\O
    #\P
    #\Q
    #\R
    #\S
    #\T
    #\U
    #\V
    #\W
    #\X
    #\Y
    #\Z
    #\+
    #\/
    #\@
    #\!
    #\&
    #\*
    #\=
    #\?
    #\(
    #\)
    #\-
    #\%
    #\#
    #\,
    #\.
    #\^
    #\'
    #\[
    #\]
    #\{
    #\}
    #\;
    #\:
    #\\
    #\<
    #\>
    #\"
    #\$))


(define (random-choice len #{alphabet#vector}#)
  (let ((size (vector-length #{alphabet#vector}#)))
    (let loop ((len len) (buf (list)))
      (if (<= len 0)
        buf
        (loop (sub1 len)
              (cons (vector-ref
                      #{alphabet#vector}#
                      (big-random-int size))
                    buf))))))


(define (make-temporary-filename)
  (let* ((s (list->string
              (random-choice 10 #{alphanum#alphabet}#))))
    (string-append "/tmp/euphrates-temp-" s)))


(define (current-source-info->string info)
  (let* ((linei (assq 'line info))
         (columni (assq 'column info))
         (filenamei (assq 'filename info))
         (cwd (string-append (getcwd) "/"))
         (line (or (and linei (string-append (~a (cdr linei)) ":"))
                   ""))
         (column
           (or (and columni
                    (string-append (~a (cdr columni)) ":"))
               ""))
         (filename
           (or (and filenamei
                    (string-append
                      (remove-common-prefix (cdr filenamei) cwd)
                      ":"))
               ""))
         (alli (string-append filename line column)))
    (or (and alli (string-append alli " ")) "")))


(define (stringf fmt . args)
  (with-output-to-string
    (lambda () (apply printf (cons* fmt args)))))


(define #{dprint#p-default}# printf)


(define #{dprint#p}#
  (make-parameter #{dprint#p-default}#))


(define dprint
  (lambda args (apply (#{dprint#p}#) args)))


(define (dprintln fmt . args)
  (apply dprint
         (cons* (string-append fmt "\n") args)))


(define global-debug-mode-filter
  (make-parameter #f))


(define (debug fmt . args)
  (let ((p (global-debug-mode-filter)))
    (when (or (not p) (p fmt args))
          (parameterize
            ((current-output-port (current-error-port)))
            (apply printf
                   (cons* (string-append fmt "\n") args))))))


(define-syntax-rule
  (with-ignore-errors! . bodies)
  (catch-any
    (lambda _ . bodies)
    (lambda errors
      (debug "~aerror: ~s"
             (current-source-info->string
               (get-current-source-info))
             errors))))


(define (dom-print name result x cont)
  (dprint "(~a = ~a = ~a)\n" name x result)
  (cont x))


(define (port-redirect from to)
  "Redirect from `from' to `to' byte by byte, until `eof-object?'\n   Returns count of written bytes\n\n   type ::= port -> port -> int\n  "
  (let lp ((count 0))
    (let ((byte (get-u8 from)))
      (if (eof-object? byte)
        count
        (begin (put-u8 to byte) (lp (#{1+}# count)))))))


(define-syntax-rule
  (cons! x lst)
  (set! lst (cons x lst)))


(define-syntax-rule
  (memconst x)
  (let ((memory #f) (evaled? #f))
    (lambda argv
      (unless
        evaled?
        (set! evaled? #t)
        (set! memory x))
      memory)))


(define-syntax-rule
  (memvalue x)
  (let ((memory #f) (evaled? #f))
    (case-lambda
      (()
       (unless
         evaled?
         (set! evaled? #t)
         (set! memory x))
       memory)
      ((type . args)
       (case type
         ((check) evaled?)
         ((or) (or memory (car args)))
         (else
          (throw 'unknown-memoize-command
                 type
                 memory
                 evaled?)))))))


(define-syntax-rule
  (effectvalue value . effects)
  (let ((evaled? #f))
    (case-lambda
      (()
       (unless evaled? (set! evaled? #t) . effects)
       value)
      ((type . args)
       (case type
         ((check) evaled?)
         ((or) (or (and evaled? value) (car args)))
         ((value) value)
         (else
          (throw 'unknown-effectvalue-command type evaled?)))))))


(define (replicate n x)
  (if (= 0 n)
    (list)
    (cons x (replicate (#{1-}# n) x))))


(define (#{dynamic-thread-get-delay-procedure#default}#)
  (let ((sleep (dynamic-thread-sleep-p))
        (timeout (#{dynamic-thread-wait-delay#us-p}#)))
    (lambda () (sleep timeout))))


(define dynamic-thread-spawn-p
  (make-parameter sys-thread-spawn))


(define (dynamic-thread-spawn thunk)
  ((dynamic-thread-spawn-p) thunk))


(define dynamic-thread-cancel-p
  (make-parameter sys-thread-cancel))


(define (dynamic-thread-cancel thunk)
  ((dynamic-thread-cancel-p) thunk))


(define dynamic-thread-disable-cancel-p
  (make-parameter sys-thread-disable-cancel))


(define (dynamic-thread-disable-cancel)
  ((dynamic-thread-disable-cancel-p)))


(define dynamic-thread-enable-cancel-p
  (make-parameter sys-thread-enable-cancel))


(define (dynamic-thread-enable-cancel)
  ((dynamic-thread-enable-cancel-p)))


(define dynamic-thread-yield-p
  (make-parameter (lambda () 0)))


(define (dynamic-thread-yield)
  ((dynamic-thread-yield-p)))


(define #{dynamic-thread-wait-delay#us-p}#
  (make-parameter 1000))


(define dynamic-thread-sleep-p
  (make-parameter sys-thread-sleep))


(define (dynamic-thread-sleep micro-seconds)
  ((dynamic-thread-sleep-p) micro-seconds))


(define dynamic-thread-get-delay-procedure-p
  (make-parameter
    #{dynamic-thread-get-delay-procedure#default}#))


(define (dynamic-thread-get-delay-procedure)
  ((dynamic-thread-get-delay-procedure-p)))


(define (dynamic-thread-get-yield-procedure)
  (dynamic-thread-yield-p))


(define dynamic-thread-cancel-tag
  'euphrates-dynamic-thread-cancelled)


(define (dynamic-thread-mutex-make)
  ((dynamic-thread-mutex-make-p)))


(define (dynamic-thread-mutex-lock! mut)
  ((dynamic-thread-mutex-lock!-p) mut))


(define (dynamic-thread-mutex-unlock! mut)
  ((dynamic-thread-mutex-unlock!-p) mut))


(define (#{dynamic-thread-critical-make#default}#)
  (let* ((mut (dynamic-thread-mutex-make))
         (lock-func (dynamic-thread-mutex-lock!-p))
         (unlock-func (dynamic-thread-mutex-unlock!-p))
         (lock (lambda () (lock-func mut)))
         (unlock (lambda () (unlock-func mut))))
    (lambda (thunk)
      (dynamic-thread-disable-cancel)
      (lock)
      (let ((ret (thunk)))
        (unlock)
        (dynamic-thread-enable-cancel)
        ret))))


(define dynamic-thread-critical-make-p
  (make-parameter
    #{dynamic-thread-critical-make#default}#))


(define (dynamic-thread-critical-make)
  ((dynamic-thread-critical-make-p)))


(define-values
  (make-uni-spinlock
    uni-spinlock-lock!
    uni-spinlock-unlock!
    make-uni-spinlock-critical)
  (let* ((make (lambda () (make-atomic-box #f)))
         (lock (lambda (o)
                 (let ((yield (dynamic-thread-get-yield-procedure)))
                   (let lp ()
                     (unless
                       (atomic-box-compare-and-set! o #f #t)
                       (yield)
                       (lp))))))
         (unlock (lambda (o) (atomic-box-set! o #f)))
         (critical
           (lambda ()
             (let ((box (make)))
               (lambda (thunk)
                 (dynamic-thread-disable-cancel)
                 (lock box)
                 (let ((ret (thunk)))
                   (unlock box)
                   (dynamic-thread-enable-cancel)
                   ret))))))
    (values make lock unlock critical)))


(define-syntax-rule
  (with-critical critical-func . bodies)
  (critical-func (lambda () . bodies)))


(define-syntax-rule
  (sleep-until condi . body)
  (let ((sleep (dynamic-thread-get-delay-procedure)))
    (do () (condi) (sleep) . body)))


(define (dynamic-thread-async-thunk thunk)
  (let ((results #f) (status #f))
    (dynamic-thread-spawn
      (lambda ()
        (catch-any
          (lambda ()
            (call-with-values
              thunk
              (lambda vals
                (set! results vals)
                (set! status 'ok))))
          (lambda errors
            (set! results errors)
            (set! status 'fail)))))
    (lambda ()
      (sleep-until status)
      (when (eq? 'fail status)
            (throw 'dynamic-thread-run-async-failed results))
      (apply values results))))


(define-syntax-rule
  (dynamic-thread-async . bodies)
  (dynamic-thread-async-thunk (lambda () . bodies)))


(define-values
  (universal-lockr! universal-unlockr!)
  (let ((critical (make-uni-spinlock-critical))
        (h (make-hash-table)))
    (values
      (lambda (resource)
        (let ((sleep (dynamic-thread-get-delay-procedure)))
          (let lp ()
            (when (with-critical
                    critical
                    (let ((r (hash-ref h resource #f)))
                      (if r #t (begin (hash-set! h resource #t) #f))))
                  (sleep)
                  (lp)))))
      (lambda (resource)
        (with-critical
          critical
          (hash-set! h resource #f))))))


(define (universal-usleep micro-seconds)
  (let* ((nano-seconds (micro->nano@unit micro-seconds))
         (start-time
           (time-get-monotonic-nanoseconds-timestamp))
         (end-time (+ start-time nano-seconds))
         (sleep-rate (#{dynamic-thread-wait-delay#us-p}#))
         (yield (dynamic-thread-get-yield-procedure)))
    (let lp ()
      (let ((t (time-get-monotonic-nanoseconds-timestamp)))
        (unless
          (> t end-time)
          (let ((s (min sleep-rate
                        (nano->micro@unit (- end-time t)))))
            (yield)
            (usleep s)
            (lp)))))))


(define-syntax monadic-bare-handle-tags
  (syntax-rules ()
    ((monadic-bare-handle-tags ()) (list))
    ((monadic-bare-handle-tags . tags) (list . tags))))


(define-syntax monadic-bare
  (syntax-rules ()
    ((_ f ((a . as) b . tags))
     (let-values
       (((r-x r-cont qvar qval qtags last?)
         (f (memconst
              (call-with-values (lambda _ b) (lambda x x)))
            (lambda args (apply values args))
            '(a . as)
            'b
            (monadic-bare-handle-tags . tags)
            #t)))
       (r-cont (r-x))))
    ((_ f ((a . as) b . tags) . bodies)
     (let-values
       (((r-x r-cont qvar qval qtags last?)
         (f (memconst
              (call-with-values (lambda _ b) (lambda x x)))
            (lambda (k)
              (apply (lambda (a . as) (monadic-bare f . bodies))
                     k))
            '(a . as)
            'b
            (monadic-bare-handle-tags . tags)
            #f)))
       (r-cont (r-x))))
    ((_ f (a b . tags))
     (let-values
       (((r-x r-cont qvar qval qtags last?)
         (f (memconst b)
            identity
            'a
            'b
            (monadic-bare-handle-tags . tags)
            #t)))
       (r-cont (r-x))))
    ((_ f (a b . tags) . bodies)
     (let-values
       (((r-x r-cont qvar qval qtags last?)
         (f (memconst b)
            (lambda (a) (monadic-bare f . bodies))
            'a
            'b
            (monadic-bare-handle-tags . tags)
            #f)))
       (r-cont (r-x))))))


(define monadic-global-parameter
  (make-parameter #f))


(define-syntax-rule
  (monadic-parameterize f . body)
  (parameterize
    ((monadic-global-parameter f))
    (begin . body)))


(define-syntax-rule
  (with-monadic-left f . body)
  (let ((current-monad (monadic-global-parameter)))
    (let ((new-monad
            (lambda (old-monad old-monad-quoted)
              (let ((applied
                      (if current-monad
                        (current-monad old-monad old-monad-quoted)
                        old-monad)))
                (compose f applied)))))
      (parameterize
        ((monadic-global-parameter new-monad))
        (begin . body)))))


(define-syntax-rule
  (with-monadic-right f . body)
  (let ((current-monad (monadic-global-parameter)))
    (let ((new-monad
            (lambda (old-monad old-monad-quoted)
              (let ((applied
                      (if current-monad
                        (current-monad old-monad old-monad-quoted)
                        old-monad)))
                (compose applied f)))))
      (parameterize
        ((monadic-global-parameter new-monad))
        (begin . body)))))


(define-syntax-rule
  (monadic fexpr . argv)
  (let* ((p (monadic-global-parameter)) (f fexpr))
    (if p
      (monadic-bare (p f (quote fexpr)) . argv)
      (monadic-bare f . argv))))


(define-syntax-rule
  (monadic-id . argv)
  (monadic identity-monad . argv))


(define (#{monad-arg#lazy}# monad-input)
  (first monad-input))


(define (monad-arg monad-input)
  ((#{monad-arg#lazy}# monad-input)))


(define (monad-cont monad-input)
  (second monad-input))


(define (monad-qvar monad-input)
  (third monad-input))


(define (monad-qval monad-input)
  (fourth monad-input))


(define (monad-qtags monad-input)
  (fifth monad-input))


(define (monad-last? monad-input)
  (sixth monad-input))


(define-syntax-rule
  (monad-cret monad-input arg cont)
  (values
    (memconst arg)
    cont
    (monad-qvar monad-input)
    (monad-qval monad-input)
    (monad-qtags monad-input)
    (monad-last? monad-input)))


(define-syntax-rule
  (monad-ret monad-input arg)
  (values
    (memconst arg)
    (monad-cont monad-input)
    (monad-qvar monad-input)
    (monad-qval monad-input)
    (monad-qtags monad-input)
    (monad-last? monad-input)))


(define-syntax-rule
  (monad-ret-id monad-input)
  (apply values monad-input))


(define (monad-handle-multiple monad-input arg)
  (let* ((qvar (monad-qvar monad-input))
         (len (if (list? qvar) (length qvar) 1)))
    (if (< len 2) arg (replicate len (arg)))))


(define (monad-replicate-multiple monad-input arg)
  (let* ((qvar (monad-qvar monad-input))
         (len (if (list? qvar) (length qvar) 1)))
    (if (< len 2) arg (replicate len arg))))


(define (except-monad)
  (let ((exceptions (list)))
    (lambda monad-input
      (if (monad-last? monad-input)
        (monad-ret
          monad-input
          (monad-handle-multiple
            monad-input
            (if (null? exceptions)
              (monad-arg monad-input)
              (apply throw 'except-monad exceptions))))
        (if (or (null? exceptions)
                (memq 'always (monad-qtags monad-input)))
          (monad-ret
            monad-input
            (catch-any
              (#{monad-arg#lazy}# monad-input)
              (lambda args
                (cons! args exceptions)
                (monad-replicate-multiple
                  monad-input
                  'monad-except-default))))
          (monad-ret
            monad-input
            (monad-handle-multiple
              monad-input
              'monad-except-default)))))))


(define log-monad
  (lambda monad-input
    (dprint
      "(~a = ~a = ~a)\n"
      (monad-qvar monad-input)
      (monad-arg monad-input)
      (monad-qval monad-input))
    (monad-ret-id monad-input)))


(define identity-monad
  (lambda monad-input (apply values monad-input)))


(define (maybe-monad predicate)
  (lambda monad-input
    (let ((arg (monad-arg monad-input)))
      (if (predicate arg)
        (monad-cret monad-input arg identity)
        (monad-ret monad-input arg)))))


(define lazy-monad
  (lambda monad-input
    (let* ((qvar (monad-qvar monad-input))
           (len (if (list? qvar) (length qvar) 1))
           (single? (< len 2))
           (result
             (if (memq 'async (monad-qtags monad-input))
               (dynamic-thread-async (monad-arg monad-input))
               (#{monad-arg#lazy}# monad-input)))
           (choose
             (lambda (i) (memconst (list-ref (result) i))))
           (return
             (if single? result (map choose (range len)))))
      (monad-ret monad-input return))))


(define (replace-monad test/replace-procedure)
  (lambda monad-input
    (let* ((tags (monad-qtags monad-input))
           (#{arg#lazy}# (#{monad-arg#lazy}# monad-input)))
      (apply values
             (cons (test/replace-procedure tags #{arg#lazy}#)
                   (cdr monad-input))))))


(define (filter-monad test-any)
  (replace-monad
    (lambda (tags #{arg#lazy}#)
      (if (or-map test-any tags)
        (lambda () 'filter-monad-skipped-evaluation)
        #{arg#lazy}#))))


(define #{call-with-finally#return-tag}#
  '#{euphrates-call-with-finally#return-error}#)


(define #{call-with-finally#return}#
  (let ((dynamic-stack (make-parameter (list))))
    (lambda (expr finally)
      (let* ((err #f)
             (normal? #t)
             (finally-executed? #f)
             (finally-wraped
               (lambda args
                 (unless
                   finally-executed?
                   (set! finally-executed? #t)
                   (apply finally args)))))
        (call-with-values
          (lambda ()
            (catch-any
              (lambda ()
                (call/cc
                  (lambda (k)
                    (parameterize
                      ((dynamic-stack
                         (cons (cons k finally-wraped) (dynamic-stack))))
                      (expr (lambda argv
                              (set! normal? #f)
                              (let lp ((st (dynamic-stack)))
                                (unless
                                  (null? st)
                                  (let ((p (car st)))
                                    ((cdr p))
                                    (when (not (eq? (car p) k))
                                          (lp (cdr st))))))
                              (apply k argv)))))))
              (lambda args (set! err args))))
          (lambda ret
            (when normal? (finally-wraped))
            (when err (apply throw err))
            (apply values ret)))))))


(define (call-with-finally expr finally)
  (let ((err #f))
    (catch-any
      expr
      (lambda errors (set! err errors)))
    (finally)
    (when err (apply throw err))))


(define read-all-port
  (case-lambda
    ((readf port)
     "`readf' is usually `read-char' or `read-byte'"
     (let loop ((result '()) (chr (readf port)))
       (if (eof-object? chr)
         (list->string (reverse result))
         (loop (cons chr result) (readf port)))))
    ((port) (read-all-port read-char port))))


(define (read-string-file path)
  (let* ((in (open-file path "r"))
         (text (read-all-port read-char in))
         (go (close-port in)))
    text))


(define (write-string-file path data)
  (let* ((out (open-file path "w"))
         (re (display data out))
         (go (close-port out)))
    re))


(define (append-string-file path data)
  (let* ((out (open-file path "a"))
         (re (display data out))
         (go (close-port out)))
    re))


(define (path-redirect newbase path)
  (let ((oldbase (take-common-prefix newbase path)))
    (string-append
      newbase
      (substring path (string-length oldbase)))))


(define (path-rebase newbase path)
  (let ((oldbase (take-common-prefix newbase path)))
    (if (string-null? oldbase)
      #f
      (string-append
        newbase
        (substring path (string-length oldbase))))))


(define-rec
  np-thread-obj
  continuation
  cancel-scheduled?
  cancel-enabled?)


(define (np-thread-parameterize-env make-critical thunk)
  (define (make-np-thread-obj thunk)
    (np-thread-obj thunk #f #t))
  (define thread-queue (make-queue 16))
  (define current-thread #f)
  (define critical (make-critical))
  (define (np-thread-list-add th)
    (with-critical
      critical
      (queue-push! thread-queue th)))
  (define (np-thread-list-switch)
    (with-critical
      critical
      (let loop ((head (queue-pop! thread-queue #f)))
        (if (not head)
          'np-thread-empty-list
          (if (and (np-thread-obj-cancel-scheduled? head)
                   (np-thread-obj-cancel-enabled? head))
            (begin (loop (queue-pop! thread-queue #f)))
            (begin (set! current-thread head) head))))))
  (define start-point #f)
  (define (np-thread-end)
    (let ((p (np-thread-list-switch)))
      (if (eq? p 'np-thread-empty-list)
        (start-point)
        (begin
          ((np-thread-obj-continuation p))
          (np-thread-end)))))
  (define (np-thread-yield)
    (let ((me current-thread))
      (when (and (np-thread-obj-cancel-scheduled? me)
                 (np-thread-obj-cancel-enabled? me))
            (throw dynamic-thread-cancel-tag))
      (call/cc
        (lambda (k)
          (set-np-thread-obj-continuation! me k)
          (np-thread-list-add me)
          (np-thread-end)))))
  (define (np-thread-fork thunk)
    (let ((first? #t) (ret #f))
      (call/cc
        (lambda (k)
          (set! ret (make-np-thread-obj k))
          (np-thread-list-add ret)))
      (unless first? (thunk) (np-thread-end))
      (set! first? #f)
      ret))
  (define (np-thread-run! thunk)
    (call/cc
      (lambda (k)
        (set! start-point k)
        (set! current-thread (make-np-thread-obj thunk))
        (thunk)
        (np-thread-end))))
  (define #{np-thread-cancel!#unsafe}#
    (case-lambda (() (np-thread-end)) ((chosen) 0)))
  (define (np-thread-cancel! chosen)
    (set-np-thread-obj-cancel-scheduled?! chosen #t)
    (when (np-thread-obj-cancel-enabled? chosen)
          (#{np-thread-cancel!#unsafe}# chosen)))
  (define (np-thread-make-critical)
    (lambda (fn)
      (let* ((me current-thread))
        (set-np-thread-obj-cancel-enabled?! me #f)
        (fn)
        (set-np-thread-obj-cancel-enabled?! me #t)
        (np-thread-yield))))
  (define (np-thread-disable-cancel)
    (let ((me current-thread))
      (set-np-thread-obj-cancel-enabled?! me #f)))
  (define (np-thread-enable-cancel)
    (let ((me current-thread))
      (set-np-thread-obj-cancel-enabled?! me #t)))
  (parameterize
    ((dynamic-thread-spawn-p np-thread-fork)
     (dynamic-thread-cancel-p np-thread-cancel!)
     (dynamic-thread-disable-cancel-p
       np-thread-disable-cancel)
     (dynamic-thread-enable-cancel-p
       np-thread-enable-cancel)
     (dynamic-thread-yield-p np-thread-yield)
     (dynamic-thread-sleep-p universal-usleep)
     (dynamic-thread-mutex-make-p make-unique)
     (dynamic-thread-mutex-lock!-p universal-lockr!)
     (dynamic-thread-mutex-unlock!-p
       universal-unlockr!)
     (dynamic-thread-critical-make-p
       np-thread-make-critical))
    (np-thread-run! thunk)))


(define-syntax-rule
  (#{with-np-thread-env#non-interruptible}#
    .
    bodies)
  (np-thread-parameterize-env
    (lambda () (lambda (fn) (fn)))
    (lambda () . bodies)))


(define comprocess-stdout (make-parameter #f))


(define comprocess-stderr (make-parameter #f))


(define #{run-comprocess#p}#
  (make-parameter #{run-comprocess#p-default}#))


(define run-comprocess
  (lambda args (apply (#{run-comprocess#p}#) args)))


(define #{kill-comprocess#p}#
  (make-parameter #{kill-comprocess#p-default}#))


(define kill-comprocess
  (lambda args
    (apply (#{kill-comprocess#p}#) args)))


(define (kill-comprocess-with-timeout p timeout)
  (unless
    (comprocess-exited? p)
    (kill-comprocess p #f)
    (unless
      (comprocess-exited? p)
      (dynamic-thread-spawn
        (lambda ()
          (dynamic-thread-sleep timeout)
          (unless
            (comprocess-exited? p)
            (kill-comprocess p #t)))))))


(begin-for-syntax
  (define-syntax-rule
    (generate-add-name name)
    (generate-prefixed-name 'gfunc/instantiate- name))
  (define-syntax-rule
    (generate-param-name name)
    (generate-prefixed-name
      'gfunc/parameterize-
      name)))


(define (check-list-contract check-list args)
  (or (not check-list)
      (and (= (length check-list) (length args))
           (fold (lambda (p x c) (and c (p x)))
                 #t
                 check-list
                 args))))


(define-syntax gfunc/define
  (lambda (stx)
    (syntax-case stx ()
      ((gfunc/define name)
       (with-syntax ((add-name (generate-add-name (syntax name)))
                     (param-name (generate-param-name (syntax name))))
         (syntax
           (define-values
             (name add-name param-name)
             (let ((internal-list (make-parameter '()))
                   (critical (make-uni-spinlock-critical)))
               (values
                 (lambda args
                   (let ((m (find-first
                              (lambda (p) (check-list-contract (car p) args))
                              (internal-list))))
                     (if m
                       (apply (cdr m) args)
                       (throw 'gfunc-no-instance-found
                              (string-append
                                "No gfunc instance of "
                                (symbol->string (syntax->datum (syntax name)))
                                " accepts required arguments")))))
                 (lambda (args func)
                   (with-critical
                     critical
                     (set! internal-list
                       (make-parameter
                         (append (internal-list) (list (cons args func)))))))
                 (lambda (args func body)
                   (let ((new-list
                           (cons (cons args func) (internal-list))))
                     (parameterize
                       ((internal-list new-list))
                       (body)))))))))))))


(define-syntax gfunc/parameterize
  (lambda (stx)
    (syntax-case stx ()
      ((gfunc/parameterize name check-list func . body)
       (with-syntax ((param-name (generate-param-name (syntax name))))
         (syntax
           (param-name check-list func (lambda () . body))))))))


(define-syntax gfunc/instance
  (lambda (stx)
    (syntax-case stx ()
      ((gfunc/instance name check-list func)
       (with-syntax ((add-name (generate-add-name (syntax name))))
         (syntax (add-name (list . check-list) func)))))))


(define-syntax with-svars-helper
  (syntax-rules ()
    ((_ dict buf () body)
     (lambda dict (let buf body)))
    ((_ dict buf (name . names) body)
     (with-svars-helper
       dict
       ((name (let ((z (assq 'name dict))) (if z (cdr z) name)))
        .
        buf)
       names
       body))))


(define-syntax-rule
  (with-svars names body)
  (with-svars-helper dd () names body))


(define-syntax use-svars-helper
  (syntax-rules ()
    ((_ () () f) (f))
    ((_ buf () f) (f . buf))
    ((_ buf ((name value) . names) f)
     (use-svars-helper
       ((cons (quote name) value) . buf)
       names
       f))))


(define-syntax-rule
  (use-svars f . renames)
  (use-svars-helper () renames f))


(define-syntax make-static-package-helper
  (syntax-rules ()
    ((_ hh buf ())
     (let ((hh (make-hash-table))) (begin . buf) hh))
    ((_ hh buf ((name value) . definitions))
     (make-static-package-helper
       hh
       ((hash-set! hh (quote name) value) . buf)
       definitions))))


(define-syntax-rule
  (make-static-package definitions)
  (make-static-package-helper hh () definitions))


(define-syntax-rule
  (make-package inputs definitions)
  (with-svars
    inputs
    (make-static-package definitions)))


(define-syntax with-package-helper
  (syntax-rules ()
    ((_ inst () body) body)
    ((_ inst (name . names) body)
     (let ((name (hash-ref inst 'name)))
       (with-package-helper inst names body)))))


(define-syntax with-package-renames-helper-pre
  (syntax-rules ()
    ((_ (package . renames))
     (use-svars package . renames))
    ((_ package) (package))))


(define-syntax-rule
  (with-package package-spec names body)
  (let ((inst (with-package-renames-helper-pre package-spec)))
    (with-package-helper inst names body)))


(define (hash->mdict h)
  (let ((unique (make-unique)))
    (case-lambda
      (() h)
      ((key)
       (let ((g (hash-ref h key unique)))
         (if (unique g)
           (throw 'mdict-key-not-found key h)
           g)))
      ((key value)
       (let* ((new (make-hash-table)))
         (hash-table-foreach
           h
           (lambda (key value) (hash-set! new key value)))
         (hash-set! new key value)
         (hash->mdict new))))))


(define (alist->mdict alist)
  (hash->mdict (alist->hash-table alist)))


(define-syntax mdict-c
  (syntax-rules ()
    ((mdict-c carry) (alist->mdict carry))
    ((mdict-c carry key value . rest)
     (mdict-c (cons (cons key value) carry) . rest))))


(define-syntax-rule
  (mdict . entries)
  (mdict-c (quote ()) . entries))


(define (mdict-has? h-func key)
  (let ((h (h-func))) (hash-get-handle h key)))


(define (mdict-set! h-func key value)
  (let ((h (h-func))) (hash-set! h key value)))


(define (mdict->alist h-func)
  (let ((h (h-func))) (hash-table->alist h)))


(define (mdict-keys h-func)
  (map car (mdict->alist h-func)))


(define (shell-check-status p)
  (unless
    (equal? 0 (comprocess-status p))
    (throw 'shell-process-failed
           `(cmd: ,(comprocess-command p))
           p)))


(define (shell-inputs-to-comprocess-args inputs)
  (if (= 1 (length inputs))
    (let ((cmd (car inputs)))
      (if (list? cmd)
        (values cmd cmd)
        (values (list "/bin/sh" "-c" cmd) cmd)))
    (let ((cmd (apply stringf inputs)))
      (values (list "/bin/sh" "-c" cmd) cmd))))


(define (sh-async-no-log . inputs)
  (apply run-comprocess
         (shell-inputs-to-comprocess-args inputs)))


(define (sh-async . inputs)
  (monadic-id
    ((args cmd)
     (shell-inputs-to-comprocess-args inputs))
    (ret (apply run-comprocess args))
    (do (debug "> ~a" cmd) `(sh-cmd ,cmd) 'sh-log)
    (do ret)))


(define (sh . inputs)
  (monadic-id
    (p (apply sh-async inputs))
    (do (sleep-until (comprocess-exited? p)))
    (do (shell-check-status p))
    (do (kill-comprocess-with-timeout
         p
         (normal->micro@unit 1/2))
        'always
      'sh-kill-on-error
      p)
    (do p)))


(define (sh-re . inputs)
  (string-trim-chars
    (with-output-to-string
      (lambda _ (apply sh inputs)))
    "\n \t"
    'both))


(define-syntax-rule
  (with-no-shell-log . bodies)
  (with-monadic-right
    (filter-monad
      (lambda (tag)
        (case tag ((sh-log sh-return-log) #t) (else #f))))
    .
    bodies))


(define (system-re command)
  "Like `system', but returns (output, exit status)"
  (monadic-id
    (temp (make-temporary-filename))
    (p (system*/exit-code
         "/bin/sh"
         "-c"
         (string-append command " > " temp)))
    (output (read-string-file temp))
    (trimed (string-trim-chars output "\n \t" 'both))
    (do (cons trimed p))))


(define (parse-cli args)
  (define (trim s) (string-trim-chars s "-" 'left))
  (let lp ((pos 0) (buf (list (cons #f #t))) (left args))
    (if (null? left)
      buf
      (let ((current (car left)))
        (cond ((string-prefix? "-" current)
               (let* ((key (trim current))
                      (cell (cons key #t))
                      (rest (cdr left)))
                 (lp pos (cons cell buf) rest)))
              (#t
               (lp (#{1+}# pos)
                   (cons (cons pos current) buf)
                   (cdr left))))))))


(define parse-cli-global-p (make-parameter #f))


(define parse-cli-global-default
  (let ((value #f))
    (case-lambda
      (() value)
      ((new-value) (set! value new-value) new-value))))


(define (parse-cli!)
  (let ((parsed (parse-cli (get-command-line-arguments))))
    (parse-cli-global-default parsed)
    parsed))


(define (parse-cli-parse-or-get!)
  (if (parse-cli-global-p)
    (parse-cli-global-p)
    (if (parse-cli-global-default)
      (parse-cli-global-default)
      (parse-cli!))))


(define (parse-cli-get-flag . keys)
  (let* ((parsed (parse-cli-parse-or-get!)))
    (or-map
      (lambda (key)
        (let ((ret (assoc key parsed)))
          (if (pair? ret) (cdr ret) ret)))
      keys)))


(define (parse-cli-get-switch key)
  (let ((parsed (parse-cli-parse-or-get!)))
    (let lp ((rest parsed) (prev #f))
      (if (null? rest)
        #f
        (let* ((current (car rest))
               (k (car current))
               (v (cdr current)))
          (if (equal? k key)
            (if prev
              prev
              (throw 'command-line-flag-should-be-a-switch key))
            (lp (cdr rest) v)))))))


(define (parse-cli-get-list after-key)
  (let* ((parsed (parse-cli-parse-or-get!)))
    (let lp ((rest (reverse parsed)) (found? #f))
      (if (null? rest)
        (if found? (list) found?)
        (let ((key (car (car rest))) (val (cdr (car rest))))
          (if found?
            (if (integer? key)
              (cons val (lp (cdr rest) found?))
              (list))
            (if (equal? key after-key)
              (lp (cdr rest) (not found?))
              (lp (cdr rest) found?))))))))


(define-rec
  tree-future
  parent-index
  current-index
  children-list
  finally
  callback
  thread
  #{evaluated?#box}#
  evaluated?
  children-finished?
  finished?
  context)


(define tree-future-current (make-parameter #f))


(define tree-future-eval-context-p
  (make-parameter #f))


(define (tree-future-eval-context)
  ((tree-future-eval-context-p)))


(define (tree-future-get)
  (letrec ((message-bin null)
           (message-bin-lock (dynamic-thread-critical-make))
           (init-lock (dynamic-thread-critical-make))
           (work-thread #f)
           (futures-hash (make-hash-table))
           (get-by-index
             (lambda (index) (hash-ref futures-hash index #f)))
           (wait-all
             (lambda () (sleep-until (not work-thread))))
           (logger
             (lambda (fmt . args)
               (apply dprintln (cons fmt args))))
           (send-message
             (lambda type-args
               (with-critical
                 message-bin-lock
                 (set! message-bin (cons type-args message-bin)))))
           (run-finally
             (lambda (structure status results)
               (when (tree-future-finally structure)
                     (catch-any
                       (lambda ()
                         (apply (tree-future-finally structure)
                                (cons* structure status results)))
                       (lambda errs 0)))))
           (finish
             (lambda (structure status results)
               (run-finally structure status results)
               (send-message 'finish-async structure)
               (sleep-until
                 (tree-future-children-finished? structure))
               (let ((errs #f))
                 (when (tree-future-callback structure)
                       (catch-any
                         (lambda ()
                           (apply (tree-future-callback structure)
                                  (cons* structure status results)))
                         (lambda err (set! errs err))))
                 (set-tree-future-finished?! structure #t)
                 (send-message 'remove structure)
                 (when errs (apply throw errs)))))
           (cancel-children
             (lambda (structure args)
               (for-each
                 (lambda (child)
                   (cancel-future-sync
                     child
                     'down
                     (list 'parent-cancelled-with args)))
                 (tree-future-children-list structure))))
           (cancel-future-sync
             (lambda (structure mode args)
               (when (atomic-box-compare-and-set!
                       (#{tree-future-evaluated?#box}# structure)
                       #f
                       'cancelled)
                     (dynamic-thread-cancel
                       (tree-future-thread structure))
                     (dynamic-thread-spawn
                       (lambda () (finish structure 'cancel args))))
               (case mode
                 ((single) 0)
                 ((down) (cancel-children structure args))
                 ((all)
                  (cancel-children structure args)
                  (let ((parent
                          (get-by-index
                            (tree-future-parent-index structure))))
                    (when parent
                          (cancel-future-sync
                            parent
                            'all
                            (list 'child-cancelled-with args))))))))
           (children-finished?
             (lambda (structure)
               (and-map
                 tree-future-finished?
                 (tree-future-children-list structure))))
           (remove-future-sync
             (lambda (structure)
               (when (children-finished? structure)
                     (when (tree-future-evaluated? structure)
                           (set-tree-future-children-finished?!
                             structure
                             #t))
                     (when (tree-future-finished? structure)
                           (hash-remove!
                             futures-hash
                             (tree-future-current-index structure))
                           (dispatch
                             'remove
                             (tree-future-parent-index structure))))))
           (retry-later
             (lambda (type args)
               (apply send-message (cons type args))))
           (dispatch
             (lambda (type . args)
               (case type
                 ((start)
                  (match args
                         (`(,structure)
                          (let ((parent
                                  (get-by-index
                                    (tree-future-parent-index structure))))
                            (if (and parent (tree-future-finished? parent))
                              (logger "parent is already done")
                              (begin
                                (hash-set!
                                  futures-hash
                                  (tree-future-current-index structure)
                                  structure)
                                (when parent
                                      (set-tree-future-children-list!
                                        parent
                                        (cons structure
                                              (tree-future-children-list
                                                parent))))))))
                         (else (logger
                                 "wrong number of arguments to 'start"))))
                 ((finish-async)
                  (match args
                         (`(,structure)
                          (set-tree-future-evaluated?! structure #t)
                          (remove-future-sync structure))
                         (else (logger
                                 "wrong number of arguments to 'finish-async"))))
                 ((remove)
                  (match args
                         (`(,index)
                          (if (tree-future? index)
                            (remove-future-sync index)
                            (let ((structure (get-by-index index)))
                              (when structure
                                    (remove-future-sync structure)))))
                         (else (logger
                                 "wrong number of arguments to 'remove"))))
                 ((cancel)
                  (if (or (null? args) (null? (cdr args)))
                    (logger "wrong number of arguments to 'remove")
                    (let ((index (car args))
                          (mode (cadr args))
                          (arguments (cddr args)))
                      (case mode
                        ((single down all)
                         (let* ((structure (get-by-index index)))
                           (if structure
                             (if (tree-future-thread structure)
                               (cancel-future-sync structure mode args)
                               (retry-later type args))
                             (logger "bad index"))))
                        (else (logger "bad mode"))))))
                 ((context)
                  (match args
                         (`(,target-index ,transformer)
                          (let ((target (get-by-index target-index)))
                            (if target
                              (let ((current (tree-future-context target)))
                                (set-tree-future-context!
                                  target
                                  (let ((saved? #f) (memory #f))
                                    (lambda ()
                                      (unless
                                        saved?
                                        (set! saved? #t)
                                        (set! memory (transformer (current))))
                                      memory))))
                              (logger "target doesnt exist"))))
                         (else (logger
                                 "wrong number of arguments to 'context"))))
                 (else (logger "bad op type")))))
           (recieve-loop
             (lambda ()
               (let ((sleep (dynamic-thread-get-delay-procedure)))
                 (let lp ()
                   (let ((val null))
                     (with-critical
                       message-bin-lock
                       (begin
                         (set! val message-bin)
                         (set! message-bin null)))
                     (for-each
                       (lambda (elem) (apply dispatch elem))
                       (reverse val)))
                   (if (hash-empty? futures-hash)
                     (set! work-thread #f)
                     (begin (sleep) (lp)))))))
           (maybe-start-loopin
             (lambda ()
               (init-lock
                 (lambda ()
                   (unless
                     work-thread
                     (set! work-thread
                       (dynamic-thread-spawn recieve-loop)))))))
           (run (lambda (target-procedure
                         finally
                         callback
                         initial-context)
                  (let* ((context (lambda () initial-context))
                         (parent-index (tree-future-current))
                         (current-index (make-unique))
                         (structure
                           (tree-future
                             parent-index
                             current-index
                             null
                             finally
                             callback
                             #f
                             (make-atomic-box #f)
                             #f
                             #f
                             #f
                             context))
                         (eval-context
                           (lambda () ((tree-future-context structure)))))
                    (send-message 'start structure)
                    (set-tree-future-thread!
                      structure
                      (dynamic-thread-spawn
                        (lambda ()
                          (parameterize
                            ((tree-future-current current-index)
                             (tree-future-eval-context-p eval-context))
                            (let ((results #f) (status 'undefined))
                              (catch-any
                                (lambda ()
                                  (call-with-values
                                    target-procedure
                                    (lambda vals
                                      (set! status 'ok)
                                      (set! results vals))))
                                (lambda err
                                  (set! status 'error)
                                  (set! results err)))
                              (when (atomic-box-compare-and-set!
                                      (#{tree-future-evaluated?#box}#
                                        structure)
                                      #f
                                      #t)
                                    (finish structure status results)))))))
                    (maybe-start-loopin)
                    current-index))))
    (values run send-message wait-all)))


(define tree-future-send-message-p
  (make-parameter #f))


(define (tree-future-send-message . args)
  (apply (tree-future-send-message-p) args))


(define tree-future-run-p (make-parameter #f))


(define (tree-future-run . args)
  (apply (tree-future-run-p) args))


(define (tree-future-modify target-index transformation)
  (tree-future-send-message
    'context
    target-index
    transformation))


(define (tree-future-cancel target-index . arguments)
  (apply tree-future-send-message
         (cons* 'cancel target-index arguments)))


(define-syntax-rule
  (with-new-tree-future-env . bodies)
  (let-values
    (((run send-message wait) (tree-future-get)))
    (parameterize
      ((tree-future-run-p run)
       (tree-future-send-message-p send-message))
      (call-with-finally
        (lambda _ (begin . bodies))
        (lambda _ (wait))))))


(define-rec
  tree-future-task
  touch-procedure
  child-index)


(define (tree-future-run-task-thunk
         thunk
         user-finally
         user-callback
         initial-context)
  (let ((finished? #f)
        (evaluated? #f)
        (results #f)
        (status #f)
        (structure #f))
    (define (callback cb-structure cb-status . cb-results)
      (set! structure cb-structure)
      (catch-any
        (lambda () (tree-future-eval-context))
        (lambda errs 0))
      (set! finished? #t)
      (when user-callback
            (apply user-callback
                   (cons* cb-structure cb-status cb-results))))
    (define (finally structure cb-status . cb-results)
      (set! results cb-results)
      (set! status cb-status)
      (set! evaluated? #t)
      (when user-finally
            (apply user-finally
                   (cons* structure cb-status cb-results))))
    (define (wait target)
      (case target
        ((evaluation)
         (unless evaluated? (sleep-until evaluated?)))
        ((children)
         (unless finished? (sleep-until finished?)))))
    (define (done? target)
      (case target
        ((evaluation) evaluated?)
        ((children) finished?)))
    (define touch-procedure
      (case-lambda
        (() (touch-procedure 'default 'children))
        ((type target)
         (case type
           ((default)
            (wait target)
            (case status
              ((ok) (apply values results))
              (else
               (throw 'tree-future-await-failed status results))))
           ((no-throw)
            (wait target)
            (values structure status results))
           ((check)
            (values (done? target) structure status results))
           (else (throw 'unknown-touch-type type))))))
    (define child-index
      (tree-future-run
        thunk
        finally
        callback
        initial-context))
    (tree-future-task touch-procedure child-index)))


(define-syntax-rule
  (tree-future-run-task . bodies)
  (tree-future-run-task-thunk
    (lambda () . bodies)
    #f
    #f
    #f))


(define tree-future-wait-task
  (case-lambda
    ((task)
     ((tree-future-task-touch-procedure task)))
    (tasks (map tree-future-wait-task tasks))))


(define-syntax-rule
  (%with-dynamic-helper1 arg value . bodies)
  (if (parameter? arg)
    (parameterize ((arg value)) . bodies)
    (arg (lambda _ value) (lambda _ . bodies))))


(define-syntax with-dynamic
  (syntax-rules ()
    ((_ ((arg value)) . bodies)
     (%with-dynamic-helper1 arg value . bodies))
    ((_ ((arg value) . rest) . bodies)
     (%with-dynamic-helper1
       arg
       value
       (with-dynamic rest . bodies)))))


(define-syntax lazy-parameter
  (syntax-rules ()
    ((_ initial)
     (let ((p (make-parameter (memconst initial))))
       (case-lambda
         (() ((p)))
         ((value body)
          (let ((mem (memconst (value))))
            (parameterize ((p mem)) (body)))))))
    ((_ initial converter)
     (let ((p (make-parameter (memconst initial))))
       (case-lambda
         (() ((p)))
         ((value body)
          (let ((mem (memconst (converter (value)))))
            (parameterize ((p mem)) (body)))))))))


(define make-queue
  (case-lambda
    (() (make-queue 10))
    ((initial-size)
     (let ((ret (make-vector 4)))
       (vector-set! ret 0 (make-vector initial-size))
       (vector-set! ret 1 0)
       (vector-set! ret 2 0)
       (vector-set! ret 3 'euphrates-queue)
       ret))))


(define (queue-empty? q)
  (= (vector-ref q 1) (vector-ref q 2)))


(define (queue-peek q)
  (if (= (vector-ref q 1) (vector-ref q 2))
    (throw 'empty-queue-peek)
    (vector-ref (vector-ref q 0) (vector-ref q 1))))


(define (queue-push! q value)
  (let* ((v (vector-ref q 0))
         (first (vector-ref q 1))
         (last (vector-ref q 2))
         (size (vector-length v))
         (last+1 (+ 1 last))
         (new-last (if (< last+1 size) last+1 0))
         (need-realloc? (= new-last first))
         (new-size (* 2 size))
         (v (if (not need-realloc?)
              v
              (let ((ret (make-vector new-size)))
                (let loop ((i 0) (j first))
                  (when (< i size)
                        (vector-set! ret i (vector-ref v j))
                        (loop (add1 i) (if (< (add1 j) size) (add1 j) 0))))
                (vector-set! q 0 ret)
                ret))))
    (if need-realloc?
      (begin
        (vector-set! q 1 0)
        (vector-set! q 2 size)
        (vector-set! v (sub1 size) value))
      (begin
        (vector-set! q 2 new-last)
        (vector-set! v last value)))))


(define queue-pop!
  (let ((private-default (make-unique)))
    (case-lambda
      ((q)
       (let ((ret (queue-pop! q private-default)))
         (if (eq? private-default ret)
           (throw 'empty-queue-peek)
           ret)))
      ((q default)
       (if (= (vector-ref q 1) (vector-ref q 2))
         default
         (let* ((v (vector-ref q 0))
                (size (vector-length v))
                (first (vector-ref q 1))
                (first+1 (+ 1 first))
                (new-first (if (< first+1 size) first+1 0)))
           (vector-set! q 1 new-first)
           (vector-ref v first)))))))


(define guile-printf
  (let ((critical (make-uni-spinlock-critical)))
    (lambda (fmt . args)
      (let ((err #f))
        (critical
          (lambda ()
            (catch-any
              (lambda () (apply format (cons* #t fmt args)))
              (lambda argv (set! err argv)))))
        (when err (apply throw err))))))


(define global-interrupt-frequency-p
  (make-parameter 1000000))


(define-values
  (i-thread-yield i-thread-dont-yield)
  (let ((interruptor-thread #f)
        (lst (list))
        (interruptor-finished? #t))
    (define (interruptor-loop)
      (if (null? lst)
        (set! interruptor-finished? #t)
        (begin
          (map (lambda (th)
                 (system-async-mark dynamic-thread-yield th))
               lst)
          (usleep 900000)
          (interruptor-loop))))
    (values
      (lambda (thread)
        (when interruptor-finished?
              (set! interruptor-thread
                (call-with-new-thread interruptor-loop)))
        (system-async-mark
          (lambda ()
            (unless
              (member thread lst)
              (set! lst (cons thread lst))))
          interruptor-thread))
      (lambda (thread)
        (unless
          interruptor-finished?
          (system-async-mark
            (lambda ()
              (set! lst
                (filter
                  (lambda (th) (not (equal? th thread)))
                  lst)))
            interruptor-thread))))))


(define (i-thread-yield-me)
  (i-thread-yield
    ((@ (ice-9 threads) current-thread))))


(define (i-thread-dont-yield-me)
  (i-thread-dont-yield
    ((@ (ice-9 threads) current-thread))))


(define-syntax-rule
  (i-thread-run! . thunk)
  (dynamic-wind
    i-thread-yield-me
    (lambda () (begin . thunk))
    i-thread-dont-yield-me))


(define-values
  (i-thread-critical-points
    i-thread-critical-points-append!
    i-thread-critical-points-remove!
    i-thread-critical-points-print)
  (let ((lst (list)) (mut (make-uni-spinlock-critical)))
    (values
      (lambda () lst)
      (lambda (st)
        (with-critical mut (set! lst (cons st lst))))
      (lambda (st)
        (with-critical
          mut
          (set! lst
            (filter (lambda (el) (not (equal? el st))) lst))))
      (lambda ()
        (format #t "--- CRITICAL POINTS ---\n")
        (for-each
          (lambda (st)
            (display-backtrace st (current-output-port)))
          lst)
        (format #t "--- END CRITICAL POINTS ---\n")))))


(define-syntax-rule
  (i-thread-critical! . thunk)
  "\n  Will not interrupt during execution of `thunk'\n  Unsafe: must finish quick!\n  "
  (call-with-blocked-asyncs
    (lambda ()
      (let ((st (make-stack #t)))
        (i-thread-critical-points-append! st)
        (dynamic-thread-disable-cancel)
        (begin . thunk)
        (dynamic-thread-enable-cancel)
        (i-thread-critical-points-remove! st)))))


(define (i-thread-critical-b! thunk finally)
  "\n  Same as `i-thread-critical' but also puts `thunk' and `finally' to `call-with-finally' clause\n  "
  (i-thread-critical!
    (call-with-finally thunk finally)))


(define (#{i-thread-parameterize-env#interruptible}#
         thunk)
  (parameterize
    ((dynamic-thread-critical-make-p
       (lambda ()
         (lambda (fn) (i-thread-critical! (fn)))))
     (dynamic-thread-mutex-make-p make-unique)
     (dynamic-thread-mutex-lock!-p universal-lockr!)
     (dynamic-thread-mutex-unlock!-p
       universal-unlockr!))
    (i-thread-run! (thunk))))


(define-syntax-rule
  (#{with-i-thread-env#interruptible}# . bodies)
  (#{i-thread-parameterize-env#interruptible}#
    (lambda () . bodies)))








(use-modules (ice-9 regex))

(use-modules (ice-9 rdelim))

(define (choose-args . is)
  (define (mem x) (member x is))
  (lambda args
    (apply values
           (map car
                (filter
                  (compose mem cdr)
                  (map cons args (range (length args))))))))


(define-syntax-rule
  (parameterize-fn alist fn)
  (lambda args
    (parameterize alist (apply fn args))))


(define (cached-record-accessor get reload set)
  (lambda (f)
    (or (get f)
        (let ((new (reload f))) (set f new) new))))


(define (create-empty-file name)
  (write-string-file name ""))


(define (replace-words alist in-text)
  (define (tostring x)
    (cond ((string? x) x)
          ((symbol? x) (symbol->string x))
          ((number? x) (number->string x))
          (else (throw 'bad-alist-in-replace-words alist))))
  (define (replace-word word by in-text)
    (regexp-substitute/global
      #f
      (tostring word)
      in-text
      'pre
      (tostring by)
      'post))
  (let lp ((t in-text) (rest alist))
    (if (null? rest)
      t
      (let* ((top (car rest))
             (new (replace-word (car top) (cdr top) t)))
        (lp new (cdr rest))))))


(define (replace-words-in-lines alist in-lines)
  (if (null? alist)
    in-lines
    (let ((squashed
            (let lp ((rest in-lines) (mem #f) (buf (list)))
              (if (null? rest)
                (if mem (reverse (cons mem buf)) (reverse buf))
                (let ((cur (car rest)))
                  (if (string? cur)
                    (lp (cdr rest)
                        (if mem (string-append mem "\n" cur) cur)
                        buf)
                    (lp (cdr rest)
                        #f
                        (cons cur (if mem (cons mem buf) buf)))))))))
      (let lp ((buf squashed))
        (if (null? buf)
          (list)
          (let ((cur (car buf)))
            (if (string? cur)
              (let ((repl (replace-words alist cur)))
                (append (lines repl) (lp (cdr buf))))
              (cons cur (lp (cdr buf))))))))))


(define (temporary-file? path)
  (string-prefix?
    #{czempak-temp-directory#value#prefixed}#
    path))


(define (global-file? path)
  (string-prefix?
    (get-package-root-directory)
    path))


(define (local-file? path)
  (equal? 'local (get-file-type path)))


(define (get-file-type path)
  (cond ((temporary-file? path) 'temp)
        ((global-file? path) 'global)
        (else 'local)))


(define (get-fullpath path)
  (if (string-prefix? "/" path)
    path
    (simplify-posix-path
      (append-posix-path (getcwd) path))))


(define temporary-files-table (make-hash-table))


(define (get-temporary-path)
  (append-posix-path
    (get-czempak-temp-directory)
    (~a (big-random-int 999999))))


(define (write-to-temporary original-path . parts)
  (let ((path (get-temporary-path)))
    (cmd-echo "TEMP" path original-path)
    (hash-set!
      temporary-files-table
      path
      (apply string-append parts))
    path))


(define-syntax-rule
  (with-new-temporary-file text path . bodies)
  (let* ((path (write-to-temporary "" text))
         (_ (write-string-file path (czempak-read-file path)))
         (result (let () . bodies)))
    result))


(define (czempak-read-file path)
  (if (temporary-file? path)
    (hash-ref temporary-files-table path #f)
    (read-string-file path)))


(define (czempak-call-with-input-file path body)
  (if (temporary-file? path)
    (call-with-input-string
      (hash-ref temporary-files-table path #f)
      body)
    (call-with-input-file path body)))


(define-syntax-rule
  (define-file name path)
  (define name
    (memconst
      (let ((ret path))
        (unless
          (file-or-directory-exists? ret)
          (create-empty-file ret))
        ret))))


(define-file
  get-register-graph-file
  (append-posix-path
    (get-package-store-directory)
    "register.dat"))


(define-file
  get-binregister-graph-file
  (append-posix-path
    (get-package-store-directory)
    "binregister.dat"))


(define-file
  get-install-table-file
  (append-posix-path
    (get-package-store-directory)
    "install.dat"))


(define (register-package hash dependency-hashes)
  (define deps (or dependency-hashes (list)))
  (define row-data (cons hash deps))
  (define row
    (string-append (unwords row-data) "\n"))
  (apply cmd-echo
         (cons* "REG" hash dependency-hashes))
  (append-string-file
    (get-register-graph-file)
    row))


(define (register-bin hash dependency-paths)
  (define (shorten-path path)
    (remove-common-prefix
      path
      (get-czempak-root-prefix)))
  (define deps (or dependency-paths (list)))
  (define row-data0 (cons hash deps))
  (define row-data (map shorten-path row-data0))
  (define row
    (string-append (unwords row-data) "\n"))
  (apply cmd-echo (cons "RBIN" row-data0))
  (append-string-file
    (get-binregister-graph-file)
    row))


(define-syntax list-fold*
  (syntax-rules ()
    ((_ ((acc-x . acc-xs) acc-value)
        (i-name i-value)
        .
        bodies)
     (let loop ((acc-list
                  (call-with-values
                    (lambda _ acc-value)
                    (lambda x x)))
                (i-all i-value))
       (if (null? i-all)
         (apply values acc-list)
         (let ((i-name (car i-all)))
           (call-with-values
             (lambda _ (apply values acc-list))
             (lambda (acc-x . acc-xs)
               (call-with-values
                 (lambda _ . bodies)
                 (lambda new-acc (loop new-acc (cdr i-all))))))))))
    ((_ (acc-name acc-value)
        (i-name i-value)
        .
        bodies)
     (let loop ((acc-name acc-value) (i-all i-value))
       (if (null? i-all)
         acc-name
         (let ((i-name (car i-all)))
           (let ((new-acc (let () . bodies)))
             (loop new-acc (cdr i-all)))))))))


(define (make-directories path)
  (define mk-single-dir mkdir)
  (define parts (string-split path #\/))
  (list-fold*
    (acc #f)
    (i parts)
    (let* ((path0 (if acc (append-posix-path acc i) i))
           (path (if (string-null? path0) "/" path0)))
      (unless (file-exists? path) (mk-single-dir path))
      path)))


(define (#{make-directory#transitive}# path)
  (cmd-echo "MKDIR" path)
  (make-directories path))


(define-syntax-rule
  (get-or-make-directory value)
  (memconst
    (intermezzo
      result
      (unless
        (file-or-directory-exists? result)
        (#{make-directory#transitive}# result))
      value)))


(define-syntax-rule
  (define-directory name value)
  (define name (get-or-make-directory value)))


(define czempak-root-override-variable
  "CZEMPAK_ROOT")


(define czempak-root-override
  (getenv czempak-root-override-variable))


(define #{czempak-root-prefix#value}#
  (or czempak-root-override
      (append-posix-path
        (getenv "HOME")
        ".local"
        "share"
        "czempak")))


(define-directory
  get-czempak-root-prefix
  #{czempak-root-prefix#value}#)


(define #{package-store-directory#value}#
  (append-posix-path
    #{czempak-root-prefix#value}#
    "store"))


(define-directory
  get-package-store-directory
  #{package-store-directory#value}#)


(define #{import-root-prefix#value}#
  (append-posix-path
    #{package-store-directory#value}#
    "bin"))


(define-directory
  get-import-root-prefix
  #{import-root-prefix#value}#)


(define #{package-root-directory#value}#
  (append-posix-path
    #{package-store-directory#value}#
    "src"))


(define-directory
  get-package-root-directory
  #{package-root-directory#value}#)


(define #{pots-root#value}#
  (append-posix-path
    #{czempak-root-prefix#value}#
    "pots"))


(define-directory
  get-pots-root
  #{pots-root#value}#)


(define #{czempak-temp-directory#value}#
  (append-posix-path
    #{czempak-root-prefix#value}#
    "temp"))


(define-directory
  get-czempak-temp-directory
  #{czempak-temp-directory#value}#)


(define #{czempak-temp-directory#value#prefixed}#
  (string-append
    #{czempak-temp-directory#value}#
    "/"))


(define (package-location-from-hash hash)
  (append-posix-path
    (get-package-root-directory)
    hash))


(define cache-signaturefile-path ".czempak-link")


(define get-current-link
  (memconst
    (catch-any
      (lambda _
        (read-string-file cache-signaturefile-path))
      (lambda _
        (let ((link (~a (big-random-int 9999999999999999))))
          (write-string-file cache-signaturefile-path link)
          link)))))


(define czempak-verbosity
  (string->number
    (or (getenv "CZEMPAK_VERBOSITY") "10")))


(define (message . args)
  (apply cmd-echo-f (cons "INFO" args)))


(define (failwith . args)
  (apply throw (cons 'fail args)))


(define (errorwith . args)
  (apply throw (cons 'error args)))


(define-syntax-rule
  (czempak-top-level-guard . bodies)
  (let ((error #f))
    (catch 'fail
           (lambda _
             (catch 'error
                    (lambda _ . bodies)
                    (lambda (key . args)
                      (apply cmd-echo-f (cons "ERROR" args))
                      (set! error #t))))
           (lambda (key . args)
             (apply cmd-echo-f (cons "FAIL" args))
             (set! error #t)))
    (if error (exit 1) (exit 0))))


(define cmd-input-stream (make-parameter #f))


(define (cmd program . args)
  (cond ((string? program)
         (let* ((p (with-no-shell-log
                     (apply sh-async (cons program args))))
                (pipe (comprocess-pipe p)))
           (when (cmd-input-stream)
                 (display (cmd-input-stream) pipe)
                 (close-port pipe))
           (sleep-until (comprocess-exited? p))
           (shell-check-status p)))
        ((procedure? program)
         (with-output-to-string
           (lambda ()
             (with-input-from-string
               (or (cmd-input-stream) "")
               (lambda () (program args))))))
        (else
         (throw 'program-is-expected-to-be-either-string-or-procedure
                program))))


(define (cmd-re program . args)
  (string-trim-chars
    (with-output-to-string
      (lambda _ (apply cmd (cons program args))))
    "\n \t"
    'both))


(define cmd-echo-max-len 100)


(define cmd-echo-pad-len 6)


(define cmd-echo-pad
  (case-lambda
    ((str) (cmd-echo-pad str 0))
    ((str diff)
     (string-pad-right
       str
       (+ cmd-echo-pad-len diff)
       #\space))))


(define (#{cmd-echo#raw}# name . args)
  (define (print x)
    (display x (current-error-port)))
  (print (cmd-echo-pad name))
  (let loop ((args args) (count cmd-echo-pad-len))
    (unless
      (null? args)
      (let* ((cur (car args))
             (len (string-length cur))
             (len+1 (+ 1 len))
             (break?
               (and (> (+ count len) cmd-echo-max-len)
                    (> (+ count len) (+ cmd-echo-pad-len len)))))
        (when break? (print (cmd-echo-pad "\n " 1)))
        (print cur)
        (print " ")
        (loop (cdr args)
              (if break?
                (+ cmd-echo-pad-len len+1)
                (+ count len+1))))))
  (print "\n"))


(define cmd-echo-temporary-file-table
  (make-hash-table))


(define cmd-echo-temporary-file-counter 0)


(define (cmd-echo-temporary-get-or-add file)
  (let ((num (let ((o (hash-ref cmd-echo-temporary-file-table file #f)))
               (or o
                   (begin
                     (set! cmd-echo-temporary-file-counter
                       (+ 1 cmd-echo-temporary-file-counter))
                     (hash-set!
                       cmd-echo-temporary-file-table
                       file
                       cmd-echo-temporary-file-counter)
                     cmd-echo-temporary-file-counter)))))
    (string-append "$" (number->string num))))


(define (cmd-echo-force name args-full)
  (define replace-alist
    (filter
      car
      (list (cons #{import-root-prefix#value}# "$BIN")
            (cons #{package-root-directory#value}# "$SRC")
            (cons #{pots-root#value}# "$POTS")
            (cons #{package-store-directory#value}# "$STORE")
            (cons #{czempak-root-prefix#value}# "$ROOT"))))
  (define (simplify p)
    (if (temporary-file? p)
      (cmd-echo-temporary-get-or-add p)
      (replace-words replace-alist (~a p))))
  (define args (map simplify args-full))
  (apply #{cmd-echo#raw}# (cons name args)))


(define (cmd-echo name . args-full)
  (when (> czempak-verbosity 50)
        (cmd-echo-force name args-full)))


(define (cmd-echo-f name . args-full)
  (apply cmd-echo
         (cons name (words (apply stringf args-full)))))


(define (download hash)
  (cmd "czempak-download ~s" hash))


(define (copy-source source-path destination-path)
  (cmd-echo "COPY" source-path destination-path)
  (if (temporary-file? source-path)
    (write-string-file
      destination-path
      (czempak-read-file source-path))
    (copy-file source-path destination-path)))


(define (move-source source-path destination-path)
  (cmd-echo "MOVE" source-path destination-path)
  (if (temporary-file? source-path)
    (write-string-file
      destination-path
      (czempak-read-file source-path))
    (rename-file source-path destination-path)))


(define query-cli-prefix "query=")


(define query-inputs-name 'inputs)


(define query-environment-name 'environment)


(define build-input-stream (make-parameter #f))


(define current-environment
  (make-parameter
    'current-environment-not-initialized))


(define (input-args->string name args)
  (if (null? args)
    ""
    (apply string-append
           (map ~s
                (apply append
                       (map (lambda (i) (list (list name i) '!)) args))))))


(define-syntax-rule
  (with-build-inputs inputs query . bodies)
  (let* ((in0 (input-args->string
                query-environment-name
                (current-environment)))
         (in1 (input-args->string query-inputs-name inputs))
         (in2 (apply string-append (map ~s 'query)))
         (in (string-append in0 in1 in2)))
    (read-list
      (parameterize ((build-input-stream in)) . bodies))))


(define (query-result-find-multiple key)
  (lambda (bresult)
    (apply append
           (map (lambda (e)
                  (filter
                    identity
                    (map (lambda (p) (and (equal? (car p) key) (cdr p)))
                         e)))
                (filter pair? bresult)))))


(define (query-result-find-single key)
  (let ((mult (query-result-find-multiple key)))
    (lambda (bresult)
      (let ((x (mult bresult)))
        (cond ((null? x) #f)
              ((null? (cdr x)) (car x))
              (else
               (throw 'multiple-values-in-find-single
                      key
                      bresult)))))))


(define build-result-final-hash
  (query-result-find-single 'final-hash))


(define build-result-hash
  (query-result-find-single 'hash))


(define build-result-path
  (query-result-find-single 'path))


(define-rec sourcefile path %hash %content)


(define (make-sourcefile path)
  (or (hash-ref %sourcefile-cache path #f)
      (intermezzo
        result
        (hash-set! %sourcefile-cache path result)
        (sourcefile path #f #f))))


(define (make-sourcefile-from-hash hash)
  (make-sourcefile
    (package-location-from-hash hash)))


(define %sourcefile-cache (make-hash-table))


(define interpreter-run-keyword-prefix "%")


(define interpreter-run-keyword-str "run")


(define interpreter-run-keyword
  (string-append
    interpreter-run-keyword-prefix
    interpreter-run-keyword-str))


(define interpreter-run-keyword-length
  (string-length interpreter-run-keyword))


(define (get-port-interpreters file)
  (let ((result
          (let loop ()
            (let ((line (read-line file)))
              (if (eof-object? line)
                (list)
                (if (string-prefix? interpreter-run-keyword line)
                  (let* ((ws (substring line interpreter-run-keyword-length))
                         (i (or (string-index ws #\space) (string-length ws)))
                         (rr (substring ws i))
                         (r (map (curry-if symbol? symbol->string)
                                 (read-list rr))))
                    r)
                  (loop)))))))
    (close-port file)
    result))


(define (get-file-interpreters path)
  (czempak-call-with-input-file
    path
    (lambda (p) (get-port-interpreters p))))


(define (get-content-interpreters content)
  (call-with-input-string
    content
    (lambda (p) (get-port-interpreters p))))


(define fallback-interpreter (make-parameter #f))


(define (get-fallback-interpreter srcfile)
  (or (fallback-interpreter)
      (parameterize
        ((current-output-port (current-error-port)))
        (failwith
          "Do not know which interpreter to use for ~s"
          srcfile))))


(define (get-build-interpreters srcfile)
  (let* ((read-interps (get-file-interpreters srcfile))
         (interps
           (or (and (pair? read-interps) read-interps)
               (list (cons (get-fallback-interpreter srcfile)
                           "fallback")))))
    interps))


(define loaded-interpreters-hashmap
  (make-hash-table))


(define (get-interpreter-executable interp-id)
  (define loaded
    (hash-ref
      loaded-interpreters-hashmap
      interp-id
      #f))
  (or loaded
      (throw 'TODO:support-external-interpreters
             interp-id)))


(define (interpreter-do fn)
  (lambda (srcfile)
    (let* ((interps (get-build-interpreters srcfile))
           (chosen (car interps)))
      ((fn chosen) interps srcfile))))


(define (interpreter-package interp-id)
  (let ((executable
          (get-interpreter-executable interp-id)))
    (lambda (interps srcfile)
      (cmd-echo-f
        "CALL"
        "~a package ~s"
        executable
        srcfile)
      (cmd-re executable "package" srcfile))))


(define (interpreter-build interp-id)
  (let ((executable
          (get-interpreter-executable interp-id)))
    (lambda (interps srcfile)
      (cmd-echo-f
        "CALL"
        "echo '~a' | ~a build ~s"
        (build-input-stream)
        executable
        srcfile)
      (parameterize
        ((cmd-input-stream (build-input-stream)))
        (cmd-re executable "build" srcfile)))))


(define (interpreter-list-dependencies interp-id)
  (let ((executable
          (get-interpreter-executable interp-id)))
    (lambda (interps srcfile)
      (cmd-echo-f
        "CALL"
        "~a list-dependencies ~s"
        executable
        srcfile)
      (cmd executable "list-dependencies" srcfile))))


(define package
  (interpreter-do interpreter-package))


(define build (interpreter-do interpreter-build))


(define list-dependencies
  (interpreter-do interpreter-list-dependencies))










(use-modules (rnrs arithmetic bitwise) (rnrs arithmetic fixnums) (rnrs bytevectors) (rnrs control) (rnrs mutable-strings) (rnrs syntax-case) (srfi srfi-9))

(define (make-hmac
         block-length
         hash
         ->bytevector
         make-hash
         update!
         finish!
         clear!)
  (lambda (secret . data)
    (let lp ((secret secret))
      (if (> (bytevector-length secret) block-length)
        (lp (->bytevector (hash secret)))
        (let ((k-ipad (make-bytevector block-length 0))
              (k-opad (make-bytevector block-length 0)))
          (bytevector-copy!
            secret
            0
            k-ipad
            0
            (bytevector-length secret))
          (bytevector-copy!
            secret
            0
            k-opad
            0
            (bytevector-length secret))
          (do ((i 0 (fx+ i 1)))
              ((fx=? i block-length))
            (bytevector-u8-set!
              k-ipad
              i
              (fxxor 54 (bytevector-u8-ref k-ipad i)))
            (bytevector-u8-set!
              k-opad
              i
              (fxxor 92 (bytevector-u8-ref k-opad i))))
          (let ((state (make-hash)))
            (update! state k-ipad)
            (for-each (lambda (d) (update! state d)) data)
            (finish! state)
            (let ((digest (->bytevector state)))
              (clear! state)
              (update! state k-opad)
              (update! state digest)
              (finish! state)
              state)))))))


(use-modules (rnrs arithmetic bitwise) (rnrs arithmetic fixnums) (rnrs bytevectors) (rnrs control) (rnrs mutable-strings) (rnrs syntax-case) (srfi srfi-9))

(define-syntax define-fx
  (lambda (x)
    (syntax-case x ()
      ((k prefix bit-width op-name fxname bitwise-name)
       (with-syntax ((name (datum->syntax
                             (syntax prefix)
                             (string->symbol
                               (string-append
                                 (symbol->string
                                   (syntax->datum (syntax prefix)))
                                 (symbol->string
                                   (syntax->datum (syntax op-name))))))))
         (syntax
           (define-syntax name
             (identifier-syntax bitwise-name))))))))


(define-syntax define-fixnum-procedures
  (lambda (x)
    (syntax-case x ()
      ((_ prefix bit-width)
       (syntax
         (begin
           (define-fx
             prefix
             bit-width
             and
             fxand
             bitwise-and)
           (define-fx
             prefix
             bit-width
             xor
             fxxor
             bitwise-xor)
           (define-fx
             prefix
             bit-width
             ior
             fxior
             bitwise-ior)
           (define-fx
             prefix
             bit-width
             not
             fxnot
             bitwise-not)
           (define-fx prefix bit-width + fx+ +)
           (define-fx prefix bit-width - fx- -)
           (define-fx prefix bit-width * fx* *)
           (define-fx prefix bit-width =? fx=? =)
           (define-fx
             prefix
             bit-width
             bit-set?
             fxbit-set?
             bitwise-bit-set?)
           (define-fx
             prefix
             bit-width
             arithmetic-shift-right
             fxarithmetic-shift-right
             bitwise-arithmetic-shift-right)
           (define-fx
             prefix
             bit-width
             arithmetic-shift-left
             fxarithmetic-shift-left
             bitwise-arithmetic-shift-left)
           (define-fx prefix bit-width zero? fxzero? zero?)
           (define-fx
             prefix
             bit-width
             bit-field
             fxbit-field
             bitwise-bit-field)
           (define-fx
             prefix
             bit-width
             rotate-bit-field
             fxrotate-bit-field
             bitwise-rotate-bit-field)
           (define-fx
             prefix
             bit-width
             length
             fxlength
             bitwise-length)))))))


(use-modules (rnrs arithmetic bitwise) ((rnrs base) #:select (vector-map)) (rnrs bytevectors) (rnrs control) (rnrs mutable-strings) (rnrs syntax-case) (srfi srfi-9))

(define-fixnum-procedures f32 33)


(define-fixnum-procedures fx 20)


(define (sha-224-length) 28)


(define (sha-256-length) 32)


(define (sha-384-length) 48)


(define (sha-512-length) 64)


(define (vector-copy x)
  (vector-map (lambda (i) i) x))


(define-record-type
  sha-state
  (make-sha-state H init W m pending processed)
  sha-state?
  (H sha-state-H)
  (init sha-state-init)
  (W sha-state-W)
  (m sha-state-m)
  (pending
    sha-state-pending
    sha-state-pending-set!)
  (processed
    sha-state-processed
    sha-state-processed-set!))


(define (make-sha-2 initial-hash)
  (let ((W (make-vector 80 #f))
        (m (make-bytevector (* 4 32))))
    (make-sha-state
      (list->vector initial-hash)
      initial-hash
      W
      m
      0
      0)))


(define (make-sha-224)
  (make-sha-2 initial-hash224))


(define (make-sha-256)
  (make-sha-2 initial-hash256))


(define (make-sha-384)
  (make-sha-2 initial-hash384))


(define (make-sha-512)
  (make-sha-2 initial-hash512))


(define (sha-2-copy state)
  (let ((H (vector-copy (sha-state-H state)))
        (W (make-vector 80 #f))
        (m (bytevector-copy (sha-state-m state))))
    (make-sha-state
      H
      (sha-state-init state)
      W
      m
      (sha-state-pending state)
      (sha-state-processed state))))


(define (sha-224-copy x) (sha-2-copy x))


(define (sha-256-copy x) (sha-2-copy x))


(define (sha-384-copy x) (sha-2-copy x))


(define (sha-512-copy x) (sha-2-copy x))


(define (sha-2-clear! state)
  (do ((init (sha-state-init state) (cdr init))
       (i 0 (+ i 1)))
      ((null? init))
    (vector-set! (sha-state-H state) i (car init)))
  (vector-fill! (sha-state-W state) #f)
  (bytevector-fill! (sha-state-m state) 0)
  (sha-state-pending-set! state 0)
  (sha-state-processed-set! state 0))


(define (sha-224-clear! state)
  (sha-2-clear! state))


(define (sha-256-clear! state)
  (sha-2-clear! state))


(define (sha-384-clear! state)
  (sha-2-clear! state))


(define (sha-512-clear! state)
  (sha-2-clear! state))


(define initial-hash224
  '(3238371032
    914150663
    812702999
    4144912697
    4290775857
    1750603025
    1694076839
    3204075428))


(define initial-hash256
  '(1779033703
    3144134277
    1013904242
    2773480762
    1359893119
    2600822924
    528734635
    1541459225))


(define initial-hash384
  '(14680500436340154072
    7105036623409894663
    10473403895298186519
    1526699215303891257
    7436329637833083697
    10282925794625328401
    15784041429090275239
    5167115440072839076))


(define initial-hash512
  '(7640891576956012808
    13503953896175478587
    4354685564936845355
    11912009170470909681
    5840696475078001361
    11170449401992604703
    2270897969802886507
    6620516959819538809))


(define k-256
  '#(1116352408
     1899447441
     3049323471
     3921009573
     961987163
     1508970993
     2453635748
     2870763221
     3624381080
     310598401
     607225278
     1426881987
     1925078388
     2162078206
     2614888103
     3248222580
     3835390401
     4022224774
     264347078
     604807628
     770255983
     1249150122
     1555081692
     1996064986
     2554220882
     2821834349
     2952996808
     3210313671
     3336571891
     3584528711
     113926993
     338241895
     666307205
     773529912
     1294757372
     1396182291
     1695183700
     1986661051
     2177026350
     2456956037
     2730485921
     2820302411
     3259730800
     3345764771
     3516065817
     3600352804
     4094571909
     275423344
     430227734
     506948616
     659060556
     883997877
     958139571
     1322822218
     1537002063
     1747873779
     1955562222
     2024104815
     2227730452
     2361852424
     2428436474
     2756734187
     3204031479
     3329325298))


(define k-512
  '#(4794697086780616226
     8158064640168781261
     13096744586834688815
     16840607885511220156
     4131703408338449720
     6480981068601479193
     10538285296894168987
     12329834152419229976
     15566598209576043074
     1334009975649890238
     2608012711638119052
     6128411473006802146
     8268148722764581231
     9286055187155687089
     11230858885718282805
     13951009754708518548
     16472876342353939154
     17275323862435702243
     1135362057144423861
     2597628984639134821
     3308224258029322869
     5365058923640841347
     6679025012923562964
     8573033837759648693
     10970295158949994411
     12119686244451234320
     12683024718118986047
     13788192230050041572
     14330467153632333762
     15395433587784984357
     489312712824947311
     1452737877330783856
     2861767655752347644
     3322285676063803686
     5560940570517711597
     5996557281743188959
     7280758554555802590
     8532644243296465576
     9350256976987008742
     10552545826968843579
     11727347734174303076
     12113106623233404929
     14000437183269869457
     14369950271660146224
     15101387698204529176
     15463397548674623760
     17586052441742319658
     1182934255886127544
     1847814050463011016
     2177327727835720531
     2830643537854262169
     3796741975233480872
     4115178125766777443
     5681478168544905931
     6601373596472566643
     7507060721942968483
     8399075790359081724
     8693463985226723168
     9568029438360202098
     10144078919501101548
     10430055236837252648
     11840083180663258601
     13761210420658862357
     14299343276471374635
     14566680578165727644
     15097957966210449927
     16922976911328602910
     17689382322260857208
     500013540394364858
     748580250866718886
     1242879168328830382
     1977374033974150939
     2944078676154940804
     3659926193048069267
     4368137639120453308
     4836135668995329356
     5532061633213252278
     6448918945643986474
     6902733635092675308
     7801388544844847127))


(define (sha-256-transform! H* W m offset)
  (define (ror32 n count)
    (f32ior
      (f32arithmetic-shift-left
        (f32bit-field n 0 count)
        (f32- 32 count))
      (f32arithmetic-shift-right n count)))
  (define (Ch x y z)
    (f32xor (f32and x y) (f32and (f32not x) z)))
  (define (Maj x y z)
    (f32xor (f32and x y) (f32and x z) (f32and y z)))
  (define (Sigma0 x)
    (f32xor (ror32 x 2) (ror32 x 13) (ror32 x 22)))
  (define (Sigma1 x)
    (f32xor (ror32 x 6) (ror32 x 11) (ror32 x 25)))
  (define (sigma0 x)
    (f32xor
      (ror32 x 7)
      (ror32 x 18)
      (f32arithmetic-shift-right x 3)))
  (define (sigma1 x)
    (f32xor
      (ror32 x 17)
      (ror32 x 19)
      (f32arithmetic-shift-right x 10)))
  (do ((t 0 (f32+ t 1)))
      ((eqv? t 16))
    (vector-set!
      W
      t
      (bytevector-u32-ref
        m
        (f32+ (f32* t 4) offset)
        (endianness big))))
  (do ((t 16 (f32+ t 1)))
      ((eqv? t 64))
    (vector-set!
      W
      t
      (f32and
        (f32+ (f32+ (sigma1 (vector-ref W (f32- t 2)))
                    (vector-ref W (f32- t 7)))
              (f32+ (sigma0 (vector-ref W (f32- t 15)))
                    (vector-ref W (f32- t 16))))
        4294967295)))
  (let lp ((A (vector-ref H* 0))
           (B (vector-ref H* 1))
           (C (vector-ref H* 2))
           (D (vector-ref H* 3))
           (E (vector-ref H* 4))
           (F (vector-ref H* 5))
           (G (vector-ref H* 6))
           (H (vector-ref H* 7))
           (t 0))
    (cond ((eqv? t 64)
           (vector-set!
             H*
             0
             (f32and 4294967295 (+ A (vector-ref H* 0))))
           (vector-set!
             H*
             1
             (f32and 4294967295 (+ B (vector-ref H* 1))))
           (vector-set!
             H*
             2
             (f32and 4294967295 (+ C (vector-ref H* 2))))
           (vector-set!
             H*
             3
             (f32and 4294967295 (+ D (vector-ref H* 3))))
           (vector-set!
             H*
             4
             (f32and 4294967295 (+ E (vector-ref H* 4))))
           (vector-set!
             H*
             5
             (f32and 4294967295 (+ F (vector-ref H* 5))))
           (vector-set!
             H*
             6
             (f32and 4294967295 (+ G (vector-ref H* 6))))
           (vector-set!
             H*
             7
             (f32and 4294967295 (+ H (vector-ref H* 7)))))
          (else
           (let ((T1 (f32+ (f32+ H (f32+ (Sigma1 E) (Ch E F G)))
                           (f32+ (vector-ref k-256 t) (vector-ref W t))))
                 (T2 (f32+ (Sigma0 A) (Maj A B C))))
             (lp (f32and 4294967295 (f32+ T1 T2))
                 A
                 B
                 C
                 (f32and 4294967295 (f32+ D T1))
                 E
                 F
                 G
                 (f32+ t 1)))))))


(define (sha-512-transform! H* W m offset)
  (define (ror64 n count)
    (bitwise-ior
      (bitwise-arithmetic-shift-left
        (bitwise-bit-field n 0 count)
        (fx- 64 count))
      (bitwise-arithmetic-shift-right n count)))
  (define (Ch x y z)
    (bitwise-xor
      (bitwise-and x y)
      (bitwise-and (bitwise-not x) z)))
  (define (Maj x y z)
    (bitwise-xor
      (bitwise-and x y)
      (bitwise-and x z)
      (bitwise-and y z)))
  (define (Sigma0 x)
    (bitwise-xor
      (ror64 x 28)
      (ror64 x 34)
      (ror64 x 39)))
  (define (Sigma1 x)
    (bitwise-xor
      (ror64 x 14)
      (ror64 x 18)
      (ror64 x 41)))
  (define (sigma0 x)
    (bitwise-xor
      (ror64 x 1)
      (ror64 x 8)
      (bitwise-arithmetic-shift-right x 7)))
  (define (sigma1 x)
    (bitwise-xor
      (ror64 x 19)
      (ror64 x 61)
      (bitwise-arithmetic-shift-right x 6)))
  (do ((t 0 (fx+ t 1)))
      ((eqv? t 16))
    (vector-set!
      W
      t
      (bytevector-u64-ref
        m
        (fx+ (fx* t 8) offset)
        (endianness big))))
  (do ((t 16 (fx+ t 1)))
      ((eqv? t 80))
    (vector-set!
      W
      t
      (bitwise-and
        (+ (+ (sigma1 (vector-ref W (fx- t 2)))
              (vector-ref W (fx- t 7)))
           (+ (sigma0 (vector-ref W (fx- t 15)))
              (vector-ref W (fx- t 16))))
        18446744073709551615)))
  (let lp ((A (vector-ref H* 0))
           (B (vector-ref H* 1))
           (C (vector-ref H* 2))
           (D (vector-ref H* 3))
           (E (vector-ref H* 4))
           (F (vector-ref H* 5))
           (G (vector-ref H* 6))
           (H (vector-ref H* 7))
           (t 0))
    (cond ((eqv? t 80)
           (vector-set!
             H*
             0
             (bitwise-and
               18446744073709551615
               (+ A (vector-ref H* 0))))
           (vector-set!
             H*
             1
             (bitwise-and
               18446744073709551615
               (+ B (vector-ref H* 1))))
           (vector-set!
             H*
             2
             (bitwise-and
               18446744073709551615
               (+ C (vector-ref H* 2))))
           (vector-set!
             H*
             3
             (bitwise-and
               18446744073709551615
               (+ D (vector-ref H* 3))))
           (vector-set!
             H*
             4
             (bitwise-and
               18446744073709551615
               (+ E (vector-ref H* 4))))
           (vector-set!
             H*
             5
             (bitwise-and
               18446744073709551615
               (+ F (vector-ref H* 5))))
           (vector-set!
             H*
             6
             (bitwise-and
               18446744073709551615
               (+ G (vector-ref H* 6))))
           (vector-set!
             H*
             7
             (bitwise-and
               18446744073709551615
               (+ H (vector-ref H* 7)))))
          (else
           (let ((T1 (+ (+ H (+ (Sigma1 E) (Ch E F G)))
                        (+ (vector-ref k-512 t) (vector-ref W t))))
                 (T2 (+ (Sigma0 A) (Maj A B C))))
             (lp (bitwise-and 18446744073709551615 (+ T1 T2))
                 A
                 B
                 C
                 (bitwise-and 18446744073709551615 (+ D T1))
                 E
                 F
                 G
                 (fx+ t 1)))))))


(define (sha-224-update! . x)
  (apply sha-256-update! x))


(define sha-256-update!
  (case-lambda
    ((state data start end)
     (let ((m (sha-state-m state))
           (H (sha-state-H state))
           (W (sha-state-W state)))
       (let lp ((offset start))
         (cond ((eqv? (sha-state-pending state) 64)
                (sha-256-transform! H W m 0)
                (sha-state-pending-set! state 0)
                (sha-state-processed-set!
                  state
                  (+ 64 (sha-state-processed state)))
                (lp offset))
               ((= offset end) (values))
               ((or (> (sha-state-pending state) 0)
                    (> (+ offset 64) end))
                (let ((added (min (- 64 (sha-state-pending state))
                                  (- end offset))))
                  (bytevector-copy!
                    data
                    offset
                    m
                    (sha-state-pending state)
                    added)
                  (sha-state-pending-set!
                    state
                    (+ added (sha-state-pending state)))
                  (lp (+ offset added))))
               (else
                (sha-256-transform! H W data offset)
                (sha-state-processed-set!
                  state
                  (+ 64 (sha-state-processed state)))
                (lp (+ offset 64)))))))
    ((state data)
     (sha-256-update!
       state
       data
       0
       (bytevector-length data)))))


(define (sha-384-update! . x)
  (apply sha-512-update! x))


(define sha-512-update!
  (case-lambda
    ((state data start end)
     (let ((m (sha-state-m state))
           (H (sha-state-H state))
           (W (sha-state-W state)))
       (let lp ((offset start))
         (cond ((= (sha-state-pending state) 128)
                (sha-512-transform! H W m 0)
                (sha-state-pending-set! state 0)
                (sha-state-processed-set!
                  state
                  (+ 128 (sha-state-processed state)))
                (lp offset))
               ((= offset end) (values))
               ((or (> (sha-state-pending state) 0)
                    (> (+ offset 128) end))
                (let ((added (min (- 128 (sha-state-pending state))
                                  (- end offset))))
                  (bytevector-copy!
                    data
                    offset
                    m
                    (sha-state-pending state)
                    added)
                  (sha-state-pending-set!
                    state
                    (+ added (sha-state-pending state)))
                  (lp (+ offset added))))
               (else
                (sha-512-transform! H W data offset)
                (sha-state-processed-set!
                  state
                  (+ 128 (sha-state-processed state)))
                (lp (+ offset 128)))))))
    ((state data)
     (sha-512-update!
       state
       data
       0
       (bytevector-length data)))))


(define zero-block (make-bytevector 128 0))


(define (sha-224-finish! state)
  (sha-256-finish! state))


(define (sha-256-finish! state)
  (let ((m (sha-state-m state))
        (pending (+ (sha-state-pending state) 1)))
    (bytevector-u8-set!
      m
      (sha-state-pending state)
      128)
    (cond ((> pending 56)
           (bytevector-copy!
             zero-block
             0
             m
             pending
             (- 64 pending))
           (sha-256-transform!
             (sha-state-H state)
             (sha-state-W state)
             m
             0)
           (bytevector-fill! m 0))
          (else
           (bytevector-copy!
             zero-block
             0
             m
             pending
             (- 64 pending))))
    (bytevector-u64-set!
      m
      56
      (* (+ (sha-state-processed state) (- pending 1))
         8)
      (endianness big))
    (sha-256-transform!
      (sha-state-H state)
      (sha-state-W state)
      m
      0)))


(define (sha-384-finish! state)
  (sha-512-finish! state))


(define (sha-512-finish! state)
  (let ((m (sha-state-m state))
        (pending (+ (sha-state-pending state) 1)))
    (bytevector-u8-set!
      m
      (sha-state-pending state)
      128)
    (cond ((> pending 112)
           (bytevector-copy!
             zero-block
             0
             m
             pending
             (- 128 pending))
           (sha-512-transform!
             (sha-state-H state)
             (sha-state-W state)
             m
             0)
           (bytevector-fill! m 0))
          (else
           (bytevector-copy!
             zero-block
             0
             m
             pending
             (- 128 pending))))
    (bytevector-uint-set!
      m
      112
      (* (+ (sha-state-processed state) (- pending 1))
         8)
      (endianness big)
      16)
    (sha-512-transform!
      (sha-state-H state)
      (sha-state-W state)
      m
      0)))


(define (sha-2-finish copy finish!)
  (lambda (state)
    (let ((copy (copy state))) (finish! copy) copy)))


(define sha-224-finish
  (sha-2-finish sha-224-copy sha-224-finish!))


(define sha-256-finish
  (sha-2-finish sha-256-copy sha-256-finish!))


(define sha-384-finish
  (sha-2-finish sha-384-copy sha-384-finish!))


(define sha-512-finish
  (sha-2-finish sha-512-copy sha-512-finish!))


(define (sha-2 make update! finish!)
  (lambda data
    (let ((state (make)))
      (for-each (lambda (d) (update! state d)) data)
      (finish! state)
      state)))


(define sha-224
  (sha-2 make-sha-224
         sha-224-update!
         sha-224-finish!))


(define sha-256
  (sha-2 make-sha-256
         sha-256-update!
         sha-256-finish!))


(define sha-384
  (sha-2 make-sha-384
         sha-384-update!
         sha-384-finish!))


(define sha-512
  (sha-2 make-sha-512
         sha-512-update!
         sha-512-finish!))


(define (sha-2/32-copy-hash! len)
  (lambda (state bv off)
    (do ((i 0 (+ i 1)))
        ((= i len))
      (bytevector-u32-set!
        bv
        (+ off (* 4 i))
        (vector-ref (sha-state-H state) i)
        (endianness big)))))


(define sha-224-copy-hash!
  (sha-2/32-copy-hash! 7))


(define sha-256-copy-hash!
  (sha-2/32-copy-hash! 8))


(define sha-224-128-copy-hash!
  (sha-2/32-copy-hash! 4))


(define sha-256-128-copy-hash!
  (sha-2/32-copy-hash! 4))


(define (sha-2/64-copy-hash! len)
  (lambda (state bv off)
    (do ((i 0 (+ i 1)))
        ((= i len))
      (bytevector-u64-set!
        bv
        (+ off (* 8 i))
        (vector-ref (sha-state-H state) i)
        (endianness big)))))


(define sha-384-copy-hash!
  (sha-2/64-copy-hash! 6))


(define sha-512-copy-hash!
  (sha-2/64-copy-hash! 8))


(define sha-384-128-copy-hash!
  (sha-2/64-copy-hash! 2))


(define sha-512-128-copy-hash!
  (sha-2/64-copy-hash! 2))


(define (sha-2->bytevector copy! len)
  (lambda (state)
    (let ((ret (make-bytevector (* 4 len))))
      (copy! state ret 0)
      ret)))


(define sha-224->bytevector
  (sha-2->bytevector sha-224-copy-hash! 7))


(define sha-256->bytevector
  (sha-2->bytevector sha-256-copy-hash! 8))


(define sha-384->bytevector
  (sha-2->bytevector sha-384-copy-hash! 12))


(define sha-512->bytevector
  (sha-2->bytevector sha-512-copy-hash! 16))


(define (make-sha-2/32->string len)
  (lambda (state)
    (define hex "0123456789abcdef")
    (do ((ret (make-string len))
         (H (sha-state-H state))
         (i 0 (fx+ i 1)))
        ((fx=? i len) ret)
      (let ((n (bitwise-and
                 (bitwise-arithmetic-shift-right
                   (vector-ref H (fxarithmetic-shift-right i 3))
                   (fx- 28 (fx* 4 (fxand i 7))))
                 15)))
        (string-set! ret i (string-ref hex n))))))


(define (make-sha-2/64->string len)
  (lambda (state)
    (define hex "0123456789abcdef")
    (do ((ret (make-string len))
         (H (sha-state-H state))
         (i 0 (fx+ i 1)))
        ((fx=? i len) ret)
      (let ((n (bitwise-and
                 (bitwise-arithmetic-shift-right
                   (vector-ref H (fxarithmetic-shift-right i 4))
                   (fx- 60 (fx* 4 (fxand i 15))))
                 15)))
        (string-set! ret i (string-ref hex n))))))


(define sha-224->string
  (make-sha-2/32->string 56))


(define sha-256->string
  (make-sha-2/32->string 64))


(define sha-384->string
  (make-sha-2/64->string 96))


(define sha-512->string
  (make-sha-2/64->string 128))


(define (cmp/32 state bv len)
  (do ((i 0 (fx+ i 1))
       (diff 0
             (+ diff
                (bitwise-xor
                  (bytevector-u32-ref bv (* 4 i) (endianness big))
                  (vector-ref (sha-state-H state) i)))))
      ((fx=? i len) (zero? diff))))


(define (sha-224-hash=? state bv)
  (cmp/32 state bv 7))


(define (sha-256-hash=? state bv)
  (cmp/32 state bv 8))


(define (sha-384-hash=? state bv)
  (cmp/64 state bv 6))


(define (sha-512-hash=? state bv)
  (cmp/64 state bv 8))


(define (cmp/64 state bv len)
  (do ((i 0 (fx+ i 1))
       (diff 0
             (+ diff
                (bitwise-xor
                  (bytevector-u64-ref bv (* 8 i) (endianness big))
                  (vector-ref (sha-state-H state) i)))))
      ((fx=? i len) (zero? diff))))


(define (sha-224-128-hash=? state bv)
  (cmp/32 state bv 4))


(define (sha-256-128-hash=? state bv)
  (cmp/32 state bv 4))


(define (sha-384-128-hash=? state bv)
  (cmp/64 state bv 2))


(define (sha-512-128-hash=? state bv)
  (cmp/64 state bv 2))


(define hmac-sha-224
  (make-hmac
    64
    sha-224
    sha-224->bytevector
    make-sha-224
    sha-224-update!
    sha-224-finish!
    sha-224-clear!))


(define hmac-sha-256
  (make-hmac
    64
    sha-256
    sha-256->bytevector
    make-sha-256
    sha-256-update!
    sha-256-finish!
    sha-256-clear!))


(define hmac-sha-384
  (make-hmac
    128
    sha-384
    sha-384->bytevector
    make-sha-384
    sha-384-update!
    sha-384-finish!
    sha-384-clear!))


(define hmac-sha-512
  (make-hmac
    128
    sha-512
    sha-512->bytevector
    make-sha-512
    sha-512-update!
    sha-512-finish!
    sha-512-clear!))




(use-modules (rnrs bytevectors))





(use-modules (srfi srfi-9))

(use-modules (ice-9 hash-table))

(define (hash-table-copy H)
  (define R (make-hash-table))
  (hash-for-each
    (lambda (key value) (hash-set! R key value))
    H)
  R)


(define (hash-table-to-alist H)
  (hash-map->list cons H))


(define-record-type
  <database>
  (database a b)
  database?
  (a database-table)
  (b database-handler))


(define-record-type
  <rule>
  (rule a b c d)
  rule?
  (a rule-name)
  (b rule-index)
  (c rule-args)
  (d rule-body))


(define-record-type
  <usymbol>
  (usymbol a b)
  usymbol?
  (a usymbol-name)
  (b usymbol-qualifier))


(define-record-type
  <instruction>
  (instruction a b c d e)
  instruction?
  (a instruction-sign)
  (b instruction-args)
  (c instruction-arity)
  (d instruction-next)
  (e instruction-context))


(define-record-type
  <state>
  (state a b c d)
  state?
  (a state-current)
  (b state-stack)
  (c state-env)
  (d state-failstate))


(define (make-state start-instruction)
  (state start-instruction (list) (make-env) #f))


(define (state-final? s)
  (not (and s (state-current s))))


(define (state-finish s)
  (state #f
         (state-stack s)
         (state-env s)
         (state-failstate s)))


(define (tiny-logic/list-ref-or lst ref default)
  (let lp ((lst lst) (ref ref))
    (if (null? lst)
      default
      (if (= 0 ref) (car lst) (lp (cdr lst) (- ref 1))))))


(define (make-database botom-handler)
  (database (make-hash-table) botom-handler))


(define (database-handle db key arity)
  (let ((function ((database-handler db) key arity)))
    (and function (rule key 0 (list) function))))


(define (double-hash-table-ref H key1 key2)
  (define h (hash-ref H key1 #f))
  (and h (hash-ref h key2 #f)))


(define (double-hash-table-set! H key1 key2 value)
  (define h0 (hash-ref H key1 #f))
  (define h
    (or h0
        (begin
          (let ((h (make-hash-table)))
            (hash-set! H key1 h)
            h))))
  (hash-set! h key2 value))


(define (database-get db k arity)
  (define (get db key index arity)
    (let ((r (double-hash-table-ref
               (database-table db)
               key
               arity)))
      (and r (tiny-logic/list-ref-or r index #f))))
  (if (pair? k)
    (get db (car k) (cdr k) arity)
    (get db k 0 arity)))


(define (database-set! db name args body)
  (let* ((arity (length args))
         (existing
           (or (double-hash-table-ref
                 (database-table db)
                 name
                 arity)
               '()))
         (index (length existing))
         (value (rule name index args body)))
    (double-hash-table-set!
      (database-table db)
      name
      arity
      (append existing (list value)))))


(define (make-env) (make-hash-table))


(define (env-get env key)
  (if (or (symbol? key) (usymbol? key))
    (hash-ref env key #f)
    key))


(define (env-set env key value)
  (let ((copy (hash-table-copy env)))
    (hash-set! copy key value)
    copy))


(define (get-alternative-instruction db s)
  (define inst (state-current s))
  (cond ((instruction-context inst) inst)
        ((procedure? (instruction-sign inst)) #f)
        (else
         (let* ((sign (instruction-sign inst))
                (arity (instruction-arity inst))
                (get-from-pair
                  (lambda (p a)
                    (let* ((name (car p))
                           (ver (cdr p))
                           (new (cons name (+ ver 1)))
                           (get (database-get db new a)))
                      get)))
                (rule (cond ((pair? sign) (get-from-pair sign arity))
                            (else (get-from-pair (cons sign 0) arity)))))
           (and rule
                (instruction
                  (cons (rule-name rule) (rule-index rule))
                  (instruction-args inst)
                  (instruction-arity inst)
                  (instruction-next inst)
                  (instruction-context inst)))))))


(define (varname? obj)
  (or (symbol? obj) (usymbol? obj)))


(define (make-unique-varname symb rule)
  (usymbol symb rule))


(define (alpha-reduce rule args)
  (define r-args (rule-args rule))
  (define (repl symb)
    (if (not (varname? symb))
      symb
      (let lp ((rbuf r-args) (abuf args))
        (if (null? rbuf)
          (make-unique-varname symb rule)
          (if (equal? symb (car rbuf))
            (car abuf)
            (lp (cdr rbuf) (cdr abuf)))))))
  (define (app-pair body)
    (map (lambda (x) (cons (car x) (map repl (cdr x))))
         body))
  (app-pair (rule-body rule)))


(define (beta-reduce rule args)
  (build-body (alpha-reduce rule args)))


(define (enter-subroutine s instruction target-rule)
  (define replaced
    (beta-reduce
      target-rule
      (instruction-args instruction)))
  (define new-state
    (state replaced
           (cons instruction (state-stack s))
           (state-env s)
           s))
  new-state)


(define (instruction-set-ctx inst new-ctx)
  (instruction
    (instruction-sign inst)
    (instruction-args inst)
    (instruction-arity inst)
    (instruction-next inst)
    new-ctx))


(define (init-foreign-instruction inst target-rule)
  (instruction
    (rule-body target-rule)
    (instruction-args inst)
    (instruction-arity inst)
    (instruction-next inst)
    #f))


(define (enter-foreign db s instruction)
  (define env (state-env s))
  (define func (instruction-sign instruction))
  (define context
    (instruction-context instruction))
  (define args (instruction-args instruction))
  (define argv
    (map (lambda (a) (env-get env a)) args))
  (define ret-all (func argv context))
  (if (eq? #t ret-all)
    (continue s)
    (let ((ret (and ret-all (car ret-all)))
          (ctx (and ret-all (cdr ret-all))))
      (if (not ret)
        (backtrack db s)
        (continue
          (let* ((m (if (eq? ret #t) '() (map cons args ret)))
                 (new-env
                   (let loop ((e env) (buf m))
                     (if (null? buf)
                       e
                       (let* ((cur (car buf))
                              (key (car cur))
                              (val (cdr cur))
                              (nee (if (eq? #t val) e (env-set e key val))))
                         (loop nee (cdr buf))))))
                 (new-failstate
                   (if ctx
                     (construct-from-alt
                       s
                       (instruction-set-ctx instruction ctx))
                     (state-failstate s))))
            (state instruction
                   (state-stack s)
                   new-env
                   new-failstate)))))))


(define (continue s)
  (define current (state-current s))
  (define next (instruction-next current))
  (if next
    (state next
           (state-stack s)
           (state-env s)
           (state-failstate s))
    (if (null? (state-stack s))
      (state-finish s)
      (continue
        (state (car (state-stack s))
               (cdr (state-stack s))
               (state-env s)
               (state-failstate s))))))


(define (apply-instruction db s)
  (define instruction (state-current s))
  (define key (instruction-sign instruction))
  (define arity (instruction-arity instruction))
  (define target-rule (database-get db key arity))
  (if target-rule
    (enter-subroutine s instruction target-rule)
    (if (instruction-context instruction)
      (enter-foreign db s instruction)
      (let ((target-rule (database-handle db key arity)))
        (if target-rule
          (enter-foreign
            db
            s
            (init-foreign-instruction
              instruction
              target-rule))
          (backtrack db s))))))


(define (construct-from-alt s alt)
  (state alt
         (state-stack s)
         (state-env s)
         (state-failstate s)))


(define (backtrack db initial-state)
  (let lp ((s (state-failstate initial-state)))
    (if (not s)
      #f
      (let ((alt (get-alternative-instruction db s)))
        (case alt
          ((#f) (lp (state-failstate s)))
          ((builtin) s)
          (else (construct-from-alt s alt)))))))


(define (eval-state db initial-state)
  (define new-state
    (apply-instruction db initial-state))
  (if (state-final? new-state)
    new-state
    (eval-state db new-state)))


(define (build-body body)
  (define rev (reverse body))
  (define (make-one block next)
    (define sign (car block))
    (define args (cdr block))
    (instruction sign args (length args) next #f))
  (define result
    (let lp ((buf rev) (prev #f))
      (if (null? buf)
        prev
        (let ((current (make-one (car buf) prev)))
          (lp (cdr buf) current)))))
  result)


(define (create-database botom-handler lst-of-rules)
  (define db (make-database botom-handler))
  (define (handle-rule r)
    (define first (car r))
    (define name (car first))
    (define args-init (cdr first))
    (define body-init (cdr r))
    (define (ret args body-app)
      (let ((body (append body-app body-init)))
        (database-set! db name args body)))
    (let lp ((buf args-init)
             (i 0)
             (aret (list))
             (bret-app (list)))
      (if (null? buf)
        (ret (reverse aret) (reverse bret-app))
        (let ((x (car buf)))
          (if (not (varname? x))
            (let ((u (usymbol name `(arg ,i))))
              (lp (cdr buf)
                  (+ i 1)
                  (cons u aret)
                  (cons `(= ,u ,x) bret-app)))
            (lp (cdr buf) (+ i 1) (cons x aret) bret-app))))))
  (for-each handle-rule lst-of-rules)
  db)


(define (eval-query db query)
  (define (backtrack-eval db s)
    (let ((b (backtrack db s)))
      (and b (eval-state db b))))
  (define (take-vars s)
    (hash-table-to-alist (state-env s)))
  (define start-instruction (build-body query))
  (define initial-state
    (make-state start-instruction))
  (define final-state
    (eval-state db initial-state))
  (let lp ((s final-state))
    (if s
      (cons (filter
              (lambda (x) (not (usymbol? (car x))))
              (take-vars s))
            (lp (backtrack-eval db s)))
      (list))))


(define (query-get-free-variables q)
  (filter varname? (apply append (map cdr q))))


(define (and-map f lst)
  (let loop ((buf lst))
    (if (null? buf)
      #t
      (if (f (car buf)) (loop (cdr buf)) #f))))


(define (make-instantiation-check query)
  (let ((free (query-get-free-variables query)))
    (lambda (result)
      (and-map (lambda (var) (assq var result)) free))))






(define-syntax make-handler-helper
  (syntax-rules ()
    ((_ key ex-arity buf ()) (case key . buf))
    ((_ key ex-arity buf ((name op) . rest))
     (make-handler-helper
       key
       ex-arity
       (((name)
         (if (pair? op)
           (and (= (car op) ex-arity) (cdr op))
           op))
        .
        buf)
       rest))))


(define-syntax make-handler
  (syntax-rules ()
    ((_ . cases)
     (lambda (key ex-arity)
       (make-handler-helper
         key
         ex-arity
         ((else #f))
         cases)))))


(define-syntax handler-lambda
  (syntax-rules ()
    ((_ arity args . bodies)
     (cons arity (lambda args . bodies)))))


(define-syntax make-set
  (syntax-rules ()
    ((_ value)
     (let ((lst #f))
       (handler-lambda
         1
         (args ctx)
         (define x (car args))
         (unless lst (set! lst value))
         (if x
           (not (not (member x lst)))
           (let ((ctxx (or ctx lst)))
             (if (null? ctxx)
               #f
               (cons (list (car ctxx)) (cdr ctxx))))))))))


(define (try-assign-multi args lst)
  (let loop ((args args) (lst lst) (ret (list)))
    (if (null? args)
      (reverse ret)
      (if (not (car args))
        (if (not (car lst))
          (loop (cdr args) (cdr lst) (cons #t ret))
          (loop (cdr args) (cdr lst) (cons (car lst) ret)))
        (and (or (equal? #t (car lst))
                 (equal? (car args) (car lst)))
             (loop (cdr args) (cdr lst) (cons #t ret)))))))


(define (assign-multi args lst)
  (let loop ((lst lst))
    (if (null? lst)
      #f
      (let ((try (try-assign-multi args (car lst))))
        (if try (cons try (cdr lst)) (loop (cdr lst)))))))


(define-syntax make-tuple-set
  (syntax-rules ()
    ((_ value)
     (let ((lst #f))
       (lambda (args ctx)
         (define x (car args))
         (let ((ctxx (or ctx
                         (begin (unless lst (set! lst value)) lst))))
           (assign-multi args ctxx)))))))


(define (g-op ind x y z op)
  (define (in-op-domain? x)
    (and (integer? x) (>= x 0)))
  (define (repack ind z)
    (case ind
      ((0) (list z #t #t))
      ((1) (list #t z #t))
      ((2) (list #t #t z))))
  (unless
    (and (in-op-domain? x) (in-op-domain? y))
    (throw 'TODO-6:non-naturals-in-op x y))
  (let ((result (op x y)))
    (and result
         (in-op-domain? result)
         (if z (= z result) (cons (repack ind result) #f)))))


(define (binary-op action left-inverse right-inverse)
  (handler-lambda
    3
    (args ctx)
    (define x (car args))
    (define y (cadr args))
    (define z (car (cdr (cdr args))))
    (cond ((and x y) (g-op 2 x y z action))
          ((and x z) (g-op 1 z x y left-inverse))
          ((and y z) (g-op 0 z y x right-inverse))
          (else (throw 'need-more-info-in-+ args)))))


(define op+ (binary-op + - -))


(define op*
  (let ((safe-div
          (lambda (a b) (and (not (= 0 b)) (/ a b)))))
    (binary-op * safe-div safe-div)))


(define ass-less
  (handler-lambda
    2
    (args ctx)
    (define xv (car args))
    (define yv (cadr args))
    (unless
      (number? yv)
      (throw 'non-number-in-less args))
    (if xv
      (if (number? xv)
        (and (not ctx) (< xv yv) #t)
        (throw 'non-number-in-less args))
      (if (< yv 1)
        #f
        (let* ((ctxx (or ctx yv)) (ctxm (- ctxx 1)))
          (and (>= ctxm 0) (cons (list ctxm #t) ctxm)))))))


(define divisible
  (handler-lambda
    2
    (args ctx)
    (let ((x (cadr args))
          (y (car args))
          (last (or ctx 1)))
      (if x
        (= 0 (remainder y x))
        (and (< last y)
             (let loop ((i last) (cnt 0))
               (if (= 0 (remainder y i))
                 (cons (list y i) (+ i 1))
                 (loop (+ 1 i) cnt))))))))


(define (variable-equal? x y)
  (if x
    (if y (equal? x y) 'y-false)
    (if y 'x-false 'both-false)))


(define separate
  (handler-lambda
    2
    (args ctx)
    (define x (car args))
    (define y (cadr args))
    (case (variable-equal? x y)
      ((#t) #f)
      ((#f) #t)
      ((x-false) #f)
      ((y-false) #f)
      ((both-false)
       (throw 'TODO-4:both-undefined-in-separate args)))))


(define unify
  (handler-lambda
    2
    (args ctx)
    (define x (car args))
    (define y (cadr args))
    (case (variable-equal? x y)
      ((#t) #t)
      ((#f) #f)
      ((x-false) (cons (list y #t) #f))
      ((y-false) (cons (list #t x) #f))
      ((both-false)
       (throw 'TODO-3:both-undefined args)))))










(define (read-sentence)
  (let loop ((buf (list)))
    (let ((word (read)))
      (cond ((eof-object? word) (cons 'eof word))
            ((equal? word '!) (cons 'fact (reverse buf)))
            ((equal? word '?) (cons 'query (reverse buf)))
            (else (loop (cons word buf)))))))


(define (repl-print-simple re)
  (for-each
    (lambda (xs)
      (for-each
        (lambda (x) (write x) (display " "))
        xs)
      (display "\n"))
    re))


(define repl-print
  (case-lambda
    ((handler)
     (repl-print
       handler
       (lambda (re)
         (for-each (lambda (x) (write x) (newline)) re))))
    ((handler print)
     (lambda (rules st)
       (let* ((db (create-database handler rules))
              (re (eval-query db st)))
         (cond ((null? re) (display "no\n"))
               ((null? (car re)) (display "yes\n"))
               (else (print re)))
         #t)))))


(define (repl-loop prompt eval rules)
  (prompt)
  (let loop ((rules rules))
    (let* ((cur (read-sentence))
           (type (car cur))
           (st (cdr cur)))
      (case type
        ((eof) rules)
        ((fact) (loop (cons st rules)))
        ((query)
         (if (eval rules st)
           (begin (prompt) (loop rules))
           rules))))))




(use-modules (ice-9 regex))

(use-modules (ice-9 rdelim))

(use-modules (ice-9 match))

(use-modules (srfi srfi-1))

(use-modules (srfi srfi-11))

(use-modules (ice-9 hash-table))

(define-rec build-struct hash final-hash path)


(define (build-struct->list bresult)
  (list (build-struct-hash bresult)
        (build-struct-final-hash bresult)
        (build-struct-path bresult)))


(define (list->build-struct lst)
  (build-struct (car lst) (cadr lst) (caddr lst)))


(define-rec
  dependency
  type
  path
  variables
  inputs
  original)


(define cache-filename
  (append-posix-path
    (get-czempak-temp-directory)
    (string-append (get-current-link) "-guile-cache")))


(define (unserialize-top-hashtable alist)
  (define (unserialize-map o)
    (cons (car o) (cons #f (cdr o))))
  (alist->hash-table (map unserialize-map alist)))


(define (initialize-cache)
  (if (file-or-directory-exists? cache-filename)
    (let* ((p (open-input-file cache-filename))
           (alist (read p))
           (ret (unserialize-top-hashtable alist)))
      (close-port p)
      ret)
    (make-hash-table)))


(define cache-updated-once? #f)


(define (cache-update! cache serialize p value)
  (set! cache-updated-once? #t)
  (hash-set! cache p (cons serialize value)))


(define (cache-get cache deserialize p)
  (let ((x (hash-ref cache p #f)))
    (and x
         (let ((serialize (car x)) (value (cdr x)))
           (if serialize value (deserialize value))))))


(define cache-table
  (memconst (initialize-cache)))


(define-syntax-rule
  (#{with-cache!#ex}#
    cache0
    skip
    override
    invalidate
    in
    out
    type
    args
    .
    bodies)
  (if skip
    (begin . bodies)
    (let ((key (list type . args)) (cache cache0))
      (if override
        (intermezzo
          result
          (cache-update! cache in key result)
          (begin . bodies))
        (let ((get (cache-get cache out key)))
          (or (and get (not (invalidate get)) get)
              (intermezzo
                result
                (cache-update! cache in key result)
                (begin . bodies))))))))


(define #{%define#cached-global-counter}# 0)


(define-syntax-rule
  (cached-lambda
    args
    bindings
    (cache0 skip override invalidate in out get-key)
    .
    bodies)
  (let ((cache cache0)
        (type #{%define#cached-global-counter}#))
    (set! #{%define#cached-global-counter}#
      (+ 1 #{%define#cached-global-counter}#))
    (lambda args
      (let* bindings
        (#{with-cache!#ex}#
          cache
          skip
          override
          invalidate
          in
          out
          type
          get-key
          (begin . bodies))))))


(define-syntax-rule
  (#{define#cached}# (name . args) . argv)
  (define name (cached-lambda args . argv)))


(define (check-cache-mtime mtime)
  (lambda (p)
    (define saved-mtime (car p))
    (and saved-mtime
         (not (equal? mtime saved-mtime)))))


(define-syntax-rule
  (#{define#file-cached}#
    (name file . rest)
    (in out additional-keys)
    body)
  (define name
    (let ((fn (cached-lambda
                (file . rest)
                ((srcfile
                   (if (string? file) (make-sourcefile file) file))
                 (path (sourcefile-path srcfile))
                 (mtime (and (local-file? path) (get-mtime path))))
                ((cache-table)
                 (temporary-file? path)
                 #f
                 (check-cache-mtime mtime)
                 (lambda (p) (cons (car p) (in (cdr p))))
                 (lambda (p) (cons (car p) (out (cdr p))))
                 (path . additional-keys))
                (cons mtime (body srcfile path mtime)))))
      (lambda args (cdr (apply fn args))))))


(define-syntax-rule
  (#{define#hash-cached}#
    (name file . rest)
    (in out additional-keys)
    body)
  (define name
    (cached-lambda
      (file . rest)
      ((srcfile
         (cond ((string? file) (make-sourcefile file))
               ((iprog-fileinfo? file)
                (iprog-fileinfo-srcfile file))
               (else file)))
       (path (sourcefile-path srcfile))
       (hash (sourcefile-hash srcfile)))
      ((cache-table)
       (temporary-file? path)
       #f
       (const #f)
       in
       out
       (hash . additional-keys))
      (body srcfile))))


(define %local-mtime-table (make-hash-table))


(define (get-mtime path)
  (or (hash-ref %local-mtime-table path #f)
      (letin (srcfile
               (make-iprog-fileinfo (make-sourcefile path)))
             (deps (iprog-fileinfo-dependencies srcfile))
             (local-deps (filter dependency-local? deps))
             (dep-paths (map dependency-path local-deps))
             (do (intermezzo
                  result
                  (hash-set! %local-mtime-table path result)
                  (apply max
                         (cons (file-mtime path)
                               (map get-mtime dep-paths))))))))


(define-syntax-rule
  (with-double-cache!
    cache
    skip
    in
    out
    key1
    args
    key2
    invalidate1
    body1
    body2)
  (if skip
    body2
    (#{with-cache!#ex}#
      (#{with-cache!#ex}#
        cache
        #f
        #f
        invalidate1
        serialize-top-hashtable
        unserialize-top-hashtable
        key1
        args
        body1)
      #f
      #f
      (const #f)
      in
      out
      key2
      ()
      body2)))


(define (serialize-top-hashtable h)
  (hash-map->list
    (lambda (key value)
      (cons key
            (let ((serialize (car value)) (x (cdr value)))
              (if serialize (serialize x) x))))
    h))


(define (cache-save-to-file!)
  (when cache-updated-once?
        (let* ((p (open-output-file cache-filename))
               (alist (serialize-top-hashtable (cache-table))))
          (write alist p)
          (close-port p))))


(define (merge-hash-tables base additional)
  (define (func key value)
    (hash-set! base key value))
  (hash-for-each func additional))


(define (merge-single! v)
  (let* ((key (car v))
         (cache (cache-table))
         (get (cache-get cache identity key)))
    (if (and get (hash-table? get))
      (let ((other (unserialize-top-hashtable (cdr v))))
        (set! cache-updated-once? #t)
        (merge-hash-tables get other))
      (cache-update! cache #f key (cdr v)))))


(define (merge-cache!)
  (let* ((p (open-input-file cache-filename))
         (alist (read p)))
    (for-each merge-single! alist)
    (close-port p)))


(define-syntax-rule
  (with-cache-sync . bodies)
  (begin
    (cache-save-to-file!)
    (intermezzo
      result
      (merge-cache!)
      (begin . bodies))))


(define (sha256 str)
  (sha-256->string (sha-256 (string->utf8 str))))


(define (sha256-file path)
  (sha256 (czempak-read-file path)))


(#{define#file-cached}#
  (#{get-file-hash#ex}# file maybe-forced)
  (identity identity ())
  (lambda (srcfile path mtime)
    (or maybe-forced
        (intermezzo
          result
          (cmd-echo "HASH" path result)
          (sha256-file path)))))


(define (get-file-hash file)
  (#{get-file-hash#ex}# file #f))


(define-rec preprocessor-line command args)


(define preprocessor-prefix
  interpreter-run-keyword-prefix)


(define (pline->string pline)
  (string-append
    preprocessor-prefix
    (~a (preprocessor-line-command pline))
    (preprocessor-line-args pline)))


(define preprocess-map-single
  (let ((prefix-len (string-length preprocessor-prefix)))
    (lambda (line)
      (if (string-prefix? preprocessor-prefix line)
        (letin (ws (substring line prefix-len))
               (i (or (string-index ws #\space) (string-length ws)))
               (f (substring ws 0 i))
               (r (substring ws i))
               (cmd (string->symbol f))
               (do (preprocessor-line cmd r)))
        line))))


(define (preprocess-map-lines lns)
  (map preprocess-map-single lns))


(define-rec preprocessor-for-group cline lines)


(define (pline-of-type? cmd line)
  (and (preprocessor-line? line)
       (eq? cmd (preprocessor-line-command line))))


(define-syntax-rule
  (define-preprocessor-word
    key
    name
    sym-name
    check-name)
  (define-values
    (sym-name name check-name)
    (values
      'key
      (symbol->string 'key)
      (lambda (line) (pline-of-type? 'key line)))))


(define-preprocessor-word
  use
  preprocessor-use-cmd
  preprocessor-use-cmd-sym
  pline-is-use?)


(define-preprocessor-word
  var
  preprocessor-var-cmd
  preprocessor-var-cmd-sym
  pline-is-var?)


(define-preprocessor-word
  for
  preprocessor-for-cmd
  preprocessor-for-cmd-sym
  pline-is-for?)


(define-preprocessor-word
  end
  preprocessor-end-cmd
  preprocessor-end-cmd-sym
  pline-is-end?)


(define-preprocessor-word
  set
  preprocessor-set-cmd
  preprocessor-set-cmd-sym
  pline-is-set?)


(define-preprocessor-word
  run
  preprocessor-run-cmd
  preprocessor-run-cmd-sym
  pline-is-run?)


(define (preprocessor-get-lines content)
  (define lns (lines content))
  (define maped (preprocess-map-lines lns))
  (map (curry-if pline-is-use? use-pline->dependency)
       maped))


(define get-guile-version
  (memconst
    (let* ((str (with-no-shell-log
                  (sh-re "guile -v | grep -o -E -e '[0-9]+(\\.[0-9]+)+'")))
           (sp (#{string-split#simple}# str #\.)))
      (append
        (map string->number sp)
        (map (const 0) (range (- 3 (length sp))))))))


(define preprocessor-handler
  (make-handler
    (= unify)
    (!= separate)
    (+ op+)
    (* op*)
    (< ass-less)
    (divisible divisible)
    (COMPILER (make-set (list "guile")))
    (COMPILER-VERSION
      (cons 3
            (make-tuple-set (list (get-guile-version)))))))


(define (preprocess-plines preset-stmts maped0)
  (define (reverse-for-group! g)
    (set-preprocessor-for-group-lines!
      g
      (reverse (preprocessor-for-group-lines g))))
  (define (group-fors lines-initial)
    (let lp ((lines lines-initial)
             (indent 0)
             (cur #f)
             (ret (list)))
      (if (null? lines)
        (if cur
          (errorwith
            "Unclosed ~a~a directive"
            preprocessor-prefix
            preprocessor-for-cmd)
          (reverse ret))
        (let* ((line (car lines))
               (for? (pline-is-for? line))
               (end? (pline-is-end? line))
               (new-indent
                 (+ indent (cond (for? 1) (end? -1) (else 0)))))
          (when (< new-indent 0)
                (errorwith
                  "~a~a directive without a ~a~a directive at ~s"
                  preprocessor-prefix
                  preprocessor-end-cmd
                  preprocessor-prefix
                  preprocessor-for-cmd
                  (- (length lines-initial) (length lines))))
          (cond ((and for? (= indent 0) (not cur))
                 (lp (cdr lines)
                     new-indent
                     (preprocessor-for-group line (list))
                     ret))
                ((and end? (= indent 1) cur)
                 (reverse-for-group! cur)
                 (lp (cdr lines) new-indent #f (cons cur ret)))
                (cur
                 (set-preprocessor-for-group-lines!
                   cur
                   (cons line (preprocessor-for-group-lines cur)))
                 (lp (cdr lines) new-indent cur ret))
                (else
                 (lp (cdr lines) new-indent cur (cons line ret))))))))
  (define (modify-use-pline dep sets)
    (define (swap x)
      (define (is-it? y) (equal? x (car (car y))))
      (filter is-it? sets))
    (let* ((inputs0 (or (dependency-inputs dep) (list)))
           (inputs
             (apply append
                    (map (curry-if (negate pair?) swap list) inputs0))))
      (unless
        (equal? inputs0 inputs)
        (set-dependency-inputs! dep inputs))
      dep))
  (define (handle-lines current-sets lns)
    (define grouped (group-fors lns))
    (define (for-group->lines sets g)
      (letin (db (create-database preprocessor-handler sets))
             (cline (preprocessor-for-group-cline g))
             (args (preprocessor-line-args cline))
             (query (preprocessor-line->syntax cline))
             (check (make-instantiation-check query))
             (alist-multiple (eval-query db query))
             (lns (preprocessor-for-group-lines g))
             (repl (lambda (alist)
                     (if (check alist)
                       (replace-words-in-lines alist lns)
                       (errorwith
                         "Not every variable instantiated in ~s"
                         query))))
             (new-lines-buk (map repl alist-multiple))
             (new-lines (apply append new-lines-buk))))
    (let lp ((buf grouped) (sets current-sets))
      (if (null? buf)
        (list)
        (let ((cur (car buf)))
          (cond ((pline-is-set? cur)
                 (lp (cdr buf)
                     (append
                       sets
                       (list (preprocessor-line->syntax cur)))))
                ((dependency? cur)
                 (cons (modify-use-pline cur sets)
                       (lp (cdr buf) sets)))
                ((preprocessor-for-group? cur)
                 (let* ((new-lines (for-group->lines sets cur))
                        (handled (handle-lines sets new-lines)))
                   (append handled (lp (cdr buf) sets))))
                (else (cons cur (lp (cdr buf) sets))))))))
  (define (preprocessor-line->syntax pline)
    (let* ((body-s (preprocessor-line-args pline)))
      (read-list body-s)))
  (define (sets->logic-db-syntax sets)
    (map preprocessor-line->syntax sets))
  (define maped
    (map (curry-if pline-is-var? pline->string)
         maped0))
  (define handled0
    (handle-lines preset-stmts maped))
  (define handled
    (map (curry-if string? preprocess-map-single)
         handled0))
  handled)


(define (preprocess-text
         only-strings?
         preset-stmts
         content)
  (define plines (preprocessor-get-lines content))
  (define handled
    (preprocess-plines preset-stmts plines))
  (if only-strings?
    (plines->clear-text handled)
    (plines->text handled)))


(define (preprocess-file
         only-strings?
         preset-stmts
         filepath)
  (let ((content (czempak-read-file filepath)))
    (preprocess-text
      only-strings?
      preset-stmts
      content)))


(define (preprocess args-s target)
  (define args
    (with-input-from-string
      args-s
      (lambda () (read))))
  (define-values
    (ret deps)
    (preprocess-file #f args target))
  (display ret))


(define (plines->text plines)
  (define (pline->line pline)
    (cond ((string? pline) pline)
          ((dependency? pline) (dependency->string pline))
          ((preprocessor-line? pline)
           (pline->string pline))
          (else (throw 'unexpected-type-in-plines->text))))
  (unlines (map pline->line plines)))


(define (plines->clear-text plines)
  (unlines (filter string? plines)))


(define (dependency->list dep)
  (and dep
       (list (dependency-type dep)
             (dependency-path dep)
             (dependency-variables dep)
             (dependency-inputs dep)
             (dependency->list (dependency-original dep)))))


(define (list->dependency lst)
  (and lst
       (dependency
         (list-ref lst 0)
         (list-ref lst 1)
         (list-ref lst 2)
         (list-ref lst 3)
         (list->dependency (list-ref lst 4)))))


(define (dependency->use-pline dep)
  (define args (dependency-inputs dep))
  (define type-path
    (case (dependency-type dep)
      ((global)
       (string-append " \"" (dependency-path dep) "\""))
      ((local) (dependency-path dep))
      (else
       (string-append
         " ("
         (~a (dependency-type dep))
         " \""
         (~a (dependency-path dep))
         "\")"))))
  (preprocessor-line
    preprocessor-use-cmd-sym
    (string-append
      " "
      (~a (map symbol->string (dependency-variables dep)))
      (or (and args (string-append " " (~s args))) "")
      type-path)))


(define (dependency->string dep)
  (string-append
    preprocessor-prefix
    preprocessor-use-cmd
    (preprocessor-line-args
      (dependency->use-pline dep))))


(define (use-pline->dependency use-preprocessor-line)
  (define args-s
    (preprocessor-line-args use-preprocessor-line))
  (define args (read-list args-s))
  (define len (length args))
  (define-values
    (type-path-index args-index)
    (case len
      ((2) (values 1 -1))
      ((3) (values 2 1))
      (else
       (errorwith
         "Expected 2 or 3 arguments in ~s"
         args-s))))
  (define names
    (or (list-ref-or args 0)
        (errorwith
          "Dependency does not have names in ~s"
          args-s)))
  (define type-path
    (or (list-ref-or args type-path-index)
        (errorwith
          "Dependency does not have a path in ~s"
          args-s)))
  (define _
    (unless
      (or (string? type-path)
          (and (list? type-path) (= 2 (length type-path))))
      (errorwith
        "First argument of ~a-pline is not list in ~s"
        preprocessor-use-cmd
        args-s)))
  (define type
    (if (string? type-path)
      (if (string-prefix? "./" type-path)
        'local
        'global)
      (car type-path)))
  (define path
    (if (string? type-path)
      type-path
      (cadr type-path)))
  (define arguments (list-ref-or args args-index))
  (define original #f)
  (dependency type path names arguments original))


(define (dependency-local? dep)
  (eq? 'local (dependency-type dep)))


(define (dependency-global? dep)
  (eq? 'global (dependency-type dep)))


(define (dependency-abstract? dep)
  (and (dependency-inputs dep)
       (not (null? (dependency-inputs dep)))))


(define (dependency-concrete? dep)
  (not (dependency-abstract? dep)))


(define (concrete-global-dependency hash previous)
  (dependency
    'global
    hash
    (dependency-variables previous)
    #f
    previous))


(define (any-global-dependency hash previous)
  (dependency
    'global
    hash
    (dependency-variables previous)
    (dependency-inputs previous)
    previous))


(define (dependency-replace-path dep path-type path)
  (dependency
    path-type
    path
    (dependency-variables dep)
    (dependency-inputs dep)
    dep))


(define (get-initial-dependency dep)
  (let lp ((d dep))
    (if (dependency-original d)
      (lp (dependency-original d))
      d)))


(define (hashes-of-packaged-dependencies pdeps)
  (define (f dep)
    (unless
      (dependency-global? dep)
      (throw 'packaged-dependency-has-to-be-global dep))
    (dependency-path dep))
  (map f pdeps))


(define (get-original-dependencies dependencies)
  (define deps (or dependencies (list)))
  (define (check d)
    (and (dependency-original d)
         (dependency-global? (dependency-original d))))
  (define originals
    (map (curry-if check dependency-original) deps))
  (hashes-of-packaged-dependencies originals))


(define sourcefile-hash
  (cached-record-accessor
    sourcefile-%hash
    (compose get-file-hash sourcefile-path)
    set-sourcefile-%hash!))


(define sourcefile-content
  (cached-record-accessor
    sourcefile-%content
    (compose czempak-read-file sourcefile-path)
    set-sourcefile-%content!))


(define (sourcefile-update! file)
  (set-sourcefile-%content! file #f)
  (set-sourcefile-%hash! file #f))


(define (#{sourcefile-copy!#logical}#
         srcfile
         destination)
  (define path (sourcefile-path srcfile))
  (unless
    (equal? path destination)
    (set-sourcefile-path! srcfile destination)
    (hash-set! %sourcefile-cache destination srcfile)
    (when (sourcefile-%hash srcfile)
          (#{get-file-hash#ex}#
            destination
            (sourcefile-%hash srcfile)))))


(define (#{sourcefile-move!#logical}#
         srcfile
         destination)
  (hash-set!
    %sourcefile-cache
    (sourcefile-path srcfile)
    #f)
  (#{sourcefile-copy!#logical}#
    srcfile
    destination))


(define (dependency->location dep)
  (cond ((dependency-local? dep) (dependency-path dep))
        ((dependency-global? dep)
         (package-location-from-hash
           (dependency-path dep)))
        (else
         (throw 'expected-local-or-global-dependency dep))))


(define (project-file
         command
         source-path
         destination-path)
  (case command
    ((copy)
     (copy-source source-path destination-path))
    ((move)
     (move-source source-path destination-path))
    (else (throw 'bad-command command))))


(define (save-code destination-path command source-path)
  (values
    destination-path
    (intermezzo
      result
      (when result
            (project-file
              command
              source-path
              destination-path))
      (not (file-or-directory-exists? destination-path)))))


(define (save-built-code command hash source-path)
  (let ((destination-path
          (import-location-from-hash hash)))
    (save-code destination-path command source-path)))


(define (save-packaged-code command hash source-path)
  (let ((destination-path
          (package-location-from-hash hash)))
    (save-code destination-path command source-path)))


(define-rec
  iprog-fileinfo
  srcfile
  %plines
  %dependencies)


(define #{%call#memoized-global-table}#
  (make-hash-table))


(define-syntax-rule
  (#{call#memoized}# function0 . args0)
  (let* ((function function0)
         (args (list . args0))
         (key (cons function args))
         (get (hash-get-handle
                #{%call#memoized-global-table}#
                key)))
    (if get
      (cdr get)
      (intermezzo
        result
        (hash-set!
          #{%call#memoized-global-table}#
          key
          result)
        (apply function args)))))


(define-syntax-rule
  (#{define#memoized}# (name . args) . bodies)
  (define name
    (let* ((memory (make-hash-table)) (bad (make-unique)))
      (lambda args
        (let ((get (hash-ref memory (list . args) bad)))
          (if (eq? get bad)
            (intermezzo
              result
              (hash-set! memory (list . args) result)
              (let () . bodies))
            get))))))


(define (localize-path dir path)
  (if (string-prefix? "/" path)
    path
    (simplify-posix-path
      (append-posix-path dir path))))


(define (list-scan function lst)
  (let loop ((lst (cdr lst))
             (acc (car lst))
             (buf (list (car lst))))
    (if (null? lst)
      (reverse buf)
      (let ((x (function acc (car lst))))
        (loop (cdr lst) x (cons x buf))))))


(#{define#memoized}#
  (is-file-a-symlink? path)
  (and (not (equal? "/" path))
       (case (stat:type (stat path))
         ((symlink) #t)
         (else #f))))


(define (get-real-file-location path)
  (define link? (is-file-a-symlink? path))
  (if link? (readlink path) path))


(#{define#memoized}#
  (get-real-file-directory path)
  (dirname (get-real-file-location path)))


(define (localize-pline! parent-path)
  (let* ((dir (get-real-file-directory parent-path)))
    (lambda (pline)
      (if (and (dependency? pline)
               (dependency-local? pline))
        (let* ((path (dependency-path pline))
               (loc (localize-path dir path)))
          (set-dependency-path! pline loc))))))


(define iprog-fileinfo-plines
  (cached-record-accessor
    iprog-fileinfo-%plines
    (lambda (info)
      (letin (srcfile (iprog-fileinfo-srcfile info))
             (content (sourcefile-content srcfile))
             (plines (preprocessor-get-lines content))
             (path (sourcefile-path srcfile))
             (do (for-each (localize-pline! path) plines))
             (do plines)))
    set-iprog-fileinfo-%plines!))


(define iprog-fileinfo-dependencies
  (cached-record-accessor
    iprog-fileinfo-%dependencies
    (comp iprog-fileinfo-plines (filter dependency?))
    set-iprog-fileinfo-%dependencies!))


(define %iprog-fileinfo-cache (make-hash-table))


(define (make-iprog-fileinfo srcfile)
  (define path (sourcefile-path srcfile))
  (or (hash-ref %iprog-fileinfo-cache path #f)
      (intermezzo
        result
        (hash-set! %iprog-fileinfo-cache path result)
        (iprog-fileinfo srcfile #f #f))))


(define (guile-remote-package interp-id)
  (lambda (interps srcfile)
    (with-cache-sync
      ((interpreter-package interp-id) interps srcfile))))


(define (guile-local-package interps srcfile)
  (guile-package srcfile))


(define guile-abstract-package
  (interpreter-do
    (lambda (interp-id)
      (if (equal? interp-id guile-name)
        guile-local-package
        (guile-remote-package interp-id)))))


(define (guile-remote-build interp-id)
  (lambda (interps srcfile)
    (with-cache-sync
      ((interpreter-build interp-id) interps srcfile))))


(define (guile-local-build interps srcfile)
  (with-output-to-string
    (lambda _
      (with-input-from-string
        (build-input-stream)
        (lambda _ (iprog-interp-build srcfile))))))


(define guile-abstract-build
  (interpreter-do
    (lambda (interp-id)
      (if (equal? interp-id guile-name)
        guile-local-build
        (guile-remote-build interp-id)))))


(define (build-dependency dep)
  (define _1 (assert (dependency-global? dep)))
  (define original-hash (dependency-path dep))
  (define package-srcfile
    (make-sourcefile-from-hash original-hash))
  (define path (sourcefile-path package-srcfile))
  (unless
    (file-or-directory-exists? path)
    (download original-hash))
  (let* ((inputs (or (dependency-inputs dep) (list)))
         (bresult
           (with-build-inputs
             inputs
             ((final-hash final-hash) ?)
             (guile-abstract-build path)))
         (hash (build-result-final-hash bresult)))
    (if (equal? hash original-hash)
      #f
      (begin
        (cmd-echo-f "BUILT" "~a ~a" path hash)
        (concrete-global-dependency hash dep)))))


(define (rewrite-plines plines resolved-deps)
  (define H (make-hash-table))
  (define (add-to-hashtable dep)
    (unless
      (dependency-original dep)
      (throw 'cannot-rewrite-dependency-because-forgot-original
             dep))
    (let lp ((d (dependency-original dep)))
      (when d
            (let ((hash (dependency-path d)))
              (hash-set! H hash dep)
              (lp (dependency-original d))))))
  (define _
    (for-each add-to-hashtable resolved-deps))
  (define (rewrite-one d)
    (let* ((hash (dependency-path d))
           (target-pline (hash-ref H hash d)))
      target-pline))
  (map (curry-if dependency? rewrite-one) plines))


(define (guile-globalize-dependency dep)
  (if (dependency-global? dep)
    #f
    (let* ((path (dependency-path dep))
           (hash (guile-abstract-package path)))
      (any-global-dependency hash dep))))


(define (rewrite-fileinfo-dependencies
         info
         new-plines0
         new-deps
         save?
         drop-run?)
  (letin (srcfile (iprog-fileinfo-srcfile info))
         (path (sourcefile-path srcfile))
         (new-plines
           (if drop-run?
             (filter (negate pline-is-run?) new-plines0)
             new-plines0))
         (as-text (plines->text new-plines))
         (output-path (write-to-temporary path as-text))
         (out-src (make-sourcefile output-path))
         (out-info
           (iprog-fileinfo out-src new-plines new-deps))
         (do (when save? (save-package out-info)))
         (do out-info)))


(define (abstract-handle-pak
         dep-transformation
         save?
         drop-run?)
  (lambda (info)
    (define srcfile (iprog-fileinfo-srcfile info))
    (define dependencies
      (iprog-fileinfo-dependencies info))
    (define plines (iprog-fileinfo-plines info))
    (define path (sourcefile-path srcfile))
    (define hash (sourcefile-hash srcfile))
    (let* ((transformed
             (map (lambda (d) (cons (dep-transformation d) d))
                  dependencies))
           (resolved
             (filter identity (map car transformed))))
      (if (null? resolved)
        (begin
          (when save? (save-package info))
          (if drop-run? (remove-run-line info) info))
        (let* ((new-plines (rewrite-plines plines resolved))
               (new-deps
                 (map (curry-if car car cdr) transformed)))
          (rewrite-fileinfo-dependencies
            info
            new-plines
            new-deps
            save?
            drop-run?))))))


(define globalize-pak
  (abstract-handle-pak
    guile-globalize-dependency
    #t
    #f))


(define (preprocess-pak info)
  (define srcfile (iprog-fileinfo-srcfile info))
  (define path (sourcefile-path srcfile))
  (define plines (iprog-fileinfo-plines info))
  (define dependencies
    (iprog-fileinfo-dependencies info))
  (if (not (or-map pline-is-for? plines))
    info
    (letin (do (cmd-echo "CPP" path))
           (preprocessor-inputs
             (append (current-inputs) (current-environment)))
           (new-plines
             (preprocess-plines preprocessor-inputs plines))
           (as-text (plines->text new-plines))
           (new-deps (filter dependency? new-plines))
           (output-path (write-to-temporary path as-text))
           (new-src (make-sourcefile output-path))
           (do (iprog-fileinfo new-src new-plines new-deps)))))


(define build-deps
  (abstract-handle-pak build-dependency #f #t))


(define (remove-run-line info)
  (letin (srcfile (iprog-fileinfo-srcfile info))
         (path (sourcefile-path srcfile))
         (plines (iprog-fileinfo-plines info))
         (no-run (filter (negate pline-is-run?) plines))
         (text (plines->text no-run))
         (built-path (write-to-temporary path text))
         (new-src (make-sourcefile built-path))
         (new-info
           (iprog-fileinfo
             new-src
             no-run
             (iprog-fileinfo-%dependencies info)))))


(define (build-pak initial-hash info)
  (letin (packaged-file (iprog-fileinfo-srcfile info))
         (packaged-path (sourcefile-path packaged-file))
         (do (cmd-echo "BUILD" packaged-path))
         (plines (iprog-fileinfo-plines info))
         (dependencies (iprog-fileinfo-dependencies info))
         (final-hash (sourcefile-hash packaged-file))
         (vars (filter pline-is-var? plines))
         (header
           (get-header final-hash dependencies vars))
         (rest (plines->clear-text plines))
         (built-path
           (write-to-temporary packaged-path header rest))
         ((build-dest rebuild?)
          (save-build dependencies final-hash built-path))
         (do (build-struct initial-hash final-hash build-dest))))


(#{define#hash-cached}#
  (handle-pak info)
  (build-struct->list
    list->build-struct
    ((current-environment) (current-inputs)))
  (lambda _
    (define initial-hash
      (sourcefile-hash (iprog-fileinfo-srcfile info)))
    (appcomp
      info
      preprocess-pak
      build-deps
      (build-pak initial-hash))))


(define (save-build dependencies final-hash path)
  (define command
    (if (temporary-file? path) 'move 'copy))
  (define-values
    (packaged-dest built?)
    (save-built-code command final-hash path))
  (when built?
        (register-bin
          (import-location-from-hash final-hash)
          (map (compose
                 import-location-from-hash
                 dependency-path)
               dependencies)))
  (values packaged-dest built?))


(define (save-package info)
  (define srcfile (iprog-fileinfo-srcfile info))
  (define hash (sourcefile-hash srcfile))
  (define path (sourcefile-path srcfile))
  (define command
    (if (temporary-file? path) 'move 'copy))
  (let-values
    (((packaged-dest packaged?)
      (save-packaged-code command hash path)))
    (if packaged?
      (let ((dependencies (iprog-fileinfo-dependencies info)))
        (cmd-echo
          "SAVE"
          path
          (package-location-from-hash hash))
        (register-package
          hash
          (get-original-dependencies dependencies))
        (case command
          ((copy)
           (#{sourcefile-copy!#logical}#
             srcfile
             packaged-dest))
          ((move)
           (#{sourcefile-move!#logical}#
             srcfile
             packaged-dest))
          (else (throw 'bad-command command))))
      (#{sourcefile-copy!#logical}#
        srcfile
        packaged-dest)))
  info)


(#{define#file-cached}#
  (#{iprog-interp-package#ex}# file)
  ((compose sourcefile-path iprog-fileinfo-srcfile)
   (compose make-iprog-fileinfo make-sourcefile)
   ())
  (lambda (srcfile path mtime)
    (appcomp
      srcfile
      make-iprog-fileinfo
      globalize-pak
      save-package)))


(define (iprog-interp-build universe-file)
  (define srcfile
    (if (string? universe-file)
      (make-sourcefile universe-file)
      universe-file))
  (define (full-build)
    (handle-pak
      (#{iprog-interp-package#ex}# srcfile)))
  (define current-rules (make-parameter (list)))
  (define program-accepts
    (lambda (args ctx)
      (catch-any
        (lambda _
          (define input-stream
            (~s (append (current-environment) (current-inputs))))
          (guile-run
            #f
            input-stream
            universe-file
            (map ~a args))
          #t)
        (lambda errors
          (apply cmd-echo
                 (cons "ERROR"
                       (map ~a
                            (apply append
                                   (map (curry-if (negate pair?) list)
                                        errors)))))
          #f))))
  (define handler
    (compose-under
      or
      preprocessor-handler
      (make-handler
        (= unify)
        (build-errors
          (make-set
            (catch-any
              (lambda _ (full-build) (list))
              (lambda ex ex))))
        (build-dest (make-set (list (get-build-dest))))
        (final-hash
          (make-set (list (get-build-final-hash))))
        (hash (make-set (list (get-build-hash))))
        (package-dest
          (make-set (list (guile-package universe-file))))
        (program-accepts program-accepts)
        (inlined-dest (make-set (list (get-inlined))))
        (dependencies
          (make-set
            (iprog-interp-get-dependencies srcfile))))))
  (define (get-list-of name)
    (lambda _
      (let* ((db (create-database handler (current-rules)))
             (key (gensym))
             (query (list (list name key)))
             (re (eval-query db query)))
        ((query-result-find-multiple key) re))))
  (define get-inputs
    (get-list-of query-inputs-name))
  (define get-environment
    (get-list-of query-environment-name))
  (define (get-inlined)
    (parameterize
      ((current-inputs (get-inputs))
       (current-environment (get-environment)))
      (inline (get-build-dest))))
  (define (get-build-dest)
    (parameterize
      ((current-inputs (get-inputs))
       (current-environment (get-environment)))
      (build-struct-path (full-build))))
  (define (get-build-hash)
    (parameterize
      ((current-inputs (get-inputs))
       (current-environment (get-environment)))
      (build-struct-hash (full-build))))
  (define (get-build-final-hash)
    (parameterize
      ((current-inputs (get-inputs))
       (current-environment (get-environment)))
      (build-struct-final-hash (full-build))))
  (define (print re)
    (for-each (lambda (x) (write x) (newline)) re))
  (define (evaluator rules st)
    (parameterize
      ((current-rules rules))
      (let* ((db (create-database handler rules))
             (re (eval-query db st)))
        (cond ((null? re) (display "no\n"))
              ((null? (car re)) (display "yes\n"))
              (else (print re))))))
  (repl-loop (lambda _ #t) evaluator (list)))


(define iprog-interp-package
  (compose
    iprog-fileinfo-srcfile
    #{iprog-interp-package#ex}#))


(define (iprog-interp-get-dependencies target)
  (define (show d)
    (let ((path (dependency-path d)))
      (if (dependency-local? d)
        (if (string-prefix? "./" path)
          path
          (string-append "./" path))
        path)))
  (letin (srcfile
           (if (string? target)
             (make-sourcefile target)
             target))
         (deps (iprog-fileinfo-dependencies
                 (make-iprog-fileinfo srcfile)))
         (initial-deps (map get-initial-dependency deps))
         (global-hashes (map show initial-deps))))


(define current-interpreter (make-parameter #f))


(define current-inputs (make-parameter #f))


(define (#{inline#pure}# built-path)
  (define (get-iname len)
    (lambda (p)
      (letin (index (car p))
             (value (cdr p))
             (return
               (and (> index 1)
                    (< index (- len 2))
                    (= 1 (remainder index 2))
                    (symbol->string (car (car value))))))))
  (define ret0
    (let loop ((built-path built-path))
      (letin (port (open-input-file built-path))
             (header (read port))
             (rest (read-all-port port))
             (do (close-port port))
             (exports0 (last header))
             (exports (map symbol->string exports0))
             (header-len (length header))
             (inames
               (filter
                 identity
                 (map (get-iname header-len)
                      (map cons (range header-len) header))))
             (dependencies
               (map import-location-from-iname inames))
             (my-new-body rest)
             (previous (map loop dependencies))
             (return
               (append
                 (apply append previous)
                 (list (cons built-path my-new-body)))))))
  (define ret
    (let ((h (make-hash-table)))
      (let loop ((buf ret0))
        (if (null? buf)
          (list)
          (let* ((cur-pair (car buf))
                 (index (car cur-pair))
                 (value (cdr cur-pair)))
            (if (hash-ref h index #f)
              (loop (cdr buf))
              (begin
                (hash-set! h index #t)
                (cons value (loop (cdr buf))))))))))
  (apply string-append
         (cons* interpreter-run-keyword
                " "
                guile-name
                "\n"
                ret)))


(define (inline built-path)
  (define text (#{inline#pure}# built-path))
  (with-new-temporary-file
    text
    temp
    (define bresult
      (with-build-inputs
        (list)
        ((build-dest path) ?)
        (guile-abstract-build temp)))
    (build-result-path bresult)))


(define (import-location-from-iname iname)
  (append-posix-path
    (guile-dir)
    (string-append iname ".scm")))


(define (import-location-from-hash hash)
  (import-location-from-iname
    (hash->import-name hash)))


(define (import-statement-from-hash hash names)
  (let ((iname (hash->import-name hash)))
    (string-append
      "   :use-module (("
      iname
      ")\n"
      "                 :select "
      (~a (map symbol->string names))
      ")")))


(define (import-from-statements statements)
  (string-append
    (apply string-append
           (list-intersperse "\n" statements))
    "\n"))


(define (import-from-hashes hashes+names)
  (import-from-statements
    (map (lambda (o)
           (import-statement-from-hash (car o) (cdr o)))
         hashes+names)))


(define (import-from-packaged-dependencies pdeps)
  (let* ((hashes (hashes-of-packaged-dependencies pdeps))
         (names (map dependency-variables pdeps))
         (hashes+names (map cons hashes names)))
    (import-from-hashes hashes+names)))


(define (exports-from-vars vars)
  (define names (map preprocessor-line-args vars))
  (string-append "   :export " (~a names)))


(define (get-header hash pdeps vars)
  (define iname (hash->import-name hash))
  (define imports
    (import-from-packaged-dependencies pdeps))
  (define exports (exports-from-vars vars))
  (string-append
    "(define-module ("
    iname
    ")\n"
    imports
    "\n"
    exports
    ")"))


(define (hash->import-name hash)
  (string-append "x" (~a hash)))


(define (hash-saved? hash)
  (let* ((destination (import-location-from-hash hash)))
    (file-or-directory-exists? destination)))


(define guile-name "guile")


(define-directory
  guile-dir
  (append-posix-path
    (get-import-root-prefix)
    guile-name))


(define (guile-run exec? inputs srcfile args)
  (define args*
    (apply string-append
           (map (lambda (arg) (string-append " '" arg "' "))
                args)))
  (define bresult
    (read-list
      (with-input-from-string
        (string-append inputs "(build-dest path)?")
        (lambda _
          (with-output-to-string
            (lambda _ (iprog-interp-build srcfile)))))))
  (define path (build-result-path bresult))
  (unless path (throw 'wtf: bresult))
  (apply cmd-echo (cons* "RUN" path args))
  (if exec?
    (begin
      (cache-save-to-file!)
      (apply execlp
             (cons* "guile"
                    "guile"
                    "-L"
                    (guile-dir)
                    "-s"
                    path
                    args)))
    (status:exit-val
      (apply system*/exit-code
             (cons* "guile"
                    "guile"
                    "-L"
                    (guile-dir)
                    "-s"
                    path
                    args)))))


(define (guile-package filepath)
  (sourcefile-hash (iprog-interp-package filepath)))


(define (guile-package-toplevel filepath)
  (display (guile-package filepath)))


(define (guile-build-toplevel filepath)
  (iprog-interp-build filepath))


(define (guile-run-toplevel query0 argv0)
  (define target (car argv0))
  (define argv (cdr argv0))
  (define query
    (if (string-null? query0)
      ""
      (substring
        query0
        (string-length query-cli-prefix))))
  (exit (intermezzo
          result
          (cache-save-to-file!)
          (guile-run #t query target argv))))


(define (guile-install-toplevel query0 argv0)
  (define srcfile (car argv0))
  (define linkpath (cadr argv0))
  (define query
    (if (string-null? query0)
      ""
      (substring
        query0
        (string-length query-cli-prefix))))
  (define bresult
    (read-list
      (with-input-from-string
        (string-append
          query
          "(hash hash)? (build-dest path)?")
        (lambda _
          (with-output-to-string
            (lambda _ (iprog-interp-build srcfile)))))))
  (define path (build-result-path bresult))
  (define register-path
    (and path
         (remove-common-prefix
           path
           (get-czempak-root-prefix))))
  (define hash (build-result-hash bresult))
  (unless (and path hash) (throw 'wtf: bresult))
  (apply cmd-echo (list "LINK" path linkpath))
  (write-string-file
    linkpath
    (stringf
      "#! /bin/sh\nexec guile -L '~a' -s '~a' \"$@\"\n"
      (guile-dir)
      path))
  (system (stringf "chmod +x '~a'" linkpath))
  (append-string-file
    (get-install-table-file)
    (string-append
      (get-fullpath linkpath)
      " "
      hash
      " "
      register-path
      "\n")))


(define (guile-list-dependencies-toplevel target)
  (for-each
    (lambda (d) (display d) (newline))
    (iprog-interp-get-dependencies target)))


(define (parse-arg index)
  (let ((args (get-command-line-arguments)))
    (unless
      (> (length args) index)
      (errorwith "Not enough CLI arguments"))
    (list-ref args index)))


(define (get-argv)
  (let* ((args (get-command-line-arguments))
         (d (drop-while
              (lambda (s) (not (equal? "--" s)))
              args))
         (dd (if (pair? d) (cdr d) d)))
    (when (null? dd)
          (errorwith "No program specified in ~s" args))
    dd))


(define (guile-interpreter-entry args)
  (parameterize
    ((get-command-line-arguments args))
    (czempak-top-level-guard
      (let* ((action (parse-arg 0))
             (target (parse-arg 1))
             (query target)
             (lst (list (cons "run"
                              (lambda _ (guile-run-toplevel query (get-argv))))
                        (cons "build"
                              (lambda _ (guile-build-toplevel target)))
                        (cons "install"
                              (lambda _
                                (guile-install-toplevel query (get-argv))))
                        (cons "package"
                              (lambda _ (guile-package-toplevel target)))
                        (cons "list-dependencies"
                              (lambda _
                                (guile-list-dependencies-toplevel target)))))
             (get (assoc action lst))
             (fn (or (and get (cdr get))
                     (lambda _
                       (errorwith
                         "Bad action ~s, expected one of: ~a"
                         action
                         (apply string-append
                                (list-intersperse
                                  " "
                                  (map (compose ~s car) lst))))))))
        (fn)
        (cache-save-to-file!)))))


(hash-set!
  loaded-interpreters-hashmap
  "guile"
  guile-interpreter-entry)


(use-modules (ice-9 regex) (ice-9 rdelim) (ice-9 match))

(define (#{build#action}#)
  (define path (parse-cli-get-flag 1))
  (parameterize
    ((build-input-stream
       (string-append
         (read-all-port (current-input-port))
         "(final-hash hash)?")))
    (display (build path))))


(define (#{package#action}#)
  (define path (parse-cli-get-flag 1))
  (package path))


(define (run func path query args)
  (let* ((interps (get-build-interpreters path))
         (interp-id (car interps))
         (executable
           (get-interpreter-executable interp-id))
         (query-s (if (string-null? query) "" (~s query)))
         (argv (apply string-append (list-intersperse " " args)))
         (argv* (cons* func query "--" path args)))
    (cmd-echo-f
      "INT"
      "~a ~a ~a -- ~s ~a"
      interp-id
      func
      query-s
      path
      argv)
    (cond ((string? executable)
           (apply execlp
                  (cons* executable executable argv*)))
          ((procedure? executable) (executable argv*))
          (else (throw 'bad-executable-type executable)))))


(define (run-helper func path query args)
  (unless
    (file-or-directory-exists? path)
    (errorwith "No such file: ~s" path))
  (catch 'shell-process-failed
         (lambda _ (run func path query args) 0)
         (lambda _ 1)))


(define (#{run#action}#)
  (define head (parse-cli-get-flag 1))
  (define query-set?
    (string-prefix? query-cli-prefix head))
  (define query (if query-set? head ""))
  (define path
    (parse-cli-get-flag (if query-set? 2 1)))
  (define args
    (or (parse-cli-get-list "args")
        (parse-cli-get-list "")
        (parse-cli-get-list (if query-set? 2 1))))
  (exit (run-helper "run" path query args)))


(define (#{run-global#action}#)
  (define head (parse-cli-get-flag 1))
  (define query-set?
    (string-prefix? query-cli-prefix head))
  (define query (if query-set? head ""))
  (define hash
    (parse-cli-get-flag (if query-set? 2 1)))
  (define path
    (append-posix-path
      (get-package-root-directory)
      hash))
  (define args
    (or (parse-cli-get-list "args")
        (parse-cli-get-list "")
        (parse-cli-get-list (if query-set? 2 1))))
  (unless path (download hash))
  (run-helper "run" path query args))


(define (#{install#action}#)
  (define head (parse-cli-get-flag 1))
  (define query-set?
    (string-prefix? query-cli-prefix head))
  (define query (if query-set? head ""))
  (define path
    (parse-cli-get-flag (if query-set? 2 1)))
  (define install-path
    (parse-cli-get-flag (if query-set? 3 2)))
  (define args
    (or (parse-cli-get-list "args")
        (parse-cli-get-list "")
        (parse-cli-get-list (if query-set? 2 1))))
  (exit (run-helper "install" path query args)))


(define (#{list-dependencies#action}#)
  (define target (parse-cli-get-flag 1))
  (list-dependencies target))


(czempak-top-level-guard
  (let ((action (parse-cli-get-flag 0)))
    (match action
           ("build" (#{build#action}#))
           ("package" (#{package#action}#))
           ("run" (#{run#action}#))
           ("run-global" (#{run-global#action}#))
           ("install" (#{install#action}#))
           (other (errorwith "Bad action ~s" action)))))


