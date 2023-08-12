
(define args
  (cdr (command-line)))

(define code_root (car args))

(display "#! ")
(flush-all-ports)
(system* "sh" "-c" "command -v sh")
(flush-all-ports)

(display "exec guile --r7rs -L ")
(write code_root)
(display " -s ")
(write (string-append code_root "/tegfs/tegfs.sld"))
(display " \"$@\"")
(newline)

(flush-all-ports)
