
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

(check-dependency "jq")


