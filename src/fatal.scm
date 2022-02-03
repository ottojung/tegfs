
%run guile

%var fatal

%use (dprintln) "./euphrates/dprintln.scm"

(define (fatal fmt . args)
  (parameterize ((current-output-port (current-error-port)))
    (apply dprintln (cons fmt args)))
  (exit 1))

