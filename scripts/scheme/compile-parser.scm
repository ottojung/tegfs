;;;; Copyright (C) 2024  Otto Jung
;;;; This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU Affero General Public License for more details. You should have received a copy of the GNU Affero General Public License along with this program.  If not, see <https://www.gnu.org/licenses/>.


(define (process <source> <compiler>)
  (define scm-source
    (path-replace-extension <source> ".scm"))

  (define contents
    (list-last (call-with-input-file scm-source read-list)))

  (assert (list? contents))
  (assert= (car contents) 'define)

  (let ()
    (define definition (cadr contents))
    (define source-path
      (path-get-basename
       (path-without-extension <source>)))
    (define target-path
      (irregex-replace
       '(seq "definition" eos) source-path "implementation"))
    (define name
      (list
       'quote
       (string->symbol
        (irregex-replace
         '(seq "definition" eos) (~a definition) "implementation"))))

    (define compiler-definition
      (string->symbol
       (irregex-replace
        '(seq "definition" eos) (~a definition) "compiler")))

    (call-with-output-file
        <compiler>
      (lambda (port)
        (parameterize ((current-output-port port))
          (write
           `(define-library
                (,compiler-definition)
              (import (tegfs ,(string->symbol source-path)))
              (import (parser-compiler-generic))
              (import
               (only (scheme base) begin quote))
              (begin
                (parser-compiler/generic
                 ,definition
                 ,target-path
                 ,name
                 )))))))))

(define (main)
  (parameterize ((current-program-path/p "tegfs"))
    (with-cli
     (--help
      /
      <source> <compiler>
      )

     (cond
      (--help
       (define-cli:show-help))
      ((and <source> <compiler>)
       (process <source> <compiler>))
      (else
       (exit 1))))))

(main)
