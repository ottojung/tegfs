;;;; Copyright (C) 2024  Otto Jung
;;;; This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU Affero General Public License for more details. You should have received a copy of the GNU Affero General Public License along with this program.  If not, see <https://www.gnu.org/licenses/>.

(define (parser-compiler/generic/fun definition path name)
  (define serialized (parselynn:simple:serialize definition))
  (call-with-output-file
      (stringf "src/tegfs/~a.sld" path)
    (lambda (port)
      (parameterize ((current-output-port port))
        (write
         `(define-library
              (tegfs ,(string->symbol path))
            (export ,name)
            (import (scheme base))
            (import (scheme char))
            (import (euphrates parselynn-simple-deserialize-lists))
            (begin
              (define ,name
                (parselynn:simple:deserialize/lists
                 ,serialized)))))))))

(define-syntax parser-compiler/generic
  (syntax-rules ()
    ((_ definition path name)
     (parser-compiler/generic/fun
      definition path name))))
