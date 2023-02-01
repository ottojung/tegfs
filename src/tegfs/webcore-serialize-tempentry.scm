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

(cond-expand
 (guile
  (define-module (tegfs webcore::serialize-tempentry)
    :export (webcore::serialize-tempentry)
    :use-module ((euphrates raisu) :select (raisu))
    :use-module ((euphrates serialization-short) :select (serialize/short))
    :use-module ((tegfs keyword-id) :select (keyword-id))
    :use-module ((tegfs keyword-temptype) :select (keyword-temptype))
    :use-module ((tegfs permission) :select (permission?))
    :use-module ((tegfs sharedinfo) :select (sharedinfo?))
    :use-module ((tegfs sharereceipt) :select (sharereceipt?))
    )))

(define (webcore::serialize-tempentry id tempentry)
  (cond
   ((or (sharedinfo? tempentry)
        (sharereceipt? tempentry)
        (permission? tempentry))
    (let ((s (serialize/short tempentry)))
      (cons (cons keyword-id id)
            (cons (cons keyword-temptype (car s))
                  (cdr s)))))
   ((pair? tempentry)
    (cons (cons keyword-id id) tempentry))
   (else (raisu 'type-error "Unrecognized temporary entry" tempentry))))
