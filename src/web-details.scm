;;;; Copyright (C) 2022, 2023  Otto Jung
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

%run guile

%var web-details

%use (hashmap-ref) "./euphrates/hashmap.scm"
%use (profune-communicator-handle) "./euphrates/profune-communicator.scm"
%use (raisu) "./euphrates/raisu.scm"
%use (~s) "./euphrates/tilda-s.scm"
%use (words->string) "./euphrates/words-to-string.scm"
%use (web-bad-request) "./web-bad-request.scm"
%use (web-callcontext/p) "./web-callcontext-p.scm"
%use (web-context/p) "./web-context-p.scm"
%use (web-get-key) "./web-get-key.scm"
%use (web-get-query) "./web-get-query.scm"
%use (web-make-communicator) "./web-make-communicator.scm"
%use (web-respond) "./web-respond.scm"

(define (web-actual-details entry)
  (define table
    (with-output-to-string
      (lambda _
        (display "<table class='styled-table subc'>\n")
        (display "  <thead><tr><th>Prop</th><th>Value</th></tr></thead>\n")
        (for-each
         (lambda (row)
           (define name (car row))
           (define val (cdr row))
           (display "  <tbody>\n")
           (display "    <tr>\n")
           (display "      <td>")
           (display name)
           (display "</td>\n")
           (display "      <td>")
           (display val)
           (display "</td>\n")
           (display "    </tr>\n")
           (display "  </tbody>\n"))
         entry)
        (display "</table>\n"))))

  (web-respond table))

(define (web-details)
  (define callctx (web-callcontext/p))
  (define ctxq (web-get-query))
  (define vid (hashmap-ref ctxq 'vid #f))
  (define id (hashmap-ref ctxq 'id #f))
  (define key (web-get-key callctx))

  (define result
    (profune-communicator-handle
     (web-make-communicator (web-context/p))
     `(listen
       ((goal X) (senderid->entry ,vid X))
       ((goal X) (query ("%any")) (entry X) (entry-field X "id" ,id))
       whats
       (key ,key)
       (goal E)
       )))

  (cond
   ((equal? 'error (car result))
    (web-bad-request "error: ~a" (words->string (map ~s (cadr result)))))
   ((and (equal? 'its (car result))
         (equal? '= (car (cadr result))))
    (let* ((word (cadr result))
           (entry (list-ref word 2)))
      (web-actual-details entry)))
   (else
    (raisu 'profun-returned-something-weird result))))


