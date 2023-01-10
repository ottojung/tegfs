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

%var web::details

%use (hashmap-ref) "./euphrates/hashmap.scm"
%use (web::callcontext/p) "./web-callcontext-p.scm"
%use (callcontext-token) "./web-callcontext.scm"
%use (web::get-query) "./web-get-query.scm"
%use (web::iterate-profun-results) "./web-iterate-profun-results.scm"
%use (web::make-html-response) "./web-make-html-response.scm"
%use (webcore::ask) "./webcore-ask.scm"

(define (web::actual-details entry)
  (define table
    (with-output-to-string
      (lambda _
        ;; (display "<style> body { height: 100% } </style>")
        (display "<div class='centering-container'><table class='styled-table'>\n")
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
        (display "</table></div>\n"))))

  (web::make-html-response table))

(define (web::details)
  (define callctx (web::callcontext/p))
  (define ctxq (web::get-query))
  (define vid (hashmap-ref ctxq 'vid #f))
  (define id (hashmap-ref ctxq 'id #f))
  (define key (callcontext-token callctx))

  (define result
    (webcore::ask
     `(listen
       ((goal X) (senderid->entry ,vid X))
       ((goal X) (entry X) (entry-field X "id" ,id))
       whats
       (key ,key)
       (goal E)
       )))

  (web::iterate-profun-results
   result (E)
   (web::actual-details E)))
