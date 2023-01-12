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
%use (raisu) "./euphrates/raisu.scm"
%use (stringf) "./euphrates/stringf.scm"
%use (~a) "./euphrates/tilda-a.scm"
%use (words->string) "./euphrates/words-to-string.scm"
%use (keyword-tags) "./keyword-tags.scm"
%use (web::callcontext/p) "./web-callcontext-p.scm"
%use (callcontext-query callcontext-token) "./web-callcontext.scm"
%use (web::details::continue) "./web-detailscont.scm"
%use (web::form-template) "./web-form-template.scm"
%use (web::get-query) "./web-get-query.scm"
%use (web::iterate-profun-results) "./web-iterate-profun-results.scm"
%use (web::make-html-response) "./web-make-html-response.scm"
%use (web::not-found) "./web-not-found.scm"
%use (webcore::ask) "./webcore-ask.scm"

(define (web::actual-details entry)
  (define ctxq (web::get-query))
  (define vid (hashmap-ref ctxq 'vid #f))
  (define id (hashmap-ref ctxq 'id #f))
  (define vid/id
    (cond
     (vid (string-append "vid=" vid))
     (id (string-append "id=" id))
     (else (raisu 'impossible-case-172376))))

  (define table
    (with-output-to-string
      (lambda _
        (display "<div class='details-table'>") (newline)

        (for-each
         (lambda (row)
           (define name (car row))
           (define val (cdr row))

           (unless (string-prefix? "%" (~a name))
             (display "<div class='form-block form-v-element'>") (newline)
             (display "  <label>")
             (display name)
             (display "</label") (newline)
             (display "</div>") (newline)

             (display "<div class='form-block form-v-element'>") (newline)
             (display "  <input type='text' value=")
             (cond
              ((equal? name keyword-tags)
               (write (words->string (map ~a val))))
              (else
               (write val)))
             (display " name=")
             (write (string-append "field:" (~a name)))
             (display " />") (newline)
             (display "</div>") (newline)

             )

           )

         entry)

        (display "
        <div class='form-block form-v-element'>
          <button type='submit' name='action' type='submit' value='update'>Update</button>
        </div>
        <div class='form-block form-v-element'>
          <button id='dangerous' name='action' type='submit' value='delete'>Delete</button>
        </div>")

        (display "</div>\n"))))

  (web::make-html-response
   (web::form-template
    (stringf "action='details?continue=on&~a' enctype='multipart/form-data'" vid/id)
    table)))

(define (web::details::initial)
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
   :onfalse (lambda _ (web::not-found))
   :results result (E)
   (web::actual-details E)))

(define (web::details)
  (define callctx (web::callcontext/p))
  (define query (callcontext-query callctx))
  (define continue-v (hashmap-ref query 'continue #f))
  (define continue? (equal? "on" continue-v))

  (if continue?
      (web::details::continue)
      (web::details::initial)))
