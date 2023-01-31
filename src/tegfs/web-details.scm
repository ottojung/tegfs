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

(cond-expand
 (guile
  (define-module (tegfs web-details)
    :export (web::details)
    :use-module ((euphrates hashmap) :select (hashmap-ref))
    :use-module ((euphrates printf) :select (printf))
    :use-module ((euphrates raisu) :select (raisu))
    :use-module ((euphrates stringf) :select (stringf))
    :use-module ((euphrates tilda-a) :select (~a))
    :use-module ((euphrates words-to-string) :select (words->string))
    :use-module ((tegfs keyword-tags) :select (keyword-tags))
    :use-module ((tegfs keyword-target) :select (keyword-target))
    :use-module ((tegfs web-callcontext-p) :select (web::callcontext/p))
    :use-module ((tegfs web-callcontext) :select (callcontext-query callcontext-token))
    :use-module ((tegfs web-detailscont) :select (web::details::continue))
    :use-module ((tegfs web-form-template-wide) :select (web::form-template/wide))
    :use-module ((tegfs web-get-query) :select (web::get-query))
    :use-module ((tegfs web-iterate-profun-results) :select (web::iterate-profun-results))
    :use-module ((tegfs web-make-html-response) :select (web::make-html-response))
    :use-module ((tegfs web-not-found) :select (web::not-found))
    :use-module ((tegfs webcore-ask) :select (webcore::ask))
    )))



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
             (when (and vid (equal? name keyword-target))
               (printf "
                   <div class='target-share'>
                     <div id='imgbox' class='form-block form-v-element'>
                       <a href='share?vid=~a'>
                         <img src='static/share.svg' title='Share this file'/>
                       </a>
                     </div>" vid))

             (display "<div class='form-block form-v-element'>") (newline)
             (display "  <label>")
             (display name)
             (display "</label>") (newline)
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

             (when (and vid (equal? name keyword-target))
               (display "</div>\n"))

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
   (web::form-template/wide
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
