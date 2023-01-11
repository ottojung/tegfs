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

%run guile

%var web::tags

%use (hashmap-ref) "./euphrates/hashmap.scm"
%use (raisu) "./euphrates/raisu.scm"
%use (web::body::get-data/decode) "./web-body-get-data.scm"
%use (web::body-not-found) "./web-body-not-found.scm"
%use (web::callcontext/p) "./web-callcontext-p.scm"
%use (callcontext-body callcontext-query callcontext-token) "./web-callcontext.scm"
%use (web::iterate-profun-results) "./web-iterate-profun-results.scm"
%use (web::make-html-response) "./web-make-html-response.scm"
%use (parse-multipart-as-hashmap) "./web-parse-multipart.scm"
%use (web::tags::make-page) "./web-tags-make-page.scm"
%use (webcore::ask) "./webcore-ask.scm"

(define (web::tags::continue/2 callctx query body/bytes)
  (define body/hash (parse-multipart-as-hashmap body/bytes))
  (define key (callcontext-token callctx))

  (define new-categorization
    (or (web::body::get-data/decode body/hash "categorization")
        (raisu 'bad-body-sent)))

  (define result
    (webcore::ask
     `(whats (= X 1) (key ,key) (set-categorization ,new-categorization))))

  (web::iterate-profun-results
   result (X)
   (web::make-html-response
    (web::tags::make-page #t new-categorization))))

(define (web::tags::continue callctx query)
  (define body/bytes (callcontext-body callctx))
  (if body/bytes
      (web::tags::continue/2 callctx query body/bytes)
      (web::body-not-found)))

(define (web::tags::initial callctx query)
  (define key (callcontext-token callctx))

  (define result
    (webcore::ask
     `(whats (key ,key) (categorization C))))

  (web::iterate-profun-results
   result (C)
   (web::make-html-response
    (web::tags::make-page #f C))))

(define (web::tags)
  (define callctx (web::callcontext/p))
  (define query (callcontext-query callctx))
  (define continue-v (hashmap-ref query 'continue #f))
  (define continue? (equal? "on" continue-v))

  (if continue?
      (web::tags::continue callctx query)
      (web::tags::initial callctx query)))
