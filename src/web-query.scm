;;;; Copyright (C) 2022  Otto Jung
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

%var web::query

%use (hashmap-ref) "./euphrates/hashmap.scm"
%use (printf) "./euphrates/printf.scm"
%use (string->words) "./euphrates/string-to-words.scm"
%use (default-full-sharing-time) "./default-full-sharing-time.scm"
%use (default-preview-sharing-time) "./default-preview-sharing-time.scm"
%use (web::body->hashmap) "./web-body-to-hashmap.scm"
%use (web::callcontext/p) "./web-callcontext-p.scm"
%use (callcontext-body callcontext-query callcontext-token) "./web-callcontext.scm"
%use (web::decode-query) "./web-decode-query.scm"
%use (web::handle-profun-results) "./web-handle-profun-results.scm"
%use (web::make-html-response) "./web-make-html-response.scm"
%use (web::query-display-results) "./web-query-display-results.scm"
%use (webcore::ask) "./webcore-ask.scm"

(define (web::query)
  (define callctx (web::callcontext/p))
  (define ctxq (callcontext-query callctx))

  (define query/submitted/0
    (or
     (hashmap-ref ctxq 'q #f)
     (let* ((body (callcontext-body callctx))
            (bH (and body (web::body->hashmap body))))
       (and bH
            (hashmap-ref bH 'q #f)))))
  (define query/submitted
    (if (or (not query/submitted/0)
            (string-null? query/submitted/0)) #f
        query/submitted/0))
  (define query/encoded
    (or query/submitted "%any"))
  (define query (web::decode-query query/encoded))
  (define query/split (string->words query))

  (define result
    (webcore::ask
     `(whats
       (key ,(callcontext-token callctx))
       (query ,query/split)
       (entry E)
       (share-preview E ,default-preview-sharing-time _ATP _P)
       (share-full E ,default-full-sharing-time _ATF F)
       (link-shared _P PL)
       more (99999)
       )))

  (web::handle-profun-results
   result (web::query-handle-results query/submitted query)))

(define (web::query-handle-results query/submitted query)
  (lambda (equals)
    (web::make-html-response
     (lambda _
       (define maybe-value
         (if query/submitted
             (string-append " value='" query "'")
             ""))

       (display "<br/>\n")
       (display "<div class='search-input'>\n")
       (display "<div class='tiled light smooth-edged'>\n")
       (display "<div>\n")
       (display "<form class='split-container' action='query'>\n")
       (printf "  <input class='split-left' ~a autofocus type='text' name='q' placeholder='Filter by tags' />\n"
               maybe-value)
       (display " <input type='image' class='split-right' src='static/search.svg' alt='Submit'/>")
       (display "</form>\n")
       (display "</div>\n")
       (display "</div>\n")
       (display "</div>\n")
       (display "<br/>\n")

       (web::query-display-results equals)))))
