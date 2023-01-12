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

%var web::details::continue

%use (assoc-set-value) "./euphrates/assoc-set-value.scm"
%use (assq-or) "./euphrates/assq-or.scm"
%use (hashmap-foreach hashmap-ref) "./euphrates/hashmap.scm"
%use (memconst) "./euphrates/memconst.scm"
%use (raisu) "./euphrates/raisu.scm"
%use (string-drop-n) "./euphrates/string-drop-n.scm"
%use (string->words) "./euphrates/string-to-words.scm"
%use (keyword-id) "./keyword-id.scm"
%use (keyword-tags) "./keyword-tags.scm"
%use (update-entry) "./update-entry.scm"
%use (web::body::get-data/decode) "./web-body-get-data.scm"
%use (web::body-not-found) "./web-body-not-found.scm"
%use (web::callcontext/p) "./web-callcontext-p.scm"
%use (callcontext-body callcontext-token) "./web-callcontext.scm"
%use (web::get-query) "./web-get-query.scm"
%use (web::iterate-profun-results) "./web-iterate-profun-results.scm"
%use (web::not-found) "./web-not-found.scm"
%use (parse-multipart-as-hashmap) "./web-parse-multipart.scm"
%use (web::return) "./web-return.scm"
%use (webcore::ask) "./webcore-ask.scm"

(use-modules (ice-9 pretty-print))

(define (web::detailscont/3 callctx body/bytes original-entry)
  (define body/hash
    (parse-multipart-as-hashmap body/bytes))

  (define get-fields
    (memconst
     (let* ((coll '())
            (pref "field:")
            (skip (string-length pref)))
       (hashmap-foreach
        (lambda (key val)
          (when (string-prefix? pref key)
            (let* ((key*
                    (string->symbol
                     (string-drop-n skip key)))
                   (val* (web::body::get-data/decode body/hash key))
                   (val**
                    (if (equal? key* keyword-tags)
                        (map string->symbol (string->words val*))
                        val*))
                   (p (cons key* val**)))
              (set! coll (cons p coll)))))
        body/hash)
       coll)))

  (define action
    (web::body::get-data/decode body/hash "action"))

  (define updated-entry
    (cond
     ((equal? action "delete") #f)
     ((equal? action "update")
      (let loop ((ret original-entry)
                 (left (get-fields)))
        (if (null? left) ret
            (let* ((p (car left))
                   (key (car p))
                   (val (cdr p)))
              (loop
               (assoc-set-value
                key val ret)
               (cdr left))))))
     (else
      (raisu 'bad-action "Bad action. Expected 'update' or 'delete'"))))

  (newline)
  (display "UPDATED:") (newline)
  (pretty-print updated-entry)
  (newline)
  (newline)

  (define key (callcontext-token callctx))
  (define id
    (or (assq-or keyword-id original-entry #f)
        (raisu 'entry-does-not-have-an-id original-entry)))
  (define result
    (webcore::ask
     `(whats
       (key ,key)
       (entry E)
       (entry-field E "id" ,id)
       (update-entry E ,updated-entry))))

  (web::iterate-profun-results
   :results result (E)
   (let* ((ctxq (web::get-query))
          (vid (hashmap-ref ctxq 'vid #f))
          (id (hashmap-ref ctxq 'id #f))
          (L (cond
              (vid (string-append "details?vid=" vid))
              (id (string-append "details?id=" id))
              (else "home"))))
     (web::return
      303
      `((Location . ,L)
        (Cache-Control . "no-cache"))
      #f))))

(define (web::detailscont/2 callctx body/bytes)
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
   (web::detailscont/3 callctx body/bytes E)))

(define (web::details::continue)
  (define callctx (web::callcontext/p))
  (define body/bytes (callcontext-body callctx))
  (if body/bytes
      (web::detailscont/2 callctx body/bytes)
      (web::body-not-found)))
