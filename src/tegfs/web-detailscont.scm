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
  (define-module (tegfs web-detailscont)
    :export (web::details::continue)
    :use-module ((euphrates assoc-set-value) :select (assoc-set-value))
    :use-module ((euphrates assq-or) :select (assq-or))
    :use-module ((euphrates hashmap) :select (hashmap-foreach hashmap-ref))
    :use-module ((euphrates memconst) :select (memconst))
    :use-module ((euphrates raisu) :select (raisu))
    :use-module ((euphrates string-drop-n) :select (string-drop-n))
    :use-module ((euphrates string-to-words) :select (string->words))
    :use-module ((tegfs keyword-id) :select (keyword-id))
    :use-module ((tegfs keyword-tags) :select (keyword-tags))
    :use-module ((tegfs update-entry) :select (update-entry))
    :use-module ((tegfs web-body-get-data) :select (web::body::get-data/decode))
    :use-module ((tegfs web-body-not-found) :select (web::body-not-found))
    :use-module ((tegfs web-callcontext-p) :select (web::callcontext/p))
    :use-module ((tegfs web-callcontext) :select (callcontext-body callcontext-token))
    :use-module ((tegfs web-get-query) :select (web::get-query))
    :use-module ((tegfs web-iterate-profun-results) :select (web::iterate-profun-results))
    :use-module ((tegfs web-make-info-box-response) :select (web::make-info-box-response))
    :use-module ((tegfs web-not-found) :select (web::not-found))
    :use-module ((tegfs web-parse-multipart) :select (parse-multipart-as-hashmap))
    :use-module ((tegfs web-redirect) :select (web::redirect))
    :use-module ((tegfs webcore-ask) :select (webcore::ask))
    )))



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

     (cond
      ((equal? action "delete")
       (web::make-info-box-response "Entry deleted"))
      ((equal? action "update")
       (web::redirect callctx L #f))
      (else
       (raisu 'bad-action "Bad action. Expected 'update' or 'delete'"))))))

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
