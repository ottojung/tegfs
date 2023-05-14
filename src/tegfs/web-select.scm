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
  (define-module (tegfs web-select)
    :export (web::select)
    :use-module ((euphrates absolute-posix-path-q) :select (absolute-posix-path?))
    :use-module ((euphrates assq-set-value) :select (assq-set-value))
    :use-module ((euphrates fn-pair) :select (fn-pair))
    :use-module ((euphrates hashmap) :select (alist->hashmap hashmap->alist hashmap-delete! hashmap-has?))
    :use-module ((euphrates list-deduplicate) :select (list-deduplicate))
    :use-module ((euphrates md5) :select (md5-digest))
    :use-module ((euphrates raisu) :select (raisu))
    :use-module ((euphrates remove-common-prefix) :select (remove-common-prefix))
    :use-module ((euphrates string-drop-n) :select (string-drop-n))
    :use-module ((euphrates string-to-words) :select (string->words))
    :use-module ((tegfs add-entry) :select (add-entry))
    :use-module ((tegfs entry-card-name) :select (entry-card-name-id entry-card-name-type))
    :use-module ((tegfs entry-get-id) :select (entry-get-id))
    :use-module ((tegfs entry-get-tags) :select (entry-get-tags))
    :use-module ((tegfs get-db-path) :select (get-db-path))
    :use-module ((tegfs keyword-id) :select (keyword-id))
    :use-module ((tegfs keyword-tags) :select (keyword-tags))
    :use-module ((tegfs keyword-target) :select (keyword-target))
    :use-module ((tegfs update-entry) :select (update-entry))
    :use-module ((tegfs web-body-get-data) :select (web::body::get-data/decode))
    :use-module ((tegfs web-body-not-found) :select (web::body-not-found))
    :use-module ((tegfs web-callcontext-p) :select (web::callcontext/p))
    :use-module ((tegfs web-callcontext) :select (callcontext-body callcontext-token))
    :use-module ((tegfs web-iterate-profun-results) :select (web::iterate-profun-results))
    :use-module ((tegfs web-make-html-response) :select (web::make-html-response))
    :use-module ((tegfs web-parse-multipart) :select (parse-multipart-as-hashmap))
    :use-module ((tegfs webcore-ask) :select (webcore::ask))
    )))



(define (web::select)
  (define callctx (web::callcontext/p))
  (define body/bytes (callcontext-body callctx))
  (define token (callcontext-token callctx))

  (cond
   ((not body/bytes)
    (web::body-not-found))
   (else
    (let ()
      (define body/hash
        (parse-multipart-as-hashmap body/bytes))

      (define additional/s
        (or (web::body::get-data/decode body/hash "aditional") ""))
      (define additional
        (map string->symbol
             (string->words additional/s)))

      (define names
        (map car (hashmap->alist body/hash)))

      (define ids
        (filter
         identity
         (map
          (lambda (name)
            (define id (entry-card-name-id callctx name))
            (define type (entry-card-name-type name))
            (and id (cons id type)))
          names)))

      (define ids-left
        (alist->hashmap ids))

      (define (modify-existing-tags lefts)
        (web::iterate-profun-results
         (webcore::ask
          `(whats
            (key ,token)
            (entry E)
            more (99999)))
         (E)
         (begin
           (define id (entry-get-id E))
           (when (hashmap-has? lefts id)
             (let ()
               (define current-tags (entry-get-tags E))
               (define new-tags
                 (list-deduplicate (append additional current-tags)))
               (define new-entry
                 (assq-set-value keyword-tags new-tags E))

               ;; TODO: Am I intentionally ignoring the result here???
               ;; TODO: This makes me O(n^2) SLOW
               (webcore::ask
                `(whats
                  (key ,token)
                  (entry _E)
                  (entry-field _E "id" ,id)
                  (update-entry _E ,new-entry)))

               (hashmap-delete! lefts id))))))

      (modify-existing-tags ids-left)

      ;; check if we have some unrecognized ids now
      (for-each
       (fn-pair
        (id type)
        (cond
         ((equal? type 'entry-card-name-id)
          (raisu 'unrecognized-id id
                 "Probably is not in the database"))
         ((not (equal? type 'entry-card-name-senderid))
          (raisu 'unexpected-entry-card-name-type type id))
         ((not (absolute-posix-path? id))
          (raisu 'expected-file-entry id type))))
       (hashmap->alist ids-left))

      ;; Now we know that all our ids are filepaths.
      ;; Using hashsed ids for them.
      (let ()
        (define hashed
          (alist->hashmap
           (map
            (fn-pair
             (id type)
             (define relative
               (remove-common-prefix
                id (get-db-path)))
             (when (string-null? relative)
               (raisu 'bad-selected-file-path/1 relative id))
             (unless (absolute-posix-path? relative)
               (raisu 'bad-selected-file-path/2 relative id))
             (cons (md5-digest relative) relative))
            (hashmap->alist ids-left))))

        (modify-existing-tags hashed)

        ;; What's still left is files that never been added to the database before,
        ;; so we add them now.
        (for-each
         (fn-pair
          (id-hashed relative/0)
          (define relative
            (if (string-prefix? "/" relative/0)
                (string-drop-n 1 relative/0) relative/0))
          (define new-entry
            (list
             (cons keyword-id id-hashed)
             (cons keyword-target relative)
             (cons keyword-tags additional)))

          ;; TODO: Am I intentionally ignoring the result here???
          (webcore::ask
           `(whats
             (key ,token)
             (add-entry ,new-entry))))
         (hashmap->alist hashed))

        (web::make-html-response "OKAY YOUR GOOD"))))))
