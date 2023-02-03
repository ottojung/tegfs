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
  (define-module (tegfs update-tempentry)
    :export (update-tempentry)
    :use-module ((euphrates assq-or) :select (assq-or))
    :use-module ((euphrates fn-pair) :select (fn-pair))
    :use-module ((euphrates hashmap) :select (hashmap-delete! hashmap-ref))
    :use-module ((euphrates raisu) :select (raisu))
    :use-module ((euphrates stringf) :select (stringf))
    :use-module ((tegfs custom-tempentry) :select (custom-tempentry-date custom-tempentry-id custom-tempentry-stime custom-tempentry? set-custom-tempentry-date! set-custom-tempentry-fields! set-custom-tempentry-stime!))
    :use-module ((tegfs keyword-date) :select (keyword-date))
    :use-module ((tegfs keyword-id) :select (keyword-id))
    :use-module ((tegfs keyword-stime) :select (keyword-stime))
    )))

(define (get-mandatory-field tempentry field-name)
  (assq-or field-name tempentry
           (raisu 'type-error (stringf "Updated tempentry missing ~s field" field-name))))

(define (modify-custom-tempentry orig updated-tempentry)
  (define new-date
    (get-mandatory-field updated-tempentry keyword-date))
  (define new-stime
    (get-mandatory-field updated-tempentry keyword-stime))
  (define new-id
    (assq-or keyword-id updated-tempentry #f))
  (define new-fields
    (filter
     (fn-pair
      (key val)
      (not (memq key (list keyword-id keyword-stime keyword-date))))
     updated-tempentry))

  (when new-id
    (unless (equal? new-id (custom-tempentry-id orig))
      (raisu 'permission-denied (stringf "Cannot update ~s field of a tempentry" keyword-id))))

  (unless (equal? new-date
                  (custom-tempentry-date orig))
    (set-custom-tempentry-date! orig new-date))

  (unless (equal? new-stime
                  (custom-tempentry-stime orig))
    (set-custom-tempentry-stime! orig new-stime))

  (set-custom-tempentry-fields! orig new-fields)

  (when #f #t))

(define (update-tempentry::continue tempentries id updated-tempentry)
  (define get
    (hashmap-ref tempentries id (raisu 'not-found "Original tempentry does not exist.")))

  (if (custom-tempentry? get)
      (if updated-tempentry
          (modify-custom-tempentry get updated-tempentry)
          (hashmap-delete! tempentries id))
      (raisu 'permission-denied "Only custom created tempentries can be updated directly.")))

(define (update-tempentry tempentries original-tempentry updated-tempentry)
  (define id
    (get-mandatory-field original-tempentry keyword-id))
  (update-tempentry::continue tempentries id updated-tempentry))
