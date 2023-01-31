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

(cond-expand
 (guile
  (define-module (tegfs get)
    :export (tegfs-get tegfs-get/parse tegfs-get/cached)
    :use-module ((euphrates absolute-posix-path-q) :select (absolute-posix-path?))
    :use-module ((euphrates dprintln) :select (dprintln))
    :use-module ((euphrates hashmap) :select (hashmap-clear! hashmap-ref make-hashmap))
    :use-module ((tegfs entries-for-each) :select (entries-for-each))
    :use-module ((tegfs entries-to-hashmap) :select (entries->hashmap))
    :use-module ((tegfs entry-print-formatted) :select (entry-print/formatted))
    :use-module ((tegfs entry-print) :select (entry-print))
    :use-module ((tegfs keyword-id) :select (keyword-id))
    :use-module ((tegfs standalone-file-to-entry) :select (standalone-file->entry))
    )))



(define (tegfs-get/parse <get-format> <showid>)
  (define entry (tegfs-get <showid>))
  (if entry
      (if <get-format>
          (entry-print/formatted <get-format> entry)
          (entry-print entry))
      (begin
        (display "Not found." (current-error-port))
        (exit 1)))
  (newline))

(define (tegfs-get <showid>)
  (if (absolute-posix-path? <showid>)
      (standalone-file->entry <showid>)
      (call-with-current-continuation
       (lambda (k)
         (entries-for-each
          (lambda (entry)
            (define id-pair (assoc keyword-id entry))
            (if id-pair
                (when (equal? (cdr id-pair) <showid>)
                  (k entry))
                (parameterize ((current-output-port (current-error-port)))
                  (dprintln "Entry does not have an id: ~s" entry)))))
         #f))))

(define tegfs-get/cached
  (let ((H (make-hashmap)))
    (lambda (<showid>)
      (if (absolute-posix-path? <showid>)
          (standalone-file->entry <showid>)
          (or (hashmap-ref H <showid> #f)
              (begin
                (hashmap-clear! H)
                (entries->hashmap H)
                (hashmap-ref H <showid> #f)))))))
