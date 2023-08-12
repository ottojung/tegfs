;;;; Copyright (C) 2022, 2023  Otto Jung
;;;; This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU Affero General Public License for more details. You should have received a copy of the GNU Affero General Public License along with this program.  If not, see <https://www.gnu.org/licenses/>.

(define (tegfs-get/parse <get-format> <showid>)
  (define entry (tegfs-get <showid>))
  (if entry
      (if <get-format>
          (entry-print/formatted <get-format> entry)
          (entry-print entry))
      (begin
        (log-info "Not found.")
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
                (log-warning
                 "Entry does not have an id: ~s" entry))))
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
