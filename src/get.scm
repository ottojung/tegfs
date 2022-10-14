;;;; Copyright (C) 2022  Otto Jung
;;;;
;;;; This program is free software: you can redistribute it and/or modify
;;;; it under the terms of the GNU General Public License as published by
;;;; the Free Software Foundation; version 3 of the License.
;;;;
;;;; This program is distributed in the hope that it will be useful,
;;;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;;;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;;;; GNU General Public License for more details.
;;;;
;;;; You should have received a copy of the GNU General Public License
;;;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

%run guile

%var tegfs-get
%var tegfs-get/parse
%var tegfs-get/cached

%use (absolute-posix-path?) "./euphrates/absolute-posix-path-q.scm"
%use (dprintln) "./euphrates/dprintln.scm"
%use (hashmap-clear! hashmap-ref make-hashmap) "./euphrates/ihashmap.scm"
%use (entries-for-each) "./entries-for-each.scm"
%use (entries->hashmap) "./entries-to-hashmap.scm"
%use (entry-print/formatted) "./entry-print-formatted.scm"
%use (entry-print) "./entry-print.scm"
%use (standalone-file->entry) "./standalone-file-to-entry.scm"

(define (tegfs-get/parse <get-format> <showid>)
  (define entry (tegfs-get <showid>))
  (if entry
      (if <get-format>
          (entry-print/formatted <get-format> entry)
          (entry-print entry))
      (display "Not found." (current-error-port)))
  (newline))

(define (tegfs-get <showid>)
  (if (absolute-posix-path? <showid>)
      (standalone-file->entry <showid> #f)
      (call-with-current-continuation
       (lambda (k)
         (entries-for-each
          (lambda (entry)
            (define id-pair (assoc 'id entry))
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
          (standalone-file->entry <showid> #f)
          (or (hashmap-ref H <showid> #f)
              (begin
                (hashmap-clear! H)
                (entries->hashmap H)
                (hashmap-ref H <showid> #f)))))))
