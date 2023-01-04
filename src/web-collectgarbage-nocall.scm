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

%run guile

%var web-collectgarbage/nocall

%use (directory-files) "./euphrates/directory-files.scm"
%use (file-delete) "./euphrates/file-delete.scm"
%use (hashmap-delete! hashmap-foreach) "./euphrates/hashmap.scm"
%use (path-without-extension) "./euphrates/path-without-extension.scm"
%use (raisu) "./euphrates/raisu.scm"
%use (time-get-current-unixtime) "./euphrates/time-get-current-unixtime.scm"
%use (current-time/p) "./current-time-p.scm"
%use (filemap-delete-by-recepientid! filemap-ref-by-recepientid) "./filemap.scm"
%use (permission-still-valid?) "./permission-still-valid-huh.scm"
%use (permission-filemap) "./permission.scm"
%use (sharedinfo-ctime sharedinfo-stime) "./sharedinfo.scm"
%use (context-filemap/2 context-sharedir context-tokens) "./web-context.scm"

(define sharedinfo-time-left
  (case-lambda
   ((info)
    (sharedinfo-time-left info (time-get-current-unixtime)))
   ((info current-time)
    (define end (+ (sharedinfo-ctime info)
                   (sharedinfo-stime info)))
    (max 0 (- end current-time)))))

(define sharedinfo-still-valid?
  (case-lambda
   ((info)
    (sharedinfo-still-valid? info (time-get-current-unixtime)))
   ((info current-time)
    (< 0 (sharedinfo-time-left info current-time)))))

(define (web-collectgarbage/nocall ctx)
  (define now (or (current-time/p)
                  (raisu 'current-time-is-not-set)))
  (define sharedir (context-sharedir ctx))
  (define filemap/2 (context-filemap/2 ctx))
  (define tokens (context-tokens ctx))
  (define delayed-list '())
  (define-syntax delayop
    (syntax-rules ()
      ((_ . bodies)
       (set! delayed-list
             (cons (lambda _ . bodies) delayed-list)))))

  (hashmap-foreach
   (lambda (recepientid info)
     (unless (sharedinfo-still-valid? info)
       (delayop
        (display "UNSHARE ") (write recepientid) (newline)
        (filemap-delete-by-recepientid! filemap/2 recepientid))))
   (cdr filemap/2))

  (hashmap-foreach
   (lambda (token perm)
     (if (permission-still-valid? perm now)
         (hashmap-foreach
          (lambda (target-fullpath info)
            (unless (sharedinfo-still-valid? info)
              (delayop
               (display "UNPERM ")
               (write target-fullpath) (newline)
               (hashmap-delete!
                (permission-filemap perm) target-fullpath))))
          (permission-filemap perm))
         (delayop
          (hashmap-delete! tokens token))))
   tokens)

  (for-each (lambda (delayed) (delayed)) delayed-list)

  (for-each
   (lambda (namepair)
     (define full-name (car namepair))
     (define sharedname (cadr namepair))
     (define recepientid (path-without-extension sharedname))
     (define info (filemap-ref-by-recepientid filemap/2 recepientid #f))
     (unless info
       (display "File not shared: ")
       (write sharedname)
       (display " deleting...\n")
       (file-delete full-name)))
   (directory-files sharedir)))
