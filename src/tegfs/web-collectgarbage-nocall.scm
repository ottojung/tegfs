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

(cond-expand
 (guile
  (define-module (tegfs web-collectgarbage-nocall)
    :export (web::collectgarbage/nocall)
    :use-module ((euphrates directory-files) :select (directory-files))
    :use-module ((euphrates file-delete) :select (file-delete))
    :use-module ((euphrates hashmap) :select (hashmap-delete! hashmap-foreach))
    :use-module ((euphrates path-without-extension) :select (path-without-extension))
    :use-module ((euphrates raisu) :select (raisu))
    :use-module ((tegfs current-time-p) :select (current-time/p))
    :use-module ((tegfs custom-tempentry-still-valid-huh) :select (custom-tempentry-still-valid?))
    :use-module ((tegfs custom-tempentry) :select (custom-tempentry?))
    :use-module ((tegfs filemap) :select (filemap-delete-by-recepientid! filemap-ref-by-recepientid))
    :use-module ((tegfs log-info) :select (log-info))
    :use-module ((tegfs permission-still-valid-huh) :select (permission-still-valid?))
    :use-module ((tegfs permission) :select (permission-filemap permission?))
    :use-module ((tegfs sharedinfo) :select (sharedinfo-date sharedinfo-recepientid sharedinfo-stime sharedinfo?))
    :use-module ((tegfs sharereceipt) :select (sharereceipt?))
    :use-module ((tegfs webcore-context) :select (context-filemap/2 context-sharedir context-tempentries))
    )))



(define sharedinfo-time-left
  (case-lambda
   ((info)
    (sharedinfo-time-left info (current-time/p)))
   ((info current-time)
    (define end (+ (sharedinfo-date info)
                   (sharedinfo-stime info)))
    (max 0 (- end current-time)))))

(define sharedinfo-still-valid?
  (case-lambda
   ((info)
    (sharedinfo-still-valid? info (current-time/p)))
   ((info current-time)
    (< 0 (sharedinfo-time-left info current-time)))))

(define (web::collectgarbage/nocall ctx)
  (define now (or (current-time/p)
                  (raisu 'current-time-is-not-set)))
  (define sharedir (context-sharedir ctx))
  (define tempentries (context-tempentries ctx))
  (define filemap/2 (context-filemap/2 ctx))
  (define delayed-list '())
  (define-syntax delayop
    (syntax-rules ()
      ((_ . bodies)
       (set! delayed-list
             (cons (lambda _ . bodies) delayed-list)))))

  (hashmap-foreach
   (lambda (token tempentry)
     (cond
      ((sharedinfo? tempentry)
       (let* ((info tempentry)
              (recepientid (sharedinfo-recepientid info)))
         (unless (sharedinfo-still-valid? info)
           (delayop
            (log-info "UNSHARE ~s." recepientid)
            (filemap-delete-by-recepientid! filemap/2 recepientid)))))
      ((sharereceipt? tempentry) 0) ;; should be deleted by the above line
      ((permission? tempentry)
       (let ((perm tempentry))
         (if (permission-still-valid? perm now)
             (hashmap-foreach
              (lambda (target-fullpath info)
                (unless (sharedinfo-still-valid? info)
                  (delayop
                   (log-info "UNPERM ~s." target-fullpath)
                   (hashmap-delete!
                    (permission-filemap perm) target-fullpath))))
              (permission-filemap perm))
             (delayop
              (hashmap-delete! tempentries token)))))
      ((custom-tempentry? tempentry)
       (unless (custom-tempentry-still-valid? tempentry now)
         (delayop (hashmap-delete! tempentries token))))
      (else (log-warning "Unknown object in gc ~s." tempentry))))
   tempentries)

  (for-each (lambda (delayed) (delayed)) delayed-list)

  (for-each
   (lambda (namepair)
     (define full-name (car namepair))
     (define sharedname (cadr namepair))
     (define recepientid (path-without-extension sharedname))
     (define info (filemap-ref-by-recepientid filemap/2 recepientid #f))
     (unless info
       (log-warning "File not shared: ~s, deleting it." sharedname)
       (file-delete full-name)))
   (directory-files #t sharedir)))
