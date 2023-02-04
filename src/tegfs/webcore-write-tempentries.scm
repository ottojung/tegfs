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
  (define-module (tegfs webcore-write-tempentries)
    :export (webcore::write-tempentries)
    :use-module ((euphrates append-posix-path) :select (append-posix-path))
    :use-module ((euphrates hashmap) :select (hashmap-foreach))
    :use-module ((euphrates make-directories) :select (make-directories))
    :use-module ((euphrates path-get-dirname) :select (path-get-dirname))
    :use-module ((euphrates profun-accept) :select (profun-accept))
    :use-module ((euphrates profun-error) :select (make-profun-error))
    :use-module ((euphrates profun-op-lambda) :select (profun-op-lambda))
    :use-module ((tegfs default-tempentries-path) :select (default-tempentries-path))
    :use-module ((tegfs entry-print) :select (entry-print))
    :use-module ((tegfs get-root) :select (get-root))
    :use-module ((tegfs webcore-access) :select (can-manage-tempentries?))
    :use-module ((tegfs webcore-context) :select (context-tempentries))
    :use-module ((tegfs webcore-get-current-permissions) :select (webcore::get-current-permissions))
    :use-module ((tegfs webcore-serialize-tempentry) :select (webcore::serialize-tempentry))
    )))

(define (webcore::write-tempentries webcore::context)
  (define tempentries (context-tempentries webcore::context))
  (profun-op-lambda
   :with-env env
   (ctx () nonames)

   (define perm (webcore::get-current-permissions))

   (cond
    ((not (can-manage-tempentries? perm))
     (make-profun-error 'permission-denied "This user cannot manage tempentries"))
    (else
     (let ()
       (define tempentries-fullpath
         (append-posix-path (get-root) default-tempentries-path))
       (define d (path-get-dirname tempentries-fullpath))
       (define p
         (begin
           (make-directories d)
           (open-file tempentries-fullpath "w")))

       (parameterize ((current-output-port p))
         (hashmap-foreach
          (lambda (key val)
            (define s (webcore::serialize-tempentry key val))
            (entry-print s)
            (newline) (newline))
          tempentries))

       (close-port p)
       (profun-accept))))))
