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
  (define-module (tegfs web-file)
    :export (web::file)
    :use-module ((euphrates append-posix-path) :select (append-posix-path))
    :use-module ((euphrates file-or-directory-exists-q) :select (file-or-directory-exists?))
    :use-module ((euphrates fp) :select (fp))
    :use-module ((euphrates hashmap) :select (hashmap-ref))
    :use-module ((euphrates open-file-port) :select (open-file-port))
    :use-module ((tegfs web-callcontext-p) :select (web::callcontext/p))
    :use-module ((tegfs web-callcontext) :select (callcontext-query))
    :use-module ((tegfs web-current-fileserver-p) :select (web::current-fileserver/p))
    :use-module ((tegfs web-current-sharedir-p) :select (web::current-sharedir/p))
    :use-module ((tegfs web-not-found) :select (web::not-found))
    :use-module ((tegfs web-respond-with-a-file) :select (web::respond-with-a-file))
    )))

(cond-expand
 (guile
  (use-modules ((ice-9 binary-ports) :select (get-bytevector-all)))))

(define (web::file)
  (define callctx (web::callcontext/p))
  (define q (callcontext-query callctx))
  (define path (hashmap-ref q 'path #f))
  (define fileserver (web::current-fileserver/p))

  (if (and path
           (not (string? fileserver))
           (not (string-index path #\/)))
      (let ()
        (define sharedir (web::current-sharedir/p))
        (define fullpath (append-posix-path sharedir path))
        (define mimetype '(octet-stream)) ;; TODO: make more specific
        (define bv
          (and (file-or-directory-exists? fullpath)
               (let ((fp (open-file-port fullpath "r")))
                 (define r (get-bytevector-all fp))
                 (close-port fp)
                 r)))
        (if bv
            (web::respond-with-a-file '(octet-stream) bv)
            (web::not-found)))
      (web::not-found)))
