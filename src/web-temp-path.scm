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

%run guile

%var web::temp-path-make
%var web::temp-path?
%var web::temp-path-tempid
%var web::temp-path-destination
%var web::temp-path-start
%var web::temp-path-stime
%var web::temp-path-public

%use (define-type9) "./euphrates/define-type9.scm"

(define-type9 <web::temp-path>
  (web::temp-path-make tempid destination/raw start stime) web::temp-path?
  (tempid web::temp-path-tempid)
  (destination/raw web::temp-path-destination/raw)
  (start web::temp-path-start) ;; when this was created
  (stime web::temp-path-stime) ;; for how long this should live
  )

(define (web::temp-path-destination tpath)
  (define raw (web::temp-path-destination/raw tpath))
  (if (procedure? raw)
      ((raw))
      raw))

(define (web::temp-path-public tpath)
  (string-append "/" (web::temp-path-tempid tpath)))
