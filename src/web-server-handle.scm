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

%var web::server-handle

%use (alist->hashmap hashmap-ref) "./euphrates/hashmap.scm"
%use (~a) "./euphrates/tilda-a.scm"
%use (web::auth) "./web-auth.scm"
%use (web::authcont) "./web-authcont.scm"
%use (web::callcontext/p) "./web-callcontext-p.scm"
%use (callcontext-path) "./web-callcontext.scm"
%use (web::collectgarbage) "./web-collectgarbage.scm"
%use (web::details) "./web-details.scm"
%use (web::directory) "./web-directory.scm"
%use (web::full) "./web-full.scm"
%use (web::keygate) "./web-keygate.scm"
%use (web::login) "./web-login.scm"
%use (web::logincont) "./web-logincont.scm"
%use (web::main.css) "./web-main-css.scm"
%use (web::previewunknown) "./web-previewunknown.scm"
%use (web::previewunknownurl) "./web-previewunknownurl.scm"
%use (web::query) "./web-query.scm"
%use (web::server-handle-temp-path) "./web-server-handle-temp-path.scm"
%use (web::share) "./web-share.scm"
%use (web::upload) "./web-upload.scm"
%use (web::uploadcont) "./web-uploadcont.scm"

(define handlers-config
  `((/login ,web::login)
    (/logincont ,web::logincont)
    (/collectgarbage ,web::collectgarbage)
    (/query ,web::query)
    (/directory ,web::directory)
    (/details ,web::details)
    (/full ,web::full)
    (/upload ,web::upload)
    (/uploadcont ,web::uploadcont)
    (/share ,web::share)
    (/auth ,web::auth)
    (/authcont ,web::authcont)
    (/keygate ,web::keygate)

    (/static/main.css ,web::main.css)
    (/static/previewunknown.svg ,web::previewunknown)
    (/static/previewunknownurl.svg ,web::previewunknownurl)
    ))

(define handlers-funcmap
  (alist->hashmap
   (map
    (lambda (p) (cons (~a (car p)) (cadr p)))
    handlers-config)))

(define (web::server-handle callctx)
  (parameterize ((web::callcontext/p callctx))
    (define path (callcontext-path callctx))
    (define func (hashmap-ref handlers-funcmap path #f))
    (if func (func)
        (web::server-handle-temp-path callctx path))))
