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
  (define-module (tegfs web-server-handle)
    :export (web::server-handle)
    :use-module ((euphrates hashmap) :select (alist->hashmap hashmap-ref))
    :use-module ((euphrates tilda-a) :select (~a))
    :use-module ((tegfs web-auth) :select (web::auth))
    :use-module ((tegfs web-callcontext-p) :select (web::callcontext/p))
    :use-module ((tegfs web-callcontext) :select (callcontext-path))
    :use-module ((tegfs web-collectgarbage) :select (web::collectgarbage))
    :use-module ((tegfs web-details-svg) :select (web::details.svg))
    :use-module ((tegfs web-details) :select (web::details))
    :use-module ((tegfs web-directory-svg) :select (web::directory.svg))
    :use-module ((tegfs web-directory) :select (web::directory))
    :use-module ((tegfs web-error-svg) :select (web::error.svg))
    :use-module ((tegfs web-filebinary-svg) :select (web::filebinary.svg))
    :use-module ((tegfs web-fileaudio-svg) :select (web::fileaudio.svg))
    :use-module ((tegfs web-filevideo-svg) :select (web::filevideo.svg))
    :use-module ((tegfs web-fileunknown-svg) :select (web::fileunknown.svg))
    :use-module ((tegfs web-filetextual-svg) :select (web::filetextual.svg))
    :use-module ((tegfs web-full) :select (web::full))
    :use-module ((tegfs web-home) :select (web::home))
    :use-module ((tegfs web-icon) :select (web::favicon.ico))
    :use-module ((tegfs web-login) :select (web::login))
    :use-module ((tegfs web-logo-gray) :select (web::logo-gray.jpeg))
    :use-module ((tegfs web-logo-white) :select (web::logo-white.jpeg))
    :use-module ((tegfs web-logout) :select (web::logout))
    :use-module ((tegfs web-main-css) :select (web::main.css))
    :use-module ((tegfs web-previewunknown) :select (web::previewunknown))
    :use-module ((tegfs web-previewunknownurl) :select (web::previewunknownurl))
    :use-module ((tegfs web-query) :select (web::query))
    :use-module ((tegfs web-search) :select (web::search.svg))
    :use-module ((tegfs web-server-handle-temp-path) :select (web::server-handle-temp-path))
    :use-module ((tegfs web-settings-gear) :select (web::settings-gear))
    :use-module ((tegfs web-share-gray-svg) :select (web::share-gray.svg))
    :use-module ((tegfs web-share-svg) :select (web::share.svg))
    :use-module ((tegfs web-share) :select (web::share))
    :use-module ((tegfs web-tags) :select (web::tags))
    :use-module ((tegfs web-upload) :select (web::upload))
    :use-module ((tegfs web-what-svg) :select (web::what.svg)))))



(define handlers-config
  `((/login ,web::login)
    (/logout ,web::logout)
    (/collectgarbage ,web::collectgarbage)
    (/query ,web::query)
    (/directory ,web::directory)
    (/details ,web::details)
    (/full ,web::full)
    (/upload ,web::upload)
    (/share ,web::share)
    (/auth ,web::auth)
    (/tags ,web::tags)
    (/home ,web::home)
    (/ ,web::home)

    (/favicon.ico ,web::favicon.ico)
    (/static/logo-white.jpeg ,web::logo-white.jpeg)
    (/static/logo-gray.jpeg ,web::logo-gray.jpeg)
    (/static/main.css ,web::main.css)
    (/static/previewunknown.svg ,web::previewunknown)
    (/static/previewunknownurl.svg ,web::previewunknownurl)
    (/static/gear.svg ,web::settings-gear)
    (/static/search.svg ,web::search.svg)
    (/static/details.svg ,web::details.svg)
    (/static/share.svg ,web::share.svg)
    (/static/share-gray.svg ,web::share-gray.svg)
    (/static/error.svg ,web::error.svg)
    (/static/what.svg ,web::what.svg)
    (/static/directory.svg ,web::directory.svg)
    (/static/fileunknown.svg ,web::fileunknown.svg)
    (/static/filetextual.svg ,web::filetextual.svg)
    (/static/filebinary.svg ,web::filebinary.svg)
    (/static/filevideo.svg ,web::filevideo.svg)
    (/static/fileaudio.svg ,web::fileaudio.svg)
    ))

(define handlers-funcmap
  (alist->hashmap
   (map
    (lambda (p) (cons (~a (car p)) (cadr p)))
    handlers-config)))

(define (web::server-handle callctx)
  (parameterize ((web::callcontext/p callctx))
    (define path/ma (callcontext-path callctx))
    (define path (if (string-prefix? "/" path/ma) path/ma
                     (string-append "/" path/ma)))
    (define func (hashmap-ref handlers-funcmap path #f))
    (if func (func)
        (web::server-handle-temp-path callctx path))))
