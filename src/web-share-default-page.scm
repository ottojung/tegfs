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

%var web::share::get-default-text

%use (hashmap-copy hashmap-set!) "./euphrates/hashmap.scm"
%use (stringf) "./euphrates/stringf.scm"
%use (callcontext-query) "./web-callcontext.scm"
%use (web::form-template) "./web-form-template.scm"
%use (web::get-domainname) "./web-get-domainname.scm"
%use (web::hashmap->query) "./web-hashmap-to-query.scm"

(define web::share::inside-template
  "
          <div class='tiled-v-element split-container with-separator'>
            <div class='form-block split-left'>
              <label for='username'>Default link</label>
              <input class='copytext' readonly autofocus onfocus='this.select()' value='~a' type='text'/>
            </div>
            <div class='split-right'>
              <div class='form-block'>
                <label for='username'>Protected link</label>
                <input class='copytext' readonly value='~a' type='text'/>
              </div>
              <div class='form-block'>
                <label for='password'>Password</label>
                <input class='copytext' readonly value='~a' type='text'/>
              </div>
            </div>
          </div>
")

(define web::share::outside-template
  "
      <br/>
      <div class='form-block tiled-v-element'>
          Links expire in ~a (<a href='~a'>configure</a>)
      </div>
")

(define (web::share::get-default-text callctx share-time unprotected-link0 protected-link0 password)
  (define domainname (web::get-domainname callctx))
  (define (get-link url0)
    (string-append domainname url0))

  (define unprotected-link
    (get-link unprotected-link0))
  (define protected-link
    (get-link protected-link0))

  (define insides
    (stringf web::share::inside-template
             unprotected-link protected-link password))

  (define formatted-share-time
    (cond
     ((<= (* 24 60 60 60) share-time)
      (let ((div (inexact->exact (quotient share-time (* 24 60 60)))))
        (case div
          ((1) (stringf "1 day"))
          (else (stringf "~a days" div)))))
     ((<= (* 60 60) share-time)
      (let ((div (inexact->exact (quotient share-time (* 60 60)))))
        (case div
          ((1) (stringf "1 hour"))
          (else (stringf "~a hours" div)))))
     ((<= (* 1 60) share-time)
      (let ((div (inexact->exact (quotient share-time (* 1 60)))))
        (case div
          ((1) (stringf "1 minute"))
          (else (stringf "~a minutes" div)))))
     (else
      (let ((div (inexact->exact (floor share-time))))
        (case div
          ((1) (stringf "1 minute"))
          (else (stringf "~a minutes" div)))))))

  (define settings-query
    (let ((original (hashmap-copy (callcontext-query callctx))))
      (hashmap-set! original 'settings "yes")
      original))
  (define settings-link
    (string-append
     "/share?"
     (web::hashmap->query settings-query)))
  (define outsides
    (stringf web::share::outside-template
             formatted-share-time
             settings-link))

  (web::form-template #f insides outsides))