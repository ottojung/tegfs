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

%var web::display-entry

%use (assq-or) "./euphrates/assq-or.scm"
%use (path-get-basename) "./euphrates/path-get-basename.scm"
%use (a-weblink?) "./a-weblink-q.scm"
%use (entry-get-target) "./entry-get-target.scm"
%use (keyword-id) "./keyword-id.scm"
%use (keyword-title) "./keyword-title.scm"
%use (web::get-full-link) "./web-get-full-link.scm"

(define (display-preview target preview-link)
  (define default-preview
    (if (a-weblink? target) "static/previewunknownurl.svg" "static/previewunknown.svg"))

  (display "<img src=")
  (write (or preview-link default-preview))
  (display "/>"))

(define (maybe-display-preview entry maybe-full-senderid preview-link)
  (define target (entry-get-target entry))
  (when target
    (let ((full-link (web::get-full-link entry target maybe-full-senderid)))
      (when full-link
        (display "<a href=") (write full-link) (display ">")
        (display-preview target preview-link)
        (display "</a>")))))

(define (display-actual-title entry)
  (define title (assq-or keyword-title entry #f))

  (cond
   ((and title (not (string-null? title)))
    (display title))
   (else
    (let ()
      (define orig (entry-get-target entry))
      (unless (string-null? orig)
        (let ((relative (if (a-weblink? orig) orig (path-get-basename orig))))
          (display relative)))))))

(define (display-title maybe-full-senderid entry)
  (define id (assq-or keyword-id entry #f))
  (define details-link? (not (not maybe-full-senderid)))

  (when details-link?
    (display "<a href='full?vid=")
    (display maybe-full-senderid)
    (display "'>"))

  (display-actual-title entry)

  (when details-link?
    (display "</a>"))

  (when details-link?
    (display "<a href='details?vid=")
    (display maybe-full-senderid)
    (display "'>")
    (display "<img src='static/details.svg'/>")
    (display "</a>"))

  (when details-link?
    (display "<a href='share?vid=")
    (display maybe-full-senderid)
    (display "'>")
    (display "<img src='static/share.svg'/>")
    (display "</a>"))

  )

(define (web::display-entry entry maybe-full-senderid preview-link)
  (display "<div class='card'>")
  (maybe-display-preview entry maybe-full-senderid preview-link)
  (display "<div id='sub'>")
  (display-title maybe-full-senderid entry)
  (display "</div>")
  (display "</div>")
  )
