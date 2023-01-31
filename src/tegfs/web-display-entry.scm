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
  (define-module (tegfs web-display-entry)
    :export (web::display-entry)
    :use-module ((euphrates assq-or) :select (assq-or))
    :use-module ((euphrates file-is-directory-q-no-readlink) :select (file-is-directory?/no-readlink))
    :use-module ((euphrates file-or-directory-exists-q) :select (file-or-directory-exists?))
    :use-module ((euphrates path-get-basename) :select (path-get-basename))
    :use-module ((tegfs a-weblink-q) :select (a-weblink?))
    :use-module ((tegfs entry-get-mimetype) :select (entry-get-mimetype))
    :use-module ((tegfs entry-get-target) :select (entry-get-target))
    :use-module ((tegfs entry-target-fullpath) :select (entry-target-fullpath))
    :use-module ((tegfs file-is-audio-q) :select (file-is-audio?))
    :use-module ((tegfs file-is-image-q) :select (file-is-image?))
    :use-module ((tegfs file-is-text-q) :select (file-is-text?))
    :use-module ((tegfs file-is-video-q) :select (file-is-video?))
    :use-module ((tegfs keyword-id) :select (keyword-id))
    :use-module ((tegfs keyword-title) :select (keyword-title))
    :use-module ((tegfs web-get-full-link) :select (web::get-full-link))
    )))

(define (get-preview-by-mimetype entry)
  (define mimetype (entry-get-mimetype entry))
  (and mimetype
       (or
        (and (equal? mimetype "text/uri-list") "static/previewunknownurl.svg")
        (and (equal? mimetype "inode/directory") "static/directory.svg")
        (and (string-prefix? "image/" mimetype) "static/fileimage.svg")
        (and (string-prefix? "audio/" mimetype) "static/fileaudio.svg")
        (and (string-prefix? "video/" mimetype) "static/filevideo.svg")
        (and (string-prefix? "text/" mimetype) "static/filetextual.svg")
        (and mimetype "static/filebinary.svg"))))

(define (get-preview-by-filename target)
  (and target
       (or
        (and (a-weblink? target) "static/previewunknownurl.svg")
        (and (file-is-image? target) "static/fileimage.svg")
        (and (file-is-audio? target) "static/fileaudio.svg")
        (and (file-is-video? target) "static/filevideo.svg")
        (and (file-is-text? target) "static/filetextual.svg"))))

(define (get-preview-by-contents entry)
  (define target-fullpath (entry-target-fullpath entry))
  (and target-fullpath
       (file-or-directory-exists? target-fullpath)
       (if (file-is-directory?/no-readlink target-fullpath)
           "static/directory.svg"
           "static/fileunknown.svg")))

(define (display-preview entry target preview-link)
  (display "<img src=")
  (write
   (or preview-link
       (get-preview-by-mimetype entry)
       (get-preview-by-filename target)
       (get-preview-by-contents entry)
       (if target
           "static/fileunknown.svg"
           "static/previewunknown.svg")))
  (display "/>"))

(define (maybe-display-preview entry maybe-full-senderid preview-link)
  (define target (entry-get-target entry))
  (when target
    (let ((full-link (web::get-full-link entry target maybe-full-senderid)))
      (when full-link
        (display "<a href=") (write full-link) (display ">")
        (display-preview entry target preview-link)
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
    (display "<img src='static/details.svg' title='Details'/>")
    (display "</a>"))

  (when details-link?
    (display "<a href='share?vid=")
    (display maybe-full-senderid)
    (display "'>")
    (display "<img src='static/share.svg' title='Share'/>")
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
