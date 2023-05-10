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
    :use-module ((tegfs web-get-preview-link) :select (web::get-preview-link))
    )))

(define (get-preview-by-mimetype mimetype)
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

(define (display-preview entry mimetype target preview-linkpath)
  (define preview-link
    (and preview-linkpath (web::get-preview-link preview-linkpath)))
  (display "<img src=")
  (write
   (or preview-link
       (get-preview-by-mimetype mimetype)
       (get-preview-by-filename target)
       (get-preview-by-contents entry)
       (if target
           "static/fileunknown.svg"
           "static/previewunknown.svg")))
  (display "/>"))

(define (maybe-display-preview entry mimetype maybe-full-senderid preview-linkpath)
  (define target (entry-get-target entry))
  (let ((full-link (web::get-full-link entry target maybe-full-senderid)))
    (when full-link
      (display "<a href=") (write full-link)
      (unless (equal? mimetype "inode/directory")
        (display " target='_blank'"))
      (display ">"))
    (display-preview entry mimetype target preview-linkpath)
    (when full-link
      (display "</a>"))))

(define (display-actual-title entry)
  (define title (assq-or keyword-title entry #f))

  (cond
   ((and title (not (string-null? title)))
    (display title))
   (else
    (let ()
      (define orig (entry-get-target entry))
      (unless (or (not orig) (string-null? orig))
        (let ((relative (if (a-weblink? orig) orig (path-get-basename orig))))
          (display relative)))))))

(define (display-title entry selectable? id maybe-full-senderid)
  (define (display-query)
    (cond
     (maybe-full-senderid
      (display "vid=")
      (display maybe-full-senderid))
     (id
      (display "id=")
      (display id))))

  (define (display-select-flag)
    (display "<input")
    (display " name=") (write (string-append (if maybe-full-senderid "s" "i") ":" (or maybe-full-senderid id)))
    (display " type='checkbox' ")
    (display "/>"))

  (when selectable? (display-select-flag))
  (if maybe-full-senderid
      (display "<a href='full?")
      (display "<a href='details?"))
  (display-query)
  (display "'>")
  (display-actual-title entry)
  (display "</a>")

  (display "<a href='details?")
  (display-query)
  (display "'>")
  (display "<img src='static/details.svg' title='Details'/>")
  (display "</a>")

  (when maybe-full-senderid
    (display "<a href='share?vid=")
    (display maybe-full-senderid)
    (display "'>")
    (display "<img src='static/share.svg' title='Share'/>")
    (display "</a>")))

(define (web::display-entry entry selectable? maybe-full-senderid preview-linkpath)
  (define mimetype (entry-get-mimetype entry))
  (define id (assq-or keyword-id entry #f))

  (display "<label class='card'")
  (when selectable?
    (display " for=")
    (write (string-append "select:" (or maybe-full-senderid id))))
  (display ">")

  (maybe-display-preview entry mimetype maybe-full-senderid preview-linkpath)
  (display "<div id='sub'>")
  (display-title entry selectable? id maybe-full-senderid)
  (display "</div>")
  (display "</label>")
  )
