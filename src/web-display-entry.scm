;;;; Copyright (C) 2022  Otto Jung
;;;;
;;;; This program is free software: you can redistribute it and/or modify
;;;; it under the terms of the GNU General Public License as published by
;;;; the Free Software Foundation; version 3 of the License.
;;;;
;;;; This program is distributed in the hope that it will be useful,
;;;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;;;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;;;; GNU General Public License for more details.
;;;;
;;;; You should have received a copy of the GNU General Public License
;;;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

%run guile

%var web-display-entry

%use (append-posix-path) "./euphrates/append-posix-path.scm"
%use (file-or-directory-exists?) "./euphrates/file-or-directory-exists-q.scm"
%use (path-get-basename) "./euphrates/path-get-basename.scm"
%use (uri-encode) "./euphrates/uri-encode.scm"
%use (a-weblink?) "./a-weblink-q.scm"
%use (default-preview-sharing-time) "./default-preview-sharing-time.scm"
%use (entry-target-fullpath) "./entry-target-fullpath.scm"
%use (get-preview-path) "./get-preview-path.scm"
%use (keyword-id) "./keyword-id.scm"
%use (keyword-target) "./keyword-target.scm"
%use (sharedinfo-sharedname) "./sharedinfo.scm"
%use (web-context/p) "./web-context-p.scm"
%use (context-fileserver context-sharedir) "./web-context.scm"
%use (web-get-full-link) "./web-get-full-link.scm"
%use (web-share-file) "./web-share-file.scm"

(define (display-preview target-fullpath)
  (define ctx (web-context/p))
  (define fileserver (context-fileserver ctx))
  (define preview-fullpath (get-preview-path target-fullpath))
  (define default-preview
    (if (a-weblink? target-fullpath) "/previewunknownurl" "/previewunknown"))

  (display "<img src=")
  (unless
      (and preview-fullpath
           (let ((info (web-share-file preview-fullpath default-preview-sharing-time)))
             (and info
                  (let* ((sharedname (sharedinfo-sharedname info))
                         (sharedir (context-sharedir ctx))
                         (shared-fullpath (append-posix-path sharedir sharedname))
                         (location (string-append fileserver sharedname)))
                    (if (file-or-directory-exists? shared-fullpath)
                        (write location)
                        (write default-preview))
                    #t))))
    (write default-preview))
  (display "/>"))

(define (maybe-display-preview entry)
  (define target-fullpath (entry-target-fullpath entry))
  (when target-fullpath
    (let* ((full-link (web-get-full-link entry target-fullpath)))
      (when full-link
        (display "<a href=") (write full-link) (display ">")
        (display-preview target-fullpath)
        (display "</a>")))))

(define (display-title entry)
  (define details-link?
    (not (not (assoc keyword-id entry))))

  (when details-link?
    (display "<a href='/details?id=")
    (display (uri-encode (cdr (assoc keyword-id entry))))
    (display "' style='color: white'>"))

  (cond
   ((and (assoc 'title entry)
         (not (string-null? (cdr (assoc 'title entry)))))
    (display (cdr (assoc 'title entry))))
   ((and (assoc keyword-target entry)
         (not (string-null? (cdr (assoc keyword-target entry)))))
    (let* ((orig (cdr (assoc keyword-target entry)))
           (relative (if (a-weblink? orig) orig (path-get-basename orig))))
      (display relative)))
   (else
    (display (cdr (assoc keyword-id entry)))))

  (when details-link?
    (display "</a>"))
  )

(define (web-display-entry entry)
  (display "<div class='card'>")
  (display "<div>")
  (maybe-display-preview entry)
  (display "</div>")
  (display "<div>")
  (display-title entry)
  (display "</div>")
  (display "</div>")
  )
