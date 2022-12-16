;;;; Copyright (C) 2022  Otto Jung
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

%var web-display-entry

%use (assq-or) "./euphrates/assq-or.scm"
%use (path-get-basename) "./euphrates/path-get-basename.scm"
%use (uri-encode) "./euphrates/uri-encode.scm"
%use (a-weblink?) "./a-weblink-q.scm"
%use (keyword-id) "./keyword-id.scm"
%use (keyword-target) "./keyword-target.scm"
%use (web-get-full-link) "./web-get-full-link.scm"

;; (define (display-preview/old target-fullpath)
;;   (define ctx (web-context/p))
;;   (define perm (web-get-permissions))
;;   (define fileserver (context-fileserver ctx))
;;   (define preview-fullpath (get-preview-path target-fullpath))
;;   (define default-preview
;;     (if (a-weblink? target-fullpath) "/previewunknownurl" "/previewunknown"))

;;   (display "<img src=")
;;   (unless
;;       (and preview-fullpath
;;            (let ((info (web-share-file ctx perm preview-fullpath default-preview-sharing-time)))
;;              (and info
;;                   (let* ((recepientid (sharedinfo-recepientid info))
;;                          (sharedir (context-sharedir ctx))
;;                          (shared-fullpath (web-get-shared-fullpath sharedir preview-fullpath recepientid))
;;                          (location (web-get-shared-link fileserver preview-fullpath recepientid)))
;;                     (if (file-or-directory-exists? shared-fullpath)
;;                         (write location)
;;                         (write default-preview))
;;                     #t))))
;;     (write default-preview))
;;   (display "/>"))

;; (define (maybe-display-preview/old entry)
;;   (define target-fullpath (entry-target-fullpath entry))
;;   (when target-fullpath
;;     (let* ((full-link (web-get-full-link entry target-fullpath)))
;;       (when full-link
;;         (display "<a href=") (write full-link) (display ">")
;;         (display-preview/old target-fullpath)
;;         (display "</a>")))))

(define (display-preview target preview-link)
  (define default-preview
    (if (a-weblink? target) "/previewunknownurl" "/previewunknown"))

  (display "<img src=")
  (write (or preview-link default-preview))
  (display "/>"))

(define (maybe-display-preview entry maybe-full-senderid preview-link)
  (define target (assq-or keyword-target entry #f))
  (when target
    (let ((full-link (web-get-full-link entry target maybe-full-senderid)))
      (when full-link
        (display "<a href=") (write full-link) (display ">")
        (display-preview target preview-link)
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

(define (web-display-entry entry maybe-full-senderid preview-link)
  (display "<div class='card'>")
  (display "<div>")
  (maybe-display-preview entry maybe-full-senderid preview-link)
  (display "</div>")
  (display "<div>")
  (display-title entry)
  (display "</div>")
  (display "</div>")
  )
