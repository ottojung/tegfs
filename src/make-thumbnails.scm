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

%var tegfs-make-thumbnails/parse
%var tegfs-make-thumbnails
%var tegfs-make-image-thumbnails
%var tegfs-make-video-thumbnails

%use (system-re) "./euphrates/system-re.scm"
%use (system-fmt) "./euphrates/system-fmt.scm"
%use (define-pair) "./euphrates/define-pair.scm"
%use (raisu) "./euphrates/raisu.scm"
%use (string->seconds/columned) "./euphrates/string-to-seconds-columned.scm"
%use (appcomp comp) "./euphrates/comp.scm"
%use (string->lines) "./euphrates/string-to-lines.scm"
%use (string->words) "./euphrates/string-to-words.scm"
%use (words->string) "./euphrates/words-to-string.scm"
%use (range) "./euphrates/range.scm"
%use (string-strip) "./euphrates/string-strip.scm"
%use (~a) "./euphrates/tilda-a.scm"
%use (make-temporary-filename) "./euphrates/make-temporary-filename.scm"
%use (file-delete) "./euphrates/file-delete.scm"
%use (dprintln) "./euphrates/dprintln.scm"
%use (file-or-directory-exists?) "./euphrates/file-or-directory-exists-q.scm"

%use (web-preview-width) "./web-preview-width.scm"
%use (web-preview-height) "./web-preview-height.scm"
%use (file-is-image?) "./file-is-image-q.scm"
%use (file-is-video?) "./file-is-video-q.scm"
%use (fatal) "./fatal.scm"

%use (debug) "./euphrates/debug.scm"
%use (debugv) "./euphrates/debugv.scm"

(define (tegfs-make-thumbnails/parse <input> <output>)
  (unless (tegfs-make-thumbnails <input> <output>)
    (fatal "Could not recognize the file type"))
  (dprintln "Done!"))

(define (tegfs-make-thumbnails <input> <output>)
  (cond
   ((file-is-image? <input>)
    (tegfs-make-image-thumbnails <input> <output>))
   ((file-is-video? <input>)
    (tegfs-make-video-thumbnails <input> <output>))
   (else #f)))

(define (tegfs-make-image-thumbnails <input> <output>)
  (system-fmt
   (string-append
    "convert "
    " ~a "
    " -thumbnail ~a@ "
    " -quality 10 "
    " -gravity center "
    " -background transparent "
    " -extent ~ax~a "
    " ~a "
    )
   <input>
   (* web-preview-width web-preview-height)
   web-preview-width web-preview-height
   <output>)
  #t)

(define tegfs-n-thumbnails 20)

(define (tegfs-make-video-thumbnails <input> <output>)
  (define-pair (probe status) (system-re "ffprobe ~a 2>&1" <input>))
  (define _121312
    (unless (= 0 status) (raisu 'probe-failed status probe)))

  (define seconds
    (appcomp probe
             string->lines
             (map string-strip)
             (filter (comp (string-prefix? "Duration: ")))
             car
             string->words
             cadr
             ((lambda (s) (string-strip s ",")))
             string->seconds/columned))

  (define screenshots
    (map
     (lambda _
       (string-append (make-temporary-filename) ".png"))
     (range tegfs-n-thumbnails)))

  (let loop ((screenshots screenshots))
    (define ss
      (exact->inexact
       (* (length screenshots)
          (/ seconds tegfs-n-thumbnails))))

    (unless (null? screenshots)
      (let* ((shot (car screenshots))
             (status
              (system-fmt
               (string-append
                "echo | "
                " ffmpeg "
                " -loglevel fatal "
                " -ss ~a "
                " -i ~a "
                " -vf select='eq(pict_type\\,I)' "
                " -vframes 1 "
                " ~a "
                )
               ss <input> shot)))
        (unless (= 0 status)
          (raisu 'ffmpeg-failed status)))
      (loop (cdr screenshots))))

  (let* ((created (filter file-or-directory-exists? screenshots))
         (status
          (system-fmt
           (string-append
            "convert "
            " -coalesce +dither "
            " -layers optimize "
            " -delay 100 "
            " -loop 0 "
            " -thumbnail '~ax~a>' "
            " -gravity center "
            " -background transparent "
            " -extent ~ax~a "
            (words->string (reverse created))
            " "
            <output>)
           web-preview-width web-preview-height
           web-preview-width web-preview-height)))
    (unless (= 0 status)
      (raisu 'imagemagick-failed status))
    (for-each file-delete created))

  #t)
