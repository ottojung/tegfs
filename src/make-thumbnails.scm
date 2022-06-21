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
%use (file-delete) "./euphrates/file-delete.scm"
%use (dprintln) "./euphrates/dprintln.scm"
%use (file-or-directory-exists?) "./euphrates/file-or-directory-exists-q.scm"
%use (alist->hashmap hashmap->alist) "./euphrates/ihashmap.scm"
%use (list->hashset hashset-ref) "./euphrates/ihashset.scm"
%use (compose-under) "./euphrates/compose-under.scm"
%use (file-size) "./euphrates/file-size.scm"
%use (make-directories) "./euphrates/make-directories.scm"
%use (make-temporary-filename) "./euphrates/make-temporary-filename.scm"
%use (url-goto) "./euphrates/url-goto.scm"

%use (web-preview-width) "./web-preview-width.scm"
%use (web-preview-height) "./web-preview-height.scm"
%use (file-is-image?) "./file-is-image-q.scm"
%use (file-is-video?) "./file-is-video-q.scm"
%use (a-weblink?) "./a-weblink-q.scm"
%use (fatal) "./fatal.scm"

%use (debug) "./euphrates/debug.scm"
%use (debugv) "./euphrates/debugv.scm"

(define (tegfs-make-thumbnails/parse <input> <output>)
  (define ret (tegfs-make-thumbnails <input> <output>))
  (case ret
    ((#f) (fatal "Could not recognize the file type"))
    ((no-web-thumbnails) (fatal "The webpage does not have a thumbnail"))
    (else (raisu 'unexpected-result ret)))
  (dprintln "Done!"))

(define (tegfs-make-thumbnails <input> <output>)
  (cond
   ((a-weblink? <input>)
    (tegfs-make-url-thumbnails <input> <output>))
   ((file-is-image? <input>)
    (tegfs-make-image-thumbnails <input> <output>))
   ((file-is-video? <input>)
    (tegfs-make-video-thumbnails <input> <output>))
   (else #f)))

(define (tegfs-make-url-thumbnails <input> <output>)
  (define temp
    (make-temporary-filename))

  (define _23
    (unless (= 0 (system-fmt "curl --no-progress-meter ~a --output ~a" <input> temp))
      (raisu 'could-not-download-the-webpage <input>)))

  (define-pair (link1/uns status1)
    (system-re "cat ~a | pup 'head meta[property=\"og:image\"] attr{content}'"
               temp))

  (define __1
    (unless (= 0 status1)
      (raisu 'could-not-process-the-page-first-time status1)))

  (define link1 (string-strip link1/uns))

  (define-pair (link2/uns status2)
    (if (string-null? link1)
        (system-re "cat ~a | pup 'head meta[property=\"og:image\"] attr{content}'"
                   temp)
        (cons link1 0)))

  (define __12832
    (unless (= 0 status2)
      (raisu 'could-not-process-the-page-second-time status2)))

  (define link2 (string-strip link2/uns))

  (define _91231
    (file-delete temp))

  (if (string-null? link2) 'no-web-thumbnails
      (let* ((link/full
              (url-goto <input> link2))
             (do (unless (= 0 (system-fmt "curl --no-progress-meter ~a --output ~a" link/full temp))
                   (raisu 'could-not-download-the-preview link/full)))
             (ret (tegfs-make-image-thumbnails temp <output>)))
        (file-delete temp)
        ret)))

(define (tegfs-make-image-thumbnails <input> <output>)
  (let ((dir (dirname <output>)))
    (unless (file-or-directory-exists? dir)
      (make-directories dir)))

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

;; TODO: Maybe do video previews that are videos
;;       Take a look: https://stackoverflow.com/questions/42747935/cut-multiple-videos-and-merge-with-ffmpeg
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

  (define dir (make-temporary-filename))
  (define _1231 (make-directories dir))

  (define n-thumbnails 20)
  (define rate
    (inexact->exact
     (ceiling
      (/ seconds n-thumbnails))))

  (let* ((status
          (system-fmt
           (string-append "echo | ffmpeg -loglevel error -i ~a -vf fps=1/~a ~a/t%04d.png")
           <input> rate dir)))
    (unless (= 0 status)
      (raisu 'ffmpeg-failed status)))

  (let ((dir (dirname <output>)))
    (unless (file-or-directory-exists? dir)
      (make-directories dir)))

  (let ((status
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
           " ~a/*.png"
           " ~a ")
          web-preview-width web-preview-height
          web-preview-width web-preview-height
          dir
          <output>)))
    (system-fmt "rm -rf ~a" dir)
    (unless (= 0 status)
      (raisu 'imagemagick-failed status)))

  #t)
