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

%use (catchu-case) "./euphrates/catchu-case.scm"
%use (appcomp comp) "./euphrates/comp.scm"
%use (define-pair) "./euphrates/define-pair.scm"
%use (dprintln) "./euphrates/dprintln.scm"
%use (file-delete) "./euphrates/file-delete.scm"
%use (file-or-directory-exists?) "./euphrates/file-or-directory-exists-q.scm"
%use (make-directories) "./euphrates/make-directories.scm"
%use (make-temporary-filename) "./euphrates/make-temporary-filename.scm"
%use (raisu) "./euphrates/raisu.scm"
%use (string-strip) "./euphrates/string-strip.scm"
%use (string->lines) "./euphrates/string-to-lines.scm"
%use (string->seconds/columned) "./euphrates/string-to-seconds-columned.scm"
%use (string->words) "./euphrates/string-to-words.scm"
%use (system-fmt) "./euphrates/system-fmt.scm"
%use (system-re) "./euphrates/system-re.scm"
%use (url-goto) "./euphrates/url-goto.scm"
%use (a-weblink?) "./a-weblink-q.scm"
%use (fatal) "./fatal.scm"
%use (file-is-image?) "./file-is-image-q.scm"
%use (file-is-video?) "./file-is-video-q.scm"
%use (web-preview-height) "./web-preview-height.scm"
%use (web-preview-width) "./web-preview-width.scm"

(define (tegfs-make-thumbnails/parse <input> <output>)
  (catchu-case
   (tegfs-make-thumbnails <input> <output>)

   (('could-not-recognize-filetype)
    (fatal "Could not recognize the file type"))
   (('no-web-thumbnails)
    (fatal "The webpage does not have a thumbnail"))
   (('could-not-convert-image)
    (fatal "Image conversion failed"))
   (('could-not-download-the-webpage input)
    (fatal "Could not download the webpage"))
   (('could-not-download-the-preview input)
    (fatal "Could not download the preview"))
   (('probe-failed status probe)
    (fatal "FFMpeg probe failed with status: ~s" status))
   (('ffmpeg-failed status)
    (fatal "FFMpeg failed with status: ~s" status))
   (('imagemagick-failed status)
    (fatal "ImageMagick failed with status: ~s" status))

   )

  (dprintln "Done!"))

(define (tegfs-make-thumbnails <input> <output>)
  (cond
   ((a-weblink? <input>)
    (tegfs-make-url-thumbnails <input> <output>))
   ((file-is-image? <input>)
    (tegfs-make-image-thumbnails <input> <output>))
   ((file-is-video? <input>)
    (tegfs-make-video-thumbnails <input> <output>))
   (else
    (raisu 'could-not-recognize-filetype))))

(define (tegfs-make-url-thumbnails <input> <output>)
  (define temp
    (make-temporary-filename))

  (define _23
    (unless (= 0 (system-fmt "wget --no-verbose ~a -O ~a" <input> temp))
      (raisu 'could-not-download-the-webpage <input>)))

  (define-pair (link1/uns status1)
    (system-re "cat ~a | pup 'head meta[property=\"og:image\"] attr{content}'"
               temp))

  (define link1
    (string-strip
     (car
      (string->lines link1/uns))))

  (define-pair (link2/uns status2)
    (if (or (string-null? link1)
            (not (= 0 status1)))
        (system-re "cat ~a | pup 'head meta[property=\"og:image\"] attr{content}'"
                   temp)
        (cons link1 0)))

  (define __12832
    (unless (or (= 0 status2) (= 0 status1))
      (dprintln "Could not process the link the second time: ~a and ~a" status1 status2)))

  (define link2
    (string-strip
     (car
      (string->lines
       link2/uns))))

  (define _91231
    (file-delete temp))

  (when (string-null? link2)
    (raisu 'no-web-thumbnails))

  (let* ((link/full
          (url-goto <input> link2))
         (do (unless (= 0 (system-fmt "wget --no-verbose ~a -O ~a" link/full temp))
               (raisu 'could-not-download-the-preview link/full)))
         (ret (tegfs-make-image-thumbnails temp <output>)))
    (file-delete temp)
    ret))

(define (tegfs-make-image-thumbnails <input> <output>)
  (let ((dir (dirname <output>)))
    (unless (file-or-directory-exists? dir)
      (make-directories dir)))

  (or (= 0
         (system-fmt
          (string-append
           "convert "
           " -limit memory 32mb "
           " ~a[0] "
           " -thumbnail ~a@ "
           " -quality 30 "
           " -gravity center "
           " -background transparent "
           " -extent ~ax~a "
           " ~a "
           )
          <input>
          (* web-preview-width web-preview-height)
          web-preview-width web-preview-height
          <output>))
      'could-not-convert-image))

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
           " -limit memory 32mb "
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
