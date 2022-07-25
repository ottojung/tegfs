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

%var file-is-audio?

(define (file-is-audio? target-fullpath)
  (define small (string-downcase target-fullpath))
  (or (string-suffix? ".mp3" small)
      (string-suffix? ".ogg" small)
      (string-suffix? ".wav" small)
      (string-suffix? ".m4a" small)
      (string-suffix? ".mp4a" small)
      (string-suffix? ".midi" small)))
