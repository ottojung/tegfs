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

%var sharedinfo-ctr
%var sharedinfo?
%var sharedinfo-sourcepath
%var sharedinfo-recepientid
%var sharedinfo-vid
%var sharedinfo-ctime
%var sharedinfo-stime

%use (define-type9) "./euphrates/define-type9.scm"

(define-type9 <sharedinfo>
  (sharedinfo-ctr sourcepath recepientid vid ctime stime) sharedinfo?
  (sourcepath sharedinfo-sourcepath) ;; the original file path
  (recepientid sharedinfo-recepientid) ;; the linked file path suffix (without the sharedir)
  (vid sharedinfo-vid) ;; unique virtual id
  (ctime sharedinfo-ctime) ;; time in seconds for when this info was created
  (stime sharedinfo-stime) ;; time in seconds for how long to share this file
  )
