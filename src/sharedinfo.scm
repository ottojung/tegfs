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

%var sharedinfo-ctr
%var sharedinfo?
%var sharedinfo-entry
%var sharedinfo-sourcepath
%var sharedinfo-recepientid
%var sharedinfo-senderid
%var sharedinfo-ctime
%var sharedinfo-stime

%use (define-type9) "./euphrates/define-type9.scm"

(define-type9 <sharedinfo>
  (sharedinfo-ctr entry sourcepath recepientid senderid ctime stime) sharedinfo?
  (entry sharedinfo-entry) ;; the relative entry
  (sourcepath sharedinfo-sourcepath) ;; the original file path
  (recepientid sharedinfo-recepientid) ;; uniquer virtual id visible to the recepient (so, potentially everyone). It is the linked file path suffix (without the sharedir)
  (senderid sharedinfo-senderid) ;; unique virtual id only visible to the sender.
  (ctime sharedinfo-ctime) ;; time in seconds for when this info was created
  (stime sharedinfo-stime) ;; time in seconds for how long to share this file
  )
