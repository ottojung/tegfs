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

(cond-expand
 (guile
  (define-module (tegfs sharedinfo)
    :export (sharedinfo-ctr sharedinfo? sharedinfo-entry sharedinfo-sourcepath sharedinfo-recepientid sharedinfo-senderid sharedinfo-date sharedinfo-stime set-sharedinfo-stime!)
    :use-module ((euphrates define-type9) :select (define-type9))
    )))



(define-type9 <sharedinfo>
  (sharedinfo-ctr entry sourcepath recepientid senderid date stime) sharedinfo?
  (entry sharedinfo-entry) ;; the relative entry
  (sourcepath sharedinfo-sourcepath) ;; the original file path
  (recepientid sharedinfo-recepientid) ;; uniquer virtual id visible to the recepient (so, potentially everyone). It is the linked file path suffix (without the sharedir)
  (senderid sharedinfo-senderid) ;; unique virtual id only visible to the sender.
  (date sharedinfo-date) ;; time in seconds for when this info was created
  (stime sharedinfo-stime set-sharedinfo-stime!) ;; time in seconds for how long to share this file
  )
