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

%var callcontext-ctr
%var callcontext?
%var callcontext-break
%var callcontext-request
%var callcontext-query
%var callcontext-body
%var callcontext-time
%var callcontext-key
%var set-callcontext-key!
%var callcontext-permissions

%use (define-type9) "./euphrates/define-type9.scm"

(define-type9 <callcontext>
  (callcontext-ctr break request query body time key permissions) callcontext?
  (break callcontext-break) ;; break handler
  (request callcontext-request) ;; client request
  (query callcontext-query) ;; query hashmap
  (body callcontext-body) ;; client body
  (time callcontext-time) ;; timestamp for when request was received
  (key callcontext-key set-callcontext-key!) ;; access key to-set to
  (permissions callcontext-permissions) ;; permissions associated with this call
  )
