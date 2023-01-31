;;;; Copyright (C) 2022, 2023  Otto Jung
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
  (define-module (tegfs download-to-temporary-file)
    :export (download-to-temporary-file)
    :use-module ((euphrates dprintln) :select (dprintln))
    :use-module ((euphrates system-fmt) :select (system-fmt))
    :use-module ((euphrates tilda-s) :select (~s))
    :use-module ((euphrates url-get-hostname-and-port) :select (url-get-hostname-and-port))
    :use-module ((euphrates url-get-protocol) :select (url-get-protocol))
    :use-module ((tegfs fatal) :select (fatal))
    :use-module ((tegfs make-temporary-filename-local) :select (make-temporary-filename/local))
    )))



(define (download-to-temporary-file url)
  (dprintln "Downloading...")
  (let* ((target (make-temporary-filename/local))
         ;; NOTE: some websites (looking at you 8chan) require referer to be set to its domain name, which is silly!! and which is stupid >:
         (home (string-append (url-get-protocol url) "://" (url-get-hostname-and-port url)))
         (headers (string-append "referer: " (~s home))))
    (unless (= 0 (system-fmt "wget ~a -O ~a" url target))
      (unless (= 0 (system-fmt "wget --header ~a ~a -O ~a" headers url target))
        (fatal "Could not download ~s" url)))
    target))
