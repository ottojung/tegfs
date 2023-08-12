;;;; Copyright (C) 2022, 2023  Otto Jung
;;;; This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU Affero General Public License for more details. You should have received a copy of the GNU Affero General Public License along with this program.  If not, see <https://www.gnu.org/licenses/>.

(define (download-to-temporary-file url)
  (log-info "Downloading ~s." url)
  (let* ((target (make-temporary-filename/local))
         ;; NOTE: some websites (looking at you 8chan) require referer to be set to its domain name, which is silly!! and which is stupid >:
         (home (string-append (url-get-protocol url) "://" (url-get-hostname-and-port url)))
         (headers (string-append "referer: " (~s home))))
    (unless (= 0 (system*/exit-code "wget" url "-O" target))
      (unless (= 0 (system*/exit-code "wget" "--header" headers url "-O" target))
        (fatal "Could not download ~s" url)))
    target))
