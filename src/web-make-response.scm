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

%var web-make-response

%use (web-basic-headers) "./web-basic-headers.scm"

%for (COMPILER "guile")

(use-modules (web response)
             (sxml simple))

%end

(define* (web-make-response #:optional body #:key
                            (status 200)
                            (title #f)
                            (extra-heads '())
                            (doctype "<!DOCTYPE html>\n")
                            (content-type-params '((charset . "utf-8")))
                            (content-type 'text/html)
                            (extra-headers '()))
  (values
   (build-response
    #:code status
    ;; most of these settings come from here: https://cheatsheetseries.owasp.org/cheatsheets/HTTP_Headers_Cheat_Sheet.html
    #:headers
    (append web-basic-headers
            `((content-type . (,content-type ,@content-type-params))
              (Cache-Control . "no-cache")
              ,@extra-headers)))
   (lambda (port)
     (parameterize ((current-output-port port))
       (when doctype (display doctype))
       (display "<html>\n")
       (display "<head>\n")
       (when title
         (display "  <title>")
         (display title)
         (display "</title>\n"))
       (display "  <link rel='stylesheet' href='/main.css'>")
       (for-each display extra-heads)
       (display "</head>\n")
       (display "<body>\n")
       (if (string? body)
           (display body)
           (sxml->xml body port))
       (display "\n</body>\n")
       (display "</html>\n")))))
