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

%var web-make-html-response

%use (lines->string) "./euphrates/lines-to-string.scm"
%use (raisu) "./euphrates/raisu.scm"
%use (current-time/p) "./current-time-p.scm"
%use (web-basic-headers) "./web-basic-headers.scm"
%use (web-callcontext/p) "./web-callcontext-p.scm"
%use (callcontext-break callcontext-key) "./web-callcontext.scm"
%use (web-context/p) "./web-context-p.scm"
%use (web-get-permissions) "./web-get-permissions.scm"
%use (web-set-cookie-header) "./web-set-cookie-header.scm"

%for (COMPILER "guile")

(use-modules (web response)
             (sxml simple))

%end

(define web-og-headers
  (lines->string
   (list
    "  <meta name='viewport' content='width=device-width, initial-scale=1'>"
    "  <meta name='apple-mobile-web-app-title' content='TegFS'>"
    "  <meta name='application-name' content='TegFS'>"
    "  <meta property='og:image' content='https://www.publicdomainpictures.net/pictures/300000/velka/lighthouse-1562112203vxk.jpg'>"
    "  <meta property='og:site_name' content='TegFS'>"
    "  <meta property='og:type' content='website'>"
    "  <meta property='og:title' content='TegFS'>"
    "  <meta property='og:description' content='TegFS is a file sharing server.'>"
    ""
    )))

(define* (web-make-html-response
          body #:key
          (status 200)
          (title "TegFS")
          (extra-heads '())
          (doctype "<!DOCTYPE html>\n")
          (content-type-params '((charset . "utf-8")))
          (content-type 'text/html)
          (extra-headers '()))
  (define ctx (web-context/p))
  (define callctx (web-callcontext/p))
  (define now (current-time/p))
  (define cont (callcontext-break callctx))
  (define _perm (web-get-permissions))
  (define key (callcontext-key callctx))
  (define key-headers
    (if key (list (web-set-cookie-header "key" key)) '()))

  (cont
   (build-response
    #:code status
    ;; most of these settings come from here: https://cheatsheetseries.owasp.org/cheatsheets/HTTP_Headers_Cheat_Sheet.html
    #:headers
    (append web-basic-headers
            `((content-type . (,content-type ,@content-type-params))
              (Cache-Control . "no-cache")
              ,@key-headers
              ,@extra-headers)))
   (lambda (port)
     (parameterize ((current-output-port port))
       (when doctype (display doctype))
       (display "<html>\n")
       (display "<head>\n")
       (display web-og-headers)
       (when title
         (display "  <title>")
         (display title)
         (display "</title>\n"))
       (display "  <link rel='stylesheet' href='/main.css'>")
       (for-each display extra-heads)
       (display "</head>\n")
       (display "<body>\n")
       (cond
        ((string? body) (display body))
        ((pair? body) (sxml->xml body port))
        ((procedure? body)
         (parameterize ((web-callcontext/p callctx)
                        (web-context/p ctx)
                        (current-time/p now))
           (body)))
        (else (raisu 'unknown-body-type body)))
       (display "\n</body>\n")
       (display "</html>\n")))))
