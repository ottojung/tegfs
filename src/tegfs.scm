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

%use (with-cli define-cli:show-help) "./euphrates/define-cli.scm"
%use (system-environment-get) "./euphrates/system-environment.scm"
%use (make-directories) "./euphrates/make-directories.scm"
%use (read-all-port) "./euphrates/read-all-port.scm"
%use (dprintln) "./euphrates/dprintln.scm"
%use (current-program-path/p) "./euphrates/current-program-path-p.scm"
%use (file-or-directory-exists?) "./euphrates/file-or-directory-exists-q.scm"
%use (write-string-file) "./euphrates/write-string-file.scm"
%use (append-string-file) "./euphrates/append-string-file.scm"
%use (list-zip) "./euphrates/list-zip.scm"
%use (system-re) "./euphrates/system-re.scm"
%use (string-strip) "./euphrates/string-strip.scm"
%use (list-intersperse) "./euphrates/list-intersperse.scm"

%use (tegfs-add/parse) "./add.scm"
%use (tegfs-save/parse) "./save.scm"
%use (tegfs-categorize/parse) "./categorize.scm"
%use (tegfs-serve/parse) "./server.scm"
%use (fatal) "./fatal.scm"
%use (root/p) "./root-p.scm"

(define (main)
  (define ROOT_VAR_NAME "TEGFS_ROOT")

  (parameterize ((current-program-path/p "tegfs"))
    (with-cli
     (MAIN
      MAIN : ROOT* FUNC
      /      --help
      FUNC : add ADDOPT+
      /      save
      /      categorize
      /      status
      /      serve
      ADDOPT : --target <add-target>
      /        --title <title>
      /        --tag <tag...>
      /        --series
      /        --no-series
      /        --key <key...> <value...>
      /        --registry-file <registry-file>
      /        --date <date>
      ROOT : --root <root>
      )

     :default (<root> (system-environment-get ROOT_VAR_NAME))
     :default (--no-series #f)
     :exclusive (--no-series --series)

     (when --help
       (define-cli:show-help))

     (unless <root>
       (fatal "Root is unknown because $~a env variable is not defined" ROOT_VAR_NAME))

     (unless (file-or-directory-exists? <root>)
       (make-directories <root>))

     (parameterize ((root/p <root>))
       (cond
        (add (tegfs-add/parse
              <add-target> <title> <tag...> --series <key...> <value...>
              <registry-file> <date>))
        (save (tegfs-save/parse))
        (categorize (tegfs-categorize/parse))
        (serve (tegfs-serve/parse))
        (status
         (display "NOT IMPLEMENTED YET") (newline)
         (exit 1))
        (else
         (display "Impossible") (newline)
         (exit 1)))))))

(main)
