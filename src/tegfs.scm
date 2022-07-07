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
%use (tegfs-serve/parse) "./web-server.scm"
%use (tegfs-prolog/parse) "./prolog.scm"
%use (tegfs-query/parse) "./query.scm"
%use (tegfs-get/parse) "./get.scm"
%use (tegfs-make-thumbnails/parse) "./make-thumbnails.scm"
%use (tegfs-config/parse) "./config.scm"
%use (tegfs-dump-clipboard/parse) "./dump-clipboard.scm"
%use (fatal) "./fatal.scm"
%use (root/p) "./root-p.scm"
%use (ROOT_VAR_NAME) "./get-root.scm"

(define (main)
  (parameterize ((current-program-path/p "tegfs"))
    (with-cli
     (MAIN
      MAIN : ROOT? FUNC
      /      --help
      FUNC : add ADDOPT+
      /      save SAVEARGS
      /      categorize
      /      prolog
      /      query QUERYARGS
      /      get GETARGS
      /      status
      /      serve
      /      make-thumbnails THUMBOPT
      /      config CONFIGOPT
      /      dump-clipboard
      ADDOPT : --target <add-target>
      /        --title <title>
      /        --tag <tag...>
      /        --series
      /        --no-series
      /        --key <key...> <value...>
      /        --registry-file <registry-file>
      /        --date <date>
      SAVEARGS : SAVEOPT? SAVETARGET?
      SAVEOPT : --remote <remote>
      SAVETARGET : <savetext>
      QUERYARGS : QUERYOPT? QUERYQ+
      QUERYOPT : --format <query-format> / --entries
      QUERYQ : <query...>
      GETARGS : GETOPT? <getid>
      GETOPT : --format <get-format> / --entry
      THUMBOPT : <target> <output>
      CONFIGOPT : get <name>
      /           set <name> <value>
      ROOT : --root <root>
      )

     :default (<root> (system-environment-get ROOT_VAR_NAME))
     :default (--no-series #f)
     :exclusive (--no-series --series)
     :default (--entries #t)
     :exclusive (--entries --format)
     :default (--entry #t)
     :exclusive (--entry --format)
     :help (<remote> "A remote address like 'user1@example.com'")

     (when --help
       (define-cli:show-help))

     (when <root>
       (unless (file-or-directory-exists? <root>)
         (make-directories <root>)))

     (parameterize ((root/p <root>))
       (cond
        (add (tegfs-add/parse
              <add-target> <title> <tag...> --series <key...> <value...>
              <registry-file> <date>))
        (save (tegfs-save/parse <remote> <savetext>))
        (categorize (tegfs-categorize/parse))
        (serve (tegfs-serve/parse))
        (prolog (tegfs-prolog/parse))
        (query (tegfs-query/parse --entries <query-format> <query...>))
        ((and get <getid>) (tegfs-get/parse <get-format> <getid>))
        (make-thumbnails (tegfs-make-thumbnails/parse <target> <output>))
        (config (tegfs-config/parse get set <name> <value>))
        (dump-clipboard (tegfs-dump-clipboard/parse))
        (status
         (display "NOT IMPLEMENTED YET") (newline)
         (exit 1))
        (else
         (display "Impossible") (newline)
         (exit 1)))))))

(main)
