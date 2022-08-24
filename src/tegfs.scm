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

%use (current-program-path/p) "./euphrates/current-program-path-p.scm"
%use (define-cli:show-help with-cli) "./euphrates/define-cli.scm"
%use (tegfs-add/parse) "./add.scm"
%use (tegfs-categorize/parse) "./categorize.scm"
%use (tegfs-config/parse) "./config.scm"
%use (tegfs-dump-clipboard/parse) "./dump-clipboard.scm"
%use (get-root/default) "./get-root.scm"
%use (tegfs-get/parse) "./get.scm"
%use (tegfs-list/parse) "./list.scm"
%use (tegfs-make-thumbnails/parse) "./make-thumbnails.scm"
%use (tegfs-prolog/parse) "./prolog.scm"
%use (tegfs-query/parse) "./query.scm"
%use (root/p) "./root-p.scm"
%use (tegfs-save/parse) "./save.scm"
%use (tegfs-serve/parse) "./web-server.scm"

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
      /      list LISTARGS
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
      SAVEARGS : --link SAVETARGET
      /          --from-remote
      /          REMOTEOPT? SAVETARGET?
      REMOTEOPT : --remote <remote>
      SAVETARGET : --target <savetext>
      QUERYARGS : QUERYOPT? QUERYQ+
      QUERYOPT : --format <query-format> / --entries
      QUERYQ : <query...>
      LISTARGS : LISTDIRSQ? LISTOPT?
      /          LISTOPT? LISTDIRSQ?
      LISTDIRSQ : --depth <listdepth>
      LISTOPT : --format <list-format> / --entries
      GETARGS : GETOPT? <getid>
      GETOPT : --format <get-format> / --entry
      THUMBOPT : <target> <output>
      CONFIGOPT : get <name>
      /           set <name> <value>
      ROOT : --root <root>
      )

     :default (<root> (get-root/default))
     :default (--no-series #f)
     :exclusive (--no-series --series)
     :default (--entries #t)
     :exclusive (--entries --format)
     :default (--entry #t)
     :exclusive (--entry --format)
     :type (<listdepth> 'number)
     :help (<remote> "A remote address like 'user1@example.com'")

     (when --help
       (define-cli:show-help))

     (parameterize ((root/p <root>))
       (cond
        (add (tegfs-add/parse
              <add-target> <title> <tag...> --series <key...> <value...>
              <registry-file> <date>))
        (save (tegfs-save/parse <remote> --from-remote --link <savetext>))
        (categorize (tegfs-categorize/parse))
        (serve (tegfs-serve/parse))
        (prolog (tegfs-prolog/parse))
        (query (tegfs-query/parse --entries <query-format> <query...>))
        (list (tegfs-list/parse <listdepth> --entries <list-format>))
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
