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
%use (stringf) "./euphrates/stringf.scm"
%use (with-randomizer-seed) "./euphrates/with-randomizer-seed.scm"
%use (CLI-query) "./CLI-query.scm"
%use (tegfs-add/parse) "./add.scm"
%use (tegfs-categorize/parse) "./categorize.scm"
%use (tegfs-config/parse) "./config.scm"
%use (tegfs-dump-clipboard/parse) "./dump-clipboard.scm"
%use (get-root/default) "./get-root.scm"
%use (tegfs-get/parse) "./get.scm"
%use (keyword-diropen) "./keyword-diropen.scm"
%use (keyword-dirpreview) "./keyword-dirpreview.scm"
%use (tegfs-make-thumbnails/parse) "./make-thumbnails.scm"
%use (tegfs-prolog/parse) "./prolog.scm"
%use (root/p) "./root-p.scm"
%use (tegfs-save/parse) "./save.scm"
%use (tegfs-serve/parse) "./web-server.scm"

(define (main)
  (with-randomizer-seed
   :random
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
       SAVEARGS : --link SAVETARGET
       /          --from-remote <remote-id>
       /          REMOTEOPT? SAVETARGET?
       REMOTEOPT : --remote <remote>
       SAVETARGET : --target <savetext>
       QUERYARGS : QUERYOPT* QUERYQ+
       QUERYOPT : --format <query-format>
       /          --entries
       /          --diropen
       /          --no-diropen
       /          --dirpreview
       /          --no-dirpreview
       QUERYQ : <query...>
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

      :default (--diropen #t)
      :exclusive (--diropen --no-diropen)

      :default (--no-dirpreview #t)
      :exclusive (--no-dirpreview --dirpreview)

      :help (<remote> "A remote address like 'user1@example.com'.")
      :help (--diropen (stringf "Acknowledge <~a> property by treating elements of the ~a directory as entries having the same tags as the original entry." keyword-diropen 'target))
      :help (--dirpreview (stringf "Acknowledge <~a> property by treating elements of the ~a directory as entries having the same tags as the original entry." keyword-dirpreview 'target))

      (when --help
        (define-cli:show-help))

      (parameterize ((root/p <root>))
        (cond
         (add (tegfs-add/parse
               <add-target> <title> <tag...> --series <key...> <value...>
               <registry-file> <date>))
         (save (tegfs-save/parse <remote> --from-remote <remote-id> --link <savetext>))
         (categorize (tegfs-categorize/parse))
         (serve (tegfs-serve/parse))
         (prolog (tegfs-prolog/parse))
         (query (CLI-query --diropen --dirpreview --entries <query-format> <query...>))
         ((and get <getid>) (tegfs-get/parse <get-format> <getid>))
         (make-thumbnails (tegfs-make-thumbnails/parse <target> <output>))
         (config (tegfs-config/parse get set <name> <value>))
         (dump-clipboard (tegfs-dump-clipboard/parse))
         (status
          (display "NOT IMPLEMENTED YET") (newline)
          (exit 1))
         (else
          (display "Impossible") (newline)
          (exit 1))))))))

(main)
