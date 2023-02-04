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
  (define-module (tegfs tegfs)
    :use-module ((euphrates current-program-path-p) :select (current-program-path/p))
    :use-module ((euphrates define-cli) :select (define-cli:show-help with-cli))
    :use-module ((euphrates stringf) :select (stringf))
    :use-module ((euphrates with-randomizer-seed) :select (with-randomizer-seed))
    :use-module ((tegfs CLI-query) :select (CLI::query))
    :use-module ((tegfs CLI-talk) :select (CLI::talk))
    :use-module ((tegfs add) :select (tegfs-add/parse))
    :use-module ((tegfs categorize) :select (tegfs-categorize/parse))
    :use-module ((tegfs cli-save) :select (CLI::save))
    :use-module ((tegfs cli-show-license) :select (CLI::show-license))
    :use-module ((tegfs cli-show-warranty) :select (CLI::show-warranty))
    :use-module ((tegfs config) :select (tegfs-config/parse))
    :use-module ((tegfs dump-clipboard) :select (tegfs-dump-clipboard/parse))
    :use-module ((tegfs get-root) :select (get-root/default))
    :use-module ((tegfs get) :select (tegfs-get/parse))
    :use-module ((tegfs keyword-diropen) :select (keyword-diropen))
    :use-module ((tegfs keyword-dirpreview) :select (keyword-dirpreview))
    :use-module ((tegfs keyword-target) :select (keyword-target))
    :use-module ((tegfs make-thumbnails) :select (tegfs-make-thumbnails/parse))
    :use-module ((tegfs prolog) :select (tegfs-prolog/parse))
    :use-module ((tegfs root-p) :select (root/p))
    :use-module ((tegfs tegfs-version) :select (tegfs-version))
    :use-module ((tegfs web-server) :select (tegfs-serve/parse))
    )))


(define (main)
  (with-randomizer-seed
   :random
   (parameterize ((current-program-path/p "tegfs"))
     (with-cli
      (MAIN
       MAIN : ROOT? FUNC
       /      --help
       /      --version
       /      license
       /      warranty

       FUNC : add ADDOPT+
       /      save SAVEARGS
       /      categorize
       /      query QUERYARGS
       /      get GETARGS
       /      serve
       /      talk TALKOPTS*
       /      prolog
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
       QUERYARGS : QUERYOPT* QUERYQ*
       QUERYOPT : --format <query-format>
       /          --entries
       /          --diropen
       /          --no-diropen
       /          --dirpreview
       /          --no-dirpreview
       QUERYQ : <query...>
       GETARGS : GETOPT? <getid>
       GETOPT : --format <get-format> / --entry
       TALKOPTS : --web
       THUMBOPT : <target> <output>
       CONFIGOPT : get <name>
       /           set <name> <value>
       ROOT : --root <root>
       )

      :default (<root> (get-root/default))

      :synonym (--version -v version)
      :synonym (license copying)

      :default (--no-series #t)
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
      :help (--diropen (stringf "Acknowledge <~a> property by treating elements of the ~a directory as entries having the same tags as the original entry." keyword-diropen keyword-target))
      :help (--dirpreview (stringf "Acknowledge <~a> property by treating elements of the ~a directory as entries having the same tags as the original entry." keyword-dirpreview keyword-target))

      (when --help
        (define-cli:show-help))

      (parameterize ((root/p <root>))
        (cond
         (--version (display tegfs-version) (newline))
         (add (tegfs-add/parse
               <add-target> <title> <tag...> --series <key...> <value...>
               <registry-file> <date>))
         (save (CLI::save <remote> --from-remote <remote-id> --link <savetext>))
         (categorize (tegfs-categorize/parse))
         (serve (tegfs-serve/parse))
         (prolog (tegfs-prolog/parse))
         (query (CLI::query --diropen --dirpreview --entries <query-format> <query...>))
         ((and get <getid>) (tegfs-get/parse <get-format> <getid>))
         (talk (CLI::talk --web))
         (make-thumbnails (tegfs-make-thumbnails/parse <target> <output>))
         (config (tegfs-config/parse get set <name> <value>))
         (dump-clipboard (tegfs-dump-clipboard/parse))
         (license (CLI::show-license))
         (warranty (CLI::show-warranty))
         (else
          (display "Impossible") (newline)
          (exit 1))))))))

(main)
