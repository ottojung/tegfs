;;;; Copyright (C) 2023  Otto Jung
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
  (define-module (tegfs cli-remote)
    :export (CLI::remote/parse CLI::remote)
    :use-module ((euphrates get-command-line-arguments) :select (get-command-line-arguments))
    :use-module ((euphrates shell-quote) :select (shell-quote))
    :use-module ((euphrates words-to-string) :select (words->string))
    :use-module ((tegfs fatal) :select (fatal))
    )))


(define (CLI::remote/parse <remote>)
  (define all-cli-arguments (get-command-line-arguments))
  (define my-cli-arguments (cdr (cdr all-cli-arguments)))
  (CLI::remote <remote> my-cli-arguments))

(define (CLI::remote <remote> args)
  (let ((s (system* "ssh" "-q" "-o" "SendEnv LANG" "-t" <remote>
                    "/bin/sh" "-l" "-c"
                    (shell-quote (words->string (cons "tegfs" (map shell-quote args)))))))
    (unless (= 0 (status:exit-val s))
      (fatal "Something went wrong on the other side"))))
