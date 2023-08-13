;;;; Copyright (C) 2023  Otto Jung
;;;; This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU Affero General Public License for more details. You should have received a copy of the GNU Affero General Public License along with this program.  If not, see <https://www.gnu.org/licenses/>.

(define prolog-additional-static-predicates
  "
v(_, _) :- false.
t('%diff', [X, Y]) :- X \\= Y.
t('%any', _).
vandthis(This, X) :- X = This ; v(This, X).
%% what(X, Y) :- t(Y, X) ; t(K, Z), member(X, Z), Y = [K | Z].

build_conjunction([H], X, R, (i(X, R), t(H, X))).
build_conjunction([H|T], X, R, (t(H, X), ConjunctionT)) :-
    build_conjunction(T, X, R, ConjunctionT).

eval_query(Argv, R) :-
    build_conjunction(Argv, _X, R, Conjunction),
    call(Conjunction).

eval_query_print(Argv) :-
    eval_query(Argv, R),
    write(R), nl,
    false.
eval_query_print(_).

main :-
    current_prolog_flag(argv, Argv),
    eval_query_print(Argv),
    halt.
")
