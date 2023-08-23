:-style_check(-discontiguous).
t('%local', 1).
t(document, 1).
t(science, 1).
i(1, "x9cnod4am0ubytw0l4qiromrtjr2qb").
t('%local', 2).
t(object, "v2X").
t(photo, 2).
t(man, "v2X").
t(with, ["v2X", 2]).
t(image, 2).
v(2, "v2X").
i(2, "uyo0i1j9digl7376fbn8lced3phg8x").
t('%notarget', 3).
t(object, "v3X").
t(photo, 3).
t(woman, "v3X").
t(with, ["v3X", 3]).
t(image, 3).
v(3, "v3X").
i(3, "2ovnot1gxrvwbqjialqvloysewezr2").
t('%local', 4).
t(photo, 4).
t(image, 4).
i(4, "izn3ej3g7m0w8ayec7y5k90csjnwo8").
t('%local', 5).
t(photo, 5).
t(image, 5).
i(5, "64jchwxre1u52tue1uj3nu8jw9mq8s").
t('%local', 6).
t(photo, 6).
t(image, 6).
i(6, "ibbp3h89eik1eezsfwkjtclomf3o82").
t('%local', 7).
t(art, 7).
t(drawing, 7).
t(image, 7).
i(7, "v5ifddhy85eg40llyqt0mruim3s6p3").
t('%local', 8).
t(video, 8).
i(8, "y4r3m9069ftoro5z27lhtuht35hhlg").
t('%local', 9).
t(video, 9).
i(9, "gqci3tp4qk0czsgr5ezp4u1y431807").
t('%local', 10).
t(video, 10).
i(10, "4pggw03qn32y0dj3zz39hf9rj9cxlu").
t('%local', 11).
t(image, "v11X").
t(photo, "v11X").
t('collection-of', ["v11X", 11]).
v(11, "v11X").
i(11, "i12k87lof1hxwvyyn5u09gyzv9j33v").
t('%local', 12).
t(song, 12).
t(audio, 12).
i(12, "8qka96sjlzzvnnrc6q23bua8bzxycv").
t('%local', 13).
t(recording, 13).
t(audio, 13).
i(13, "34h9wdllz6ialr02piifswp11xaid0").
t('%local', 14).
t(song, 14).
t(audio, 14).
i(14, "ns1b5nlgc2wpoxrdv0605mcfsxmodz").
t('%local', 15).
t(document, 15).
i(15, "t5t6ekwa5h1n7o1zlbx8fig65ih4vv").
t('%remote', 16).
t(website, 16).
t(document, 16).
i(16, "876gdsvrln3q6xu4gyp4nsbb39st1r").
t('%remote', 17).
t(lecture, 17).
t(video, 17).
i(17, "chh8i2lsn77kwfsyuht8pdgeq66wwv").
t('%local', 18).
t(book, 18).
t(text, 18).
i(18, "wkpg7o5u2z6e6po3xycq46unsa89fm").
t('%local', 19).
t(pasta, 19).
t(text, 19).
i(19, "10n2ldy9mb1fqzpn24lxnzxstibmds").
tag('%remote').
tag(song).
tag('%notarget').
tag(website).
tag(with).
tag(object).
tag(man).
tag('%local').
tag(recording).
tag(woman).
tag('collection-of').
tag(video).
tag(drawing).
tag(book).
tag(text).
tag(pasta).
tag(lecture).
tag(image).
tag(science).
tag(audio).
tag(art).
tag(photo).
tag(document).
t(clip, This) :- t(song, This), t(video, This).

id(X, ID) :- i(X, ID).
id(NAME, ID) :- i(X, ID), v(X, NAME).

v(_, _) :- false.

t('%diff', [X, Y]) :- X \= Y.
t('%any', _).

vandthis(This, X) :- X = This ; v(This, X).
%% what(X, Y) :- t(Y, X) ; t(K, Z), member(X, Z), Y = [K | Z].

build_conjunction([H], X, R, (t(H, X), id(X, R))).
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
