% Inizializzo contatore
:- dynamic(counter/1).
counter(0).

% Incremento del contatore
increment_counter :-
    retract(counter(Count)),
    CountPlusOne is Count + 1,
    assert(counter(CountPlusOne)).

% Scrivo il file pnml dati place, transition e arc
write_pnml(File, Places, Transitions, Arcs) :-
    open(File, write, Stream),
    write_header(Stream),
    write_places(Stream, Places),
    write_transitions(Stream, Transitions),
    write_arcs(Stream, Arcs),
    write_footer(Stream),
    close(Stream).


% Scrivo intestazione file
write_header(Stream) :-
    write(Stream, '<?xml version="1.0" encoding="UTF-8"?>'), nl(Stream),
    write(Stream, '<pnml>'), nl(Stream),
    write(Stream, '  <net>'), nl(Stream).


% Questo predicato serve quando abbiamo finito tutti i place
write_places(Stream, []) :-
    true.

% Questo predicato serve quando ci sono ancora place da scrivere
write_places(Stream, [Place|Places]) :-
    write(Stream, '    <place id="'),
    counter(Count),
    write(Stream, Count),write(Stream, '">'), nl(Stream),
    increment_counter,
    write(Stream, '      <name>'), write(Stream, Place), write(Stream, '</name>'), nl(Stream),
    write(Stream, '    </place>'), nl(Stream),
    write_places(Stream, Places).

% Questo predicato serve quando abbiamo finito tutte le transition
write_transitions(Stream, []) :-
    true.

% Questo predicato serve quando ci sono ancora transition da scrivere
write_transitions(Stream, [Transition|Transitions]) :-
    write(Stream, '    <transition id="'),
    counter(Count),
    write(Stream, Count),write(Stream, '">'),nl(Stream),
    increment_counter,
    write(Stream, '      <name>'), write(Stream, Transition), write(Stream, '</name>'), nl(Stream),
    write(Stream, '    </transition>'), nl(Stream),
    write_transitions(Stream, Transitions).

% Questo predicato serve quando abbiamo finito tutti gli arc
write_arcs(Stream, []) :-
    true.

% Questo predicato serve quando ci sono ancora arc da scrivere
write_arcs(Stream, [arc(Source, Target)|Arcs]) :-
    write(Stream, '    <arc id="'),
    counter(Count),
    write(Stream, Count), write(Stream, '">'), nl(Stream),
    increment_counter,
    write(Stream, '      <source>'), write(Stream, Source), write(Stream, '</source>'), nl(Stream),
    write(Stream, '      <target>'), write(Stream, Target), write(Stream, '</target>'), nl(Stream),
    write(Stream, '    </arc>'), nl(Stream),
    write_arcs(Stream, Arcs).

% Questo predicato serve quando dobbiamo scrivere il pie di pagina pnml
write_footer(Stream) :-
    write(Stream, '  </net>'), nl(Stream),
    write(Stream, '</pnml>'), nl(Stream).


