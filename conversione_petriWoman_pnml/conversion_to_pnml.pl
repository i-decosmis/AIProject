% Dichiaro dinamiche le liste per poter essere modificate a runtime
:- dynamic string_list1/1.
:- dynamic string_list2/1.
:- dynamic string_list3/1.

% Aggiunge stringa nella lista in input (1,2 o 3)
add_string(ListNumber, String) :-
    (   retract(string_list(ListNumber, List))
    ->  assertz(string_list(ListNumber, [String|List]))
    ;   assertz(string_list(ListNumber, [String]))
    ).

% Restituisce la lista in base al numero in input (1,2 o 3)
get_strings(ListNumber, Strings) :-
    (   string_list(ListNumber, Strings)
    ->  true
    ;   Strings = []
    ).

% Inizializzo contatore usato nel salvataggio dei dati nel file pnml
:- dynamic(counter/1).
counter(0).

% Inizializzo contatore per gli id dei place, delle transition e degli arc nel file pnml
increment_counter :-
    retract(counter(Count)),
    CountPlusOne is Count + 1,
    assert(counter(CountPlusOne)).

% Processa il file in input ed esporta il file pnml nuovo
process_file(FileName) :-
    retractall(string_list(1, List)),
    retractall(string_list(2, List)),
    retractall(string_list(3, List)),
    consult(FileName),
    process_net,        % Inizia ad analizzare la rete
    get_strings(1, Places),
    get_strings(2, Transitions),
    get_strings(3, Arcs),
    ask_folder_path(Path),
    write_pnml(Path, Places, Transitions, Arcs).

% Chiedo dove salvare il file pnml
ask_folder_path(String) :-
    write('Enter path where to save output file(e.g. C:/User/Desktop): '),
    read_line_to_string(user_input, Read),
    atom_concat(Read, "/petri_net.pnml", String).

% Estraggo tutti i place in una lista
extract_places(Places) :-
    findall(Place, place(Place), Places).

% Estraggo tutte le transitioni in una lista
extract_transitions(Transitions) :-
    findall(Transition, transition(Transition), Transitions).

% Estraggo tutti gli archi in una lista
extract_arcs(Arcs) :-
    findall(arc(T, P, Token), arc(T, P, Token), Arcs).

% Predicato che ci permette di copiare una lista in una delle 3 liste dinamiche dato indice in input
copy_list(IdList, []) :-
    true.

copy_list(IdList, [H | T]) :-
    add_string(IdList, H),
    copy_list(IdList, T).

% Estraggo le informazioni dai fatti
process_net :-
    extract_places(NewPlaces),
    copy_list(1, NewPlaces),
    extract_transitions(NewTransitions),
    copy_list(2, NewTransitions),
    extract_arcs(NewArcs),
    copy_list(3, NewArcs).

% Scrivo il file pnml dati place, transition e arc
write_pnml(File, Places, Transitions, Arcs) :-
    open(File, write, StreamWrite),
    write_header(StreamWrite),
    write_places(StreamWrite, Places),
    write_transitions(StreamWrite, Transitions),
    write_arcs(StreamWrite, Arcs),
    write_footer(StreamWrite),
    close(StreamWrite).

% Scrivo intestazione file
write_header(StreamWrite) :-
    write(StreamWrite, '<?xml version="1.0" encoding="UTF-8"?>'), nl(StreamWrite),
    write(StreamWrite, '<pnml xmlns="http://www.pnml.org/version-2009/grammar/pnml">'), nl(StreamWrite),
    write(StreamWrite, '  <net id="net1" type="http://www.pnml.org/version-2009/grammar/ptnet">'), nl(StreamWrite),
    write(StreamWrite, '    <name>'), nl(StreamWrite),
    write(StreamWrite, '        <text> Petri Net </text>'), nl(StreamWrite),
    write(StreamWrite, '    </name>'), nl(StreamWrite),
    write(StreamWrite, '    <page id="page1">'), nl(StreamWrite).


% Questo predicato serve quando abbiamo finito tutti i place
write_places(StreamWrite, []) :-
    true.

% Questo predicato serve quando ci sono ancora place da scrivere
write_places(StreamWrite, [Place|Places]) :-
    write(StreamWrite, '        <place id="'),
    counter(Count),
    write(StreamWrite, Count),write(StreamWrite, '">'), nl(StreamWrite),
    increment_counter,
    write(StreamWrite, '            <name>'), 
    nl(StreamWrite),
    write(StreamWrite, '                <text>'), 
    write(StreamWrite, Place), write(StreamWrite, '</text>'), nl(StreamWrite),
    write(StreamWrite, '            </name>'), nl(StreamWrite),
    write(StreamWrite, '        </place>'), nl(StreamWrite),
    write_places(StreamWrite, Places).

% Questo predicato serve quando abbiamo finito tutte le transition
write_transitions(StreamWrite, []) :-
    true.

% Questo predicato serve quando ci sono ancora transition da scrivere
write_transitions(StreamWrite, [Transition|Transitions]) :-
    write(StreamWrite, '        <transition id="'),
    counter(Count),
    write(StreamWrite, Count),write(StreamWrite, '">'), nl(StreamWrite),
    increment_counter,
    write(StreamWrite, '            <name>'), 
    nl(StreamWrite),
    write(StreamWrite, '                <text>'), 
    write(StreamWrite, Transition), write(StreamWrite, '</text>'), nl(StreamWrite),
    write(StreamWrite, '            </name>'), nl(StreamWrite),
    write(StreamWrite, '        </transition>'), nl(StreamWrite),
    write_transitions(StreamWrite, Transitions).

% Questo predicato serve quando abbiamo finito tutti gli arc
write_arcs(StreamWrite, []) :-
    true.

% Questo predicato serve quando ci sono ancora arc da scrivere
write_arcs(StreamWrite, [arc(Source, Target, Token)|Arcs]) :-
    write(StreamWrite, '        <arc id="'),
    counter(Count),
    write(StreamWrite, Count),
    write(StreamWrite, '" source="'),
    write(StreamWrite, Source),
    write(StreamWrite, '" target="'),
    write(StreamWrite, Target),
    write(StreamWrite, '">'), nl(StreamWrite),
    increment_counter,
    write(StreamWrite, '            <inscription>'),nl(StreamWrite),
    write(StreamWrite, '                <text>'),
    write(StreamWrite, Token),
    write(StreamWrite, '</text>'),
    nl(StreamWrite),
    write(StreamWrite, '            </inscription>'),nl(StreamWrite),
    write(StreamWrite, '        </arc>'), nl(StreamWrite),
    write_arcs(StreamWrite, Arcs).

% Questo predicato serve quando dobbiamo scrivere il pie di pagina pnml
write_footer(StreamWrite) :-
    write(StreamWrite, '    </page>'), nl(StreamWrite),
    write(StreamWrite, '  </net>'), nl(StreamWrite),
    write(StreamWrite, '</pnml>'), nl(StreamWrite).