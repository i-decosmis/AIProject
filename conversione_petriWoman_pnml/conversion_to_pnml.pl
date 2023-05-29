% Dichiaro dinamiche le liste per poter essere modificate a runtime
:- dynamic string_list1/1.
:- dynamic string_list2/1.
:- dynamic string_list3/1.

% Inizializzo contatore usato nel salvataggio dei dati nel file pnml
:- dynamic(counter/1).
counter(0).

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

% Processa il file in input ed esporta il file pnml nuovo
process_file(FileName) :-
    retractall(string_list(1, List)),
    retractall(string_list(2, List)),
    retractall(string_list(3, List)),
    open(FileName, read, Stream),  % Apre il file per la lettura
    process_lines(Stream),        % Inizia ad analizzare le linee
    close(Stream),                % Chiude il file
    get_strings(1, Places),
    get_strings(2, Transitions),
    get_strings(3, Arcs),
    ask_folder_path(Path),
    write_pnml(Path, Places, Transitions, Arcs).

% Chiedo dove salvare il file pnml
ask_folder_path(String) :-
    write('Enter path where to save file: '),
    read_line_to_string(user_input, String).

% Caso in cui lo Stream ha raggiunto la fine del file
process_lines(Stream) :-
    at_end_of_stream(Stream).

% Dato lo stream in input lo analizzo e richiamo la funzione con il nuovo Stream
process_lines(Stream) :-
    \+ at_end_of_stream(Stream),
    read_line_to_string(Stream, Line),
    process_line(Line),
    process_lines(Stream).

% Leggo la linea in input e lo converto in atomo per poter capire se place, transition o arc
process_line(Line) :-
    atom_string(Term, Line),
    process_term(Term).

% Caso in cui leggo una riga place
process_term(Term) :-
    sub_atom(Term, _, _, _, 'place'),
    read_places(Term).

% Caso in cui leggo una rica transition
process_term(Term) :-
    sub_atom(Term, _, _, _, 'transition'),
    read_transition(Term).

% Caso in cui leggo una riga arc
process_term(Term) :-
    sub_atom(Term, _, _, _, 'arc'),
    read_arc(Term).

% Leggo places dal Term in input e lo metto nella seconda lista
read_places(Term) :-
    extract_info(Term, Info),
    add_string(1, Info).

% Leggo transition dal Term in input e lo metto nella seconda lista
read_transition(Term) :-
    extract_info(Term, Info),
    add_string(2, Info).

% Leggo arco dal Term in input e lo metto nella terza lista
read_arc(Term) :-
    extract_arc(Term, Info),
    add_string(3, Info).

% Estraggo il contenuto tra parentesi e lo restituisco come Info
extract_info(Line, Info) :-
    sub_string(Line, Before, _, After, '('),
    sub_string(Line, Before, _, 0, InfoWithParenthesis),
    sub_string(InfoWithParenthesis, 1, _, 1, Info).


% Estraggo arco dalla linea in input e lo restituisco come Info
extract_arc(Line, Info) :-
    sub_string(Line, Before, _, After, '('),
    sub_string(Line, BeforeComma, _, AfterComma, ','),
    sub_string(Line, Before, _, AfterComma, FirstInfoDirty),
    sub_string(FirstInfoDirty, 1, _, 1, FirstInfo),
    sub_string(Line, BeforeComma, _, 0, SecondInfoDirty),
    sub_string(SecondInfoDirty, 1, _, 1, SecondInfo),
    Info = (FirstInfo, SecondInfo).

% Funzione per stampare la lista in input
print_list([]).                  % Base case: an empty list
print_list([Head|Tail]) :-       % Recursive case: non-empty list
    writeln(Head),               % Output the head of the list
    print_list(Tail). 





% Inizializzo contatore per gli id dei place, delle transition e degli arc nel file pnml
increment_counter :-
    retract(counter(Count)),
    CountPlusOne is Count + 1,
    assert(counter(CountPlusOne)).

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
    write(StreamWrite, '<pnml>'), nl(StreamWrite),
    write(StreamWrite, '  <net>'), nl(StreamWrite).


% Questo predicato serve quando abbiamo finito tutti i place
write_places(StreamWrite, []) :-
    true.

% Questo predicato serve quando ci sono ancora place da scrivere
write_places(StreamWrite, [Place|Places]) :-
    write(StreamWrite, '    <place id="'),
    counter(Count),
    write(StreamWrite, Count),write(StreamWrite, '">'), nl(StreamWrite),
    increment_counter,
    write(StreamWrite, '      <name>'), write(StreamWrite, Place), write(StreamWrite, '</name>'), nl(StreamWrite),
    write(StreamWrite, '    </place>'), nl(StreamWrite),
    write_places(StreamWrite, Places).

% Questo predicato serve quando abbiamo finito tutte le transition
write_transitions(StreamWrite, []) :-
    true.

% Questo predicato serve quando ci sono ancora transition da scrivere
write_transitions(StreamWrite, [Transition|Transitions]) :-
    write(StreamWrite, '    <transition id="'),
    counter(Count),
    write(StreamWrite, Count),write(StreamWrite, '">'),nl(StreamWrite),
    increment_counter,
    write(StreamWrite, '      <name>'), write(StreamWrite, Transition), write(StreamWrite, '</name>'), nl(StreamWrite),
    write(StreamWrite, '    </transition>'), nl(StreamWrite),
    write_transitions(StreamWrite, Transitions).

% Questo predicato serve quando abbiamo finito tutti gli arc
write_arcs(StreamWrite, []) :-
    true.

% Questo predicato serve quando ci sono ancora arc da scrivere
write_arcs(StreamWrite, [(Source, Target)|Arcs]) :-
    write(StreamWrite, '    <arc id="'),
    counter(Count),
    write(StreamWrite, Count), write(StreamWrite, '">'), nl(StreamWrite),
    increment_counter,
    write(StreamWrite, '      <source>'), write(StreamWrite, Source), write(StreamWrite, '</source>'), nl(StreamWrite),
    write(StreamWrite, '      <target>'), write(StreamWrite, Target), write(StreamWrite, '</target>'), nl(StreamWrite),
    write(StreamWrite, '    </arc>'), nl(StreamWrite),
    write_arcs(StreamWrite, Arcs).

% Questo predicato serve quando dobbiamo scrivere il pie di pagina pnml
write_footer(StreamWrite) :-
    write(StreamWrite, '  </net>'), nl(StreamWrite),
    write(StreamWrite, '</pnml>'), nl(StreamWrite).

