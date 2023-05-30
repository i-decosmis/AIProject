% Initialize an empty list
init_list :-
    retractall(transition_list(_)),
    assertz(transition_list([])).

% Add an element to the list
add_to_list(Element) :-
    retract(transition_list(List)),
    assertz(transition_list([Element|List])).

% Remove an element from the list
remove_from_list(Element) :-
    retract(transition_list(List)),
    delete(List, Element, NewList),
    assertz(transition_list(NewList)).

% Retrieve the current state of the list
get_list(List) :-
    transition_list(List).

% Processa il file in input ed esporta il file pnml nuovo
process_file(FileName) :-
    init_list,
    open(FileName, read, Stream),  % Apre il file per la lettura
    process_lines(Stream),        % Inizia ad analizzare le linee
    close(Stream),                % Chiude il file
    get_list(List),
    writeln(List),
    print_transition(List).

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
    sub_atom(Term, _, _, _, 'transition(['),
    read_transition(Term).

% Caso in cui leggo una riga place
process_term(Term) :-
    !,
    true.

% Leggo places dal Term in input e lo metto nella seconda lista
read_transition(Term) :-
    extract_info(Term).

sum(X, Y, Result) :- Result is X + Y.

% Estraggo il contenuto tra parentesi e lo restituisco come Info
extract_info(Line) :-
    sub_string(Line, Before, _, After, '['),
    sub_string(Line, FirstBefore, _, FirstAfter, ']'),
    sum(Before, 1, Result),
    sum(FirstAfter, 1, ResultTwo),
    sub_string(Line, Result , _, ResultTwo, FirstInfo),
    sum(FirstAfter, 2, ResultTwo),
    sub_string(SecondInfoDirty, FirstAfter , _, 0, SecondInfoDirty),
    ThirdInfoRaw = SecondInfoRaw,
    sub_string(SecondInfoRaw, FourthBefore, _, FourthAfter, ']'),
    sum(FourthAfter, 1, ResultTwo),
    sub_string(SecondInfoRaw, 1, _, ResultTwo, SecondInfo),
    sub_string(ThirdInfoRaw, FifthBefore, _, FifthAfter, ','),
    sub_string(ThirdInfoRaw, SixthBefore, _, SixthAfter, ']'),
    sum(FifthBefore, 2, Result),
    sub_string(ThirdInfoRaw, Result, _, 0, ThirdInfoRaw),
    FourthInfoRaw = ThirdInfoRaw,
    sub_string(FourthInfoRaw, SeventhBefore, _, SeventhAfter, ']'),
    sub_string(FourthInfoRaw, EighthBefore, _, EighthAfter, ')'),
    sum(SeventhBefore, 2, Result),
    sum(EighthAfter, 1, ResultTwo),
    sub_string(FourthInfoRaw, Result, _, ResultTwo, FourthInfo),
    sub_string(ThirdInfoRaw, NinthBefore, _, NinthAfter, ']'),
    sum(NinthAfter, 1, Result),
    sub_string(ThirdInfoRaw, 0, _, Result, ThirdInfoRaw),
    create_transition_splitting_third(FirstInfo,SecondInfo,FourthInfo,ThirdInfoRaw).


create_transition_splitting_third(FirstInfo,SecondInfo,FourthInfo,ThirdInfoRaw) :-
    sub_string(ThirdInfoRaw, Before, _, After, ','),
    sub_string(ThirdInfoRaw, 0, _, After + 1, ThirdInfo),
    sum(Before, 1, Result),
    sub_string(ThirdInfoRaw, Result, _, 0, ThirdInfoRaw),
    List = [FirstInfo,SecondInfo,ThirdInfo,FourthInfo],
    add_to_list(List),
    get_list(List),
    writeln(List),
    create_transition_splitting_third(FirstInfo,SecondInfo,FourthInfo,ThirdInfoRaw).

create_transition_splitting_third(FirstInfo,SecondInfo,FourthInfo,ThirdInfoRaw) :-
    !,
    List = [FirstInfo,SecondInfo,ThirdInfoRaw,FourthInfo],
    add_to_list(List).






% Predicate to print the list of objects with numbering
print_transition(List) :-
    print_objects(List, 1).

% Base case: empty list
print_objects([], _).

% Recursive case: print the head of the list with numbering and recursively process the tail
print_objects([Object | Rest], N) :-
    format('~d: ~w~n', [N, Object]),
    N1 is N + 1,
    print_objects(Rest, N1).