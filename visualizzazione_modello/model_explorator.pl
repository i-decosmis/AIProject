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

subtract(X, Y, Result) :- Result is X - Y.

% Estraggo il contenuto tra parentesi e lo restituisco come Info
extract_info(Line) :-
    
    sub_string(Line, Before, _, After, '['),
    sub_string(Line, FirstBefore, _, FirstAfter, ']'),
    sum(Before, 1, Result),
    sum(FirstAfter, 1, ResultTwo),
    sub_string(Line, Result , _, ResultTwo, FirstInfo),
    sum(FirstBefore, 2, ResultThree),
    sub_string(Line, ResultThree , _, 0, SecondInfoRaw),
    ThirdInfoRaw = SecondInfoRaw,
    sub_string(SecondInfoRaw, FourthBefore, _, FourthAfter, ']'),
    sum(FourthAfter, 1, ResultFour),
    sub_string(SecondInfoRaw, 1, _, ResultFour, SecondInfo),
    sub_string(ThirdInfoRaw, FifthBefore, _, FifthAfter, ','),
    sub_string(ThirdInfoRaw, SixthBefore, _, SixthAfter, ']'),
    sum(SixthBefore, 3, ResultFive),
    sub_string(ThirdInfoRaw, ResultFive, _, 0, NewThirdInfoRaw),
    FourthInfoRaw = NewThirdInfoRaw,
    sub_string(FourthInfoRaw, SeventhBefore, _, SeventhAfter, ']'),
    sub_string(FourthInfoRaw, EighthBefore, _, EighthAfter, ')'),
    sum(SeventhBefore, 2, ResultSix),
    sum(EighthAfter, 1, ResultSeven),
    sub_string(FourthInfoRaw, ResultSix, _, ResultSeven, FourthInfo),
    sub_string(NewThirdInfoRaw, NinthBefore, _, NinthAfter, ']'),
    sum(NinthAfter, 1, ResultEight),
    sub_string(NewThirdInfoRaw, 0, _, ResultEight, FinalThirdInfoRaw),
    create_transition_splitting_third(FirstInfo,SecondInfo,FourthInfo,FinalThirdInfoRaw).


create_transition_splitting_third(FirstInfo,SecondInfo,FourthInfo,ThirdInfoRaw) :-
    sub_string(ThirdInfoRaw, Before, _, After, ','),
    sum(After, 1, Result),
    sub_string(ThirdInfoRaw, 0, _, Result, ThirdInfo),
    sum(Before, 1, ResultNine),
    sub_string(ThirdInfoRaw, ResultNine, _, 0, NewThirdInfoRaw),
    Transition = [FirstInfo,SecondInfo,ThirdInfo,FourthInfo],
    writeln(Transition),
    add_to_list(Transition),
    create_transition_splitting_third(FirstInfo,SecondInfo,FourthInfo,NewThirdInfoRaw).

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