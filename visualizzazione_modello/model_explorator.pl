% Soluzione per poter rendere meno vincolante il codice al modulo
% inserito.
% In questo modo basta inserire il nome del modulo da cui sono state
% lette le transition per poter visualizzare nel file i dati
% senza la necessita di fare hardcode col nome del modello
:- dynamic module/1.

% Predicato che imposta il modulo
set_module(Atom) :-
    retractall(module(_)),
    assertz(module(Atom)).

% Predicato che legge il modulo
get_module(Atom) :-
    module(Atom).

% Inizializzo contatore usato nel salvataggio dei dati nel file pnml
:- dynamic(counter/1).
counter(0).

% Inizializzo contatore per gli id dei place, delle transition e degli arc nel file pnml
increment_counter :-
    retract(counter(Count)),
    CountPlusOne is Count + 1,
    assert(counter(CountPlusOne)).

% Variabile dinamica che viene utilizzata per salvare la transizione in analisi e poter
% recuperare le sue informazioni da qualsiasi parte del codice
:- dynamic current_transition/1.

% Predicato per impostare la transizione in analisi
set_current_transition(Transition) :-
    retractall(current_transition(_)),
    assertz(current_transition(Transition)).

% Predicato per recuperare la transizione in analisi
get_current_transition(Transition) :-
    current_transition(Transition).

% Legge il file wf e lo elabora
process_file(FileName) :-
    ask_folder_path(Path),
    atom_concat(Path,"/result.txt",ResultPath),
    use_module(FileName),
    ask_module_name(ModuleString),
    string_to_atom(ModuleString, Module),
    set_module(Module),
    transition_check,
    open(ResultPath, write, Stream),
    do_works(Stream, Path).

% Inizia esecuzione principale
do_works(Stream, Path) :-
    get_module(Module),
    findall(Module:transition(Input-Output, Cases, Repetition), 
            Module:transition(Input-Output, Cases, Repetition), 
            ListTransitions),  % Gather all transitions into a list
    do_work(Module, Stream, ListTransitions),
    generate_dot_files(Path).

% Esegue le operazioni di scrittura su file e recupero delle informazioni delle transition
do_work(Module, Stream, [Module:transition(Input-Output, Cases, Repetition) | Rest]) :-
    set_current_transition(Module:transition(Input-Output, Cases, Repetition)),
    write(Stream, '------------------------------------------'),nl(Stream),
    write(Stream, 'Starting transition:'), nl(Stream),
    write(Stream, Input-Output),
    write(Stream, ','),
    write(Stream, Cases),
    write(Stream, ','),
    write(Stream, Repetition),nl(Stream),
    writeln(Stream, 'You can continue to the following ones:'),
    get_matching_transitions_with_case(Stream, Output-_, Cases, Transitions),
    do_work(Module, Stream, Rest).

% Caso base quando le transizioni da analizzare sono finite
do_work(Module, Stream, []).

% Recupera tutte le transizioni che hanno nella lista di input almeno una azione completata
% dalla lista di output (usando il predicato contains_element), identificandole come possibile transizioni successive
% Alla fine tutte le transizioni che rispettano i parametri di ricerca sono salvati nella
% lista NewTransitions
% Tutte le transizioni che non sono gia state trovate (e quindi che non sono nella lista Transitions), vengono
% aggiunte alla lista finale che si chiama LastTransitions che alla fine verra stampata
get_matching_transitions_with_case(Stream, Input-Output, [Case|Cases], Transitions) :-
    get_module(Module),
    findall(Module:transition(ListInput-Output, TransitionsList, Repetition),
            (Module:transition(ListInput-Output, TransitionsList, Repetition),
             contains_element(Input,ListInput),
             member(Case, TransitionsList)),
            NewTransitions),
    add_to_list_no_duplicates(NewTransitions, Transitions, LastTransitions),
    get_matching_transitions_with_case(Stream, Input-Output, Cases, LastTransitions).


% Caso base che termina identificando tutte le transizioni che sono teoricamente possibili ma
% che non sono mai state osservate dal sistema e stampa su file tutte le transizioni trovate
get_matching_transitions_with_case(Stream, Input-Output, [], Transitions) :-
    get_matching_transitions(Input-Output, ExtraTransitions),
    print_safe_list(Stream, Transitions),
    add_to_list_deleting_duplicates(ExtraTransitions, Transitions, NewExtraTransitions),
    writeln(Stream, 'Possible transitions that could happen even if the cases do not match:'),
    print_possible_list(Stream, NewExtraTransitions).

% Cerco le transition solo in base alla azione di input
% Basta che almeno una sia presente per essere individuata come possibile condizione
% successiva
get_matching_transitions(Input-Output, Transitions) :-
    get_module(Module),
    findall(Module:transition(ListInput-Output, TransitionsList, Repetition),
            (Module:transition(ListInput-Output, TransitionsList, Repetition),
            contains_element(Input,ListInput)),
            Transitions).

% Questo predicato serve per copiare nella seconda lista tutti
% gli elementi della prima lista che non sono presenti al suo
% interno (Usato per trovare i casi con la corrispondenza senza
% tenere duplicati)
add_to_list_no_duplicates([], List, List).

add_to_list_no_duplicates([H|B], SecondList, Output) :-
    member(H, SecondList), 
    !,
    add_to_list_no_duplicates(B, SecondList, Output).

add_to_list_no_duplicates([H|B], SecondList, Output) :-
    add_to_list_no_duplicates(B, [H|SecondList], Output).

% Questo predicato serve per creare una nuova lista che
% contiene gli elementi della prima lista meno gli elementi
% della seconda (Usato per vedere i possibili casi extra senza
% corrispondenza)
add_to_list_deleting_duplicates(List, SecondList, Output) :-
    add_to_list_deleting_duplicates(List, SecondList, [], Output).

add_to_list_deleting_duplicates([], _, Temp, Temp).


add_to_list_deleting_duplicates([H|B], SecondList, Temp, Output) :-
    member(H, SecondList), 
    !,
    add_to_list_deleting_duplicates(B, SecondList, Temp, Output).

add_to_list_deleting_duplicates([H|B], SecondList, Temp, Output) :-
    add_to_list_deleting_duplicates(B, SecondList, [H|Temp], Output).


% Predicato che stampa la lista in input e divide con dei trattini
% la formattazione, inoltre genera i fatti rappresentanti gli archi del sistema
% non indicando nessuna proprieta particolare sugli archi
print_safe_list(Stream, []) :-
    write(Stream, '------------------------------------------'),nl(Stream),
    flush_output(Stream).
print_safe_list(Stream, [H|B]) :-
    get_current_transition(Transition),
    assert(edge(Transition, H,"")),
    writeln(Stream, H), 
    print_safe_list(Stream, B). 

% Questo predicato stampa la lista in input e divide con dei trattini
% la formattazione, inoltre genera i fatti rappresentanti gli archi del sistema
% includendo anche la proprieta che indica lo stile degli archi(tratteggiato)
print_possible_list(Stream, []) :-
    write(Stream, '------------------------------------------'),nl(Stream),
    flush_output(Stream).
print_possible_list(Stream, [H|B]) :-
    get_current_transition(Transition),
    assert(edge(Transition, H,'style="dashed",')),
    writeln(Stream, H), 
    print_possible_list(Stream, B). 

% Chiedo dove salvare il file pnml
ask_folder_path(String) :-
    write('Enter path where to save output file(e.g. C:/User/Desktop): '),
    read_line_to_string(user_input, String).

% Chiedo il nome del modulo usato
ask_module_name(String) :-
    write('Enter the module name used: '),
    read_line_to_string(user_input, String).

% Predicato che ci permette di verificare se ci sono elementi in comune tra due liste
contains_element([], _) :-
    false.
contains_element([H|B], List) :-
    member(H, List),
    !.
contains_element([H|B], List) :-
    \+ member(H, List),
    contains_element(B, List).


% Questo predicato genera un file dot che contiene tutta la descrizione del grafo.
% Verra scritto con la proprieta ordering=out per la visualizzazione del grafo
generate_dot_files(Path) :-
    atom_concat(Path, "/orderingout.dot", OrderingPath),
    open(OrderingPath, write, OrderingStream),
    write(OrderingStream, "digraph G {\n"),
    write(OrderingStream, "ordering=out;\n"),
    write_edge(OrderingStream),
    write(OrderingStream, "}\n"),
    close(OrderingStream),
    atom_concat(Path, "/rankdirlr.dot", RankdirlrPath),
    open(RankdirlrPath, write, RankdirlrStream),
    write(RankdirlrStream, "digraph G {\n"),
    write(RankdirlrStream, "rankdir=LR;\n"),
    write_edge(RankdirlrStream),
    write(RankdirlrStream, "}\n"),
    close(RankdirlrStream).

% Questo predicato serve a stampare nel file dot tutti gli edge trovati 
write_edge(Stream) :-
    edge(From, To, Properties),
    get_transition(From, Module:transition(FirstInput-FirstOutput, FirstList, FirstRepetition)),
    get_transition(To, Module:transition(SecondInput-SecondOutput, SecondList, SecondRepetition)),
    length(FirstList, FirstWeight),
    length(SecondList, SecondWeight),
    number_of_items_in_common(FirstList, SecondList, Count),
    format(Stream, '"~w Weight:~w"->"~w Weight:~w"[~wlabel="~w"];\n', 
           [Module:transition(FirstInput-FirstOutput, FirstList, FirstRepetition), 
            FirstWeight,
            Module:transition(SecondInput-SecondOutput, SecondList, SecondRepetition), 
            SecondWeight,
            Properties,
            Count]),
    fail.
write_edge(_).

% Questo predicato serve a unificare la transizione in input a quella di output
get_transition(Transition, Transition).


% Questo predicato serve a trovare il numero di elementi in comune tra due liste
number_of_items_in_common([], [], 0).

number_of_items_in_common([], _, 0).

number_of_items_in_common(_, [], 0).

number_of_items_in_common([H|B], List, Count) :-
    member(H, List),
    number_of_items_in_common(B, List, PrevCount),
    Count is PrevCount + 1.

number_of_items_in_common([H|B], List, Count) :-
    \+ member(H, List),
    number_of_items_in_common(B, List, Count).

% Questo predicato viene usato nel caso in cui troviamo dei fatti
% transition/2, che vengono convertiti a transition/3 aggiungendo un
% valore unico come terzo elemento
transition_check :-
    get_module(Module),
    clause(Module:Module:transition(FirstInput-FirstOutput, FirstList), true),
    counter(Count),
    increment_counter,
    assert(Module:Module:transition(FirstInput-FirstOutput, FirstList,Count)),
    fail.

transition_check.