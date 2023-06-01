% Soluzione per poter rendere meno vincolante il codice al modulo
% inserito.
% In questo modo basta inserire il nome del modulo da cui sono state
% lette le transition per poter visualizzare nel file i dati
% senza la necessita di fare hardcode col nome del modello
:- dynamic module/1.

set_module(Atom) :-
    retractall(module(_)),
    assertz(module(Atom)).

get_module(Atom) :-
    module(Atom).

% Legge il file wf e lo elabora
process_file(FileName) :-
    ask_folder_path(Path),
    use_module(FileName),
    ask_module_name(ModuleString),
    string_to_atom(ModuleString, Module),
    set_module(Module),
    open(Path, write, Stream),
    do_works(Stream).

% Inizia esecuzione principale
do_works(Stream) :-
    get_module(Module),
    findall(Module:transition(Input-Output, Cases, Repetition), 
            Module:transition(Input-Output, Cases, Repetition), 
            ListTransitions),  % Gather all transitions into a list
    do_work(Module, Stream, ListTransitions).

% Esegue le operazioni di scrittura su file e recupero delle informazioni delle transition
do_work(Module, Stream, [Module:transition(Input-Output, Cases, Repetition) | Rest]) :-
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

% Recupera tutte le transizioni che hanno la combinazione Input-Output 
% uguale in input e verifica caso per caso se la transizione combacia con quella ricercata
% Alla fine tutte le transizioni che rispettano i parametri di ricerca sono salvati nella
% lista NewTransitions
% Tutte le transizioni che non sono gia state trovate (e quindi che non sono nella lista Transitions), vengono
% aggiunte alla lista finale che si chiama LastTransitions che alla fine verra stampata
get_matching_transitions_with_case(Stream, Input-Output, [Case|Cases], Transitions) :-
    get_module(Module),
    findall(Module:transition(Input-Output, TransitionsList, Repetition),
            (Module:transition(Input-Output, TransitionsList, Repetition),
             member(Case, TransitionsList)),
            NewTransitions),
    add_to_list_no_duplicates(NewTransitions, Transitions, LastTransitions),
    get_matching_transitions_with_case(Stream, Input-Output, Cases, LastTransitions).


% Cerco le transition in base alla azione di input e al caso
get_matching_transitions_with_case(Stream, Input-Output, [], Transitions) :-
    get_matching_transitions(Input-Output, ExtraTransitions),
    print_list(Stream, Transitions),
    add_to_list_deleting_duplicates(ExtraTransitions, Transitions, NewExtraTransitions),
    writeln(Stream, 'Possible transitions that could happen even if the cases do not match:'),
    print_list(Stream, NewExtraTransitions).

% Cerco le transition solo in base alla azione di input
get_matching_transitions(Input-Output, Transitions) :-
    get_module(Module),
    findall(Module:transition(Input-Output, TransitionsList, Repetition),
            Module:transition(Input-Output, TransitionsList, Repetition),
            Transitions).

% Questa procedura serve per copiare nella seconda lista tutti
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

% Questa procedura serve per creare una nuova lista che
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


% Procedura che stampa la lista in input e divide con dei trattini
% la formattazione
print_list(Stream, []) :-
    write(Stream, '------------------------------------------'),nl(Stream),
    flush_output(Stream).
print_list(Stream, [H|B]) :-
    writeln(Stream, H), 
    print_list(Stream, B). 

% Chiedo dove salvare il file pnml
ask_folder_path(String) :-
    write('Enter path where to save file: '),
    read_line_to_string(user_input, String).

% Chiedo dove salvare il file pnml
ask_module_name(String) :-
    write('Enter the module name used: '),
    read_line_to_string(user_input, String).
