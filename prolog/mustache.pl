:- module(mustache, [
    mustache//2,                % +Variables:list, +In:codes
    mustache_from_file/3        % +FileSpec, +Variables:list, -Out:codes
]).
%! <module> Prolog implementation of mustach templating
%  Predicates for [{{mustache}}](https://mustache.github.io/) templating system.
%  Supports the all feature of mustache spec, with following specifics:
%  * _Lambdas_: If the value associated with the property is term of the form `?(Goal)`
%    then the output is produced by the execution of the predicate `once(call(Goal, Value, Result))`
%    where result is consider as the actual value associated with the property. 
%  * _Extensions_: The proeprty values may be passed to the goals and transformed before 
%    being placed to the output list. THis may be achieved by the syntax: 
%    ```mustache
%    Some transformed {{?- my_goal. variable }} 
%    ```
%    In this case the output is produced by calling `once(call(my_goal, Value, Result))`. The goal itself
%    is retrieved by the call to the `read_term/2` therefore it must be ended by a dot.

:- use_module(library(dcg/basics)).

%%% PUBLIC PREDICATES %%%%%%%%%%%%%%%%%%%%%%%%%%

%! mustache(+Variables:list, +In:codes)// is det
%  converts mustache template into the rendered text
%  by replacing `{{ variable }}` placeholders with the  variables
%  specified in the `Variables` list. Variables are of the form 
%  `Var - Value` or `Var = Value`. The value can be atomic, codes, string, or list of values
mustache(Variables, In) -->
    mustache_impl( Variables, [ [0'{, 0'{], [0'}, 0'}] ], In).

%! mustache_from_file(+FileSpec, +Variables:list, -Out:codes) is det
%  Same as `mustache//2 but loads the template from the `FileSpec`. 
%  `FileSpec` is  resolved by `absolute_file_name/3.
mustache_from_file(FilePath, Variables, Out) :- 
    absolute_file_name(FilePath, AbsolutePath),
    read_file_to_codes(AbsolutePath, Codes, [encoding(utf8)]),
    phrase(mustache(Variables, Codes), Out).

    
%%% PRIVATE PREDICATES %%%%%%%%%%%%%%%%%%%%%%%%%

close_placeholder( [_, [] | _ ], In, In) --> [].
 close_placeholder( [_, [ H | T ] |_], [ H|In], Out) -->
    close_placeholder( [_, T ], In, Out).

comment(Delimiters, In, Out) -->
    open_placeholder(comment, Delimiters, In, R1),
    { phrase(string(_), R1, R2) },
    close_placeholder(Delimiters, R2, Out),
    !.

condition_body(_, _, In, In) --> [].
 condition_body(Variables, Delimiters, In, Out) -->
    next(Variables, Delimiters, In, Rest0),
    condition_body(Variables, Delimiters, Rest0, Out).

condition_start(Delimiters, Variable, In, Out) -->
    condition_placeholder(condition, Delimiters, Variable, In, Out).

condition_negated(Delimiters, Variable, In, Out) -->
    condition_placeholder(negation, Delimiters, Variable, In, Out).

condition_end(Delimiters, Variable, In, Out) -->
    condition_placeholder(block_end, Delimiters, Variable, In, Out).

condition_placeholder(Type, Delimiters, Variable, In, Out) -->
    open_placeholder(Type, Delimiters, In, Rest0),
    variable(Variable, Rest0, Rest1),    
    close_placeholder(Delimiters, Rest1, Out).

delimiters(Delimiters, [Start, End], In, Out) -->
    open_placeholder(delimiter, Delimiters, In, R1),
    delimiter_start(Start, R1, R2), 
    delimiter_space(R2, R3),
    delimiter_end(End, R3, R4),
    close_placeholder(Delimiters, R4, Out),
    {
        \+ length(Start, 0),
        \+ length(End, 0)
    }.

delimiter_end([], [ 0'=|In], In) --> [].
 delimiter_end([C|T], [C|In], Out) -->
    { C\= 0'= },
    delimiter_end(T, In, Out).

delimiter_space([C|In], [C|In]) -->
    { \+ is_white(C) }.
 delimiter_space([C|In], Out) -->
    { is_white(C) },
    delimiter_space(In, Out).

delimiter_start([], [S|In], [S|In]) -->
    { is_white(S) },
    !.
 delimiter_start([C|T], [C|In], Out) -->
    { \+ is_white(C) },
    delimiter_start(T, In, Out).



expand_variable(Key, VariablesIn, VariablesOut) :-
    variables_key_value(VariablesIn, Key, Value),
    expand_variable_impl(Value, VariablesIn, VariablesOut),
    !.
expand_variable(_, Variables, Variables).

expand_variable_impl(Term, VariablesIn, VariablesOut) :-
    is_dict(Term),
    dict_pairs(Term, _, Pairs),
    append(Pairs, VariablesIn, VariablesOut),
    !.
expand_variable_impl(_, Variables, Variables).

instruction(Variables, Delimiters, In, Out) -->
    (   comment( Delimiters, In, Out)
    ;   loop(Variables, Delimiters, In, Out)
    ;   negation(Variables, Delimiters, In, Out)
    ;   partial(Delimiters, In, Out)
    ;   query(Variables, Delimiters, In, Out)
    ;   lambda(Variables, Delimiters, In, Out), !
    ;   placeholder(Variables, Delimiters, In, Out)
    ).

lambda(Variables, Delimiters, In, Out) -->
    condition_placeholder(normal, Delimiters, Key, In, Out),
    {   variables_key_value(Variables, Key, ?(Goal)),
        call(Goal, Value)
    },
    push_variable_codes(Value).


loop(Variables, Delimiters, In, Out) --> 
    condition_start(Delimiters, Key, In, Rest1),
    {(  variables_key_value(Variables, Key, Value)
    ->  (   is_list(Value)
        ->  List = Value
        ;   List = [ Value ]
        )
    ;   List = []
    )}, 
    loop_elements(Key-List, Variables, Delimiters, Rest1, Out).

loop_elements( Key-[], _, Delimiters, In, Out) --> % ignore the block on empty list and move on
    {   phrase( condition_body( [], Delimiters, In, Rest), _) },
    condition_end(Delimiters, Key, Rest, Out),
    [].
 loop_elements( Key-[ Var | List], Variables, Delimiters,  In, Out) -->
    { 
        Variables1 = [Key-Var | Variables],
        expand_variable(Key, Variables1, Variables2),
        phrase( condition_body( Variables2, Delimiters, In, Rest), Body)
    },
    condition_end(Delimiters, Key, Rest, _),
    Body,
    loop_elements(Key-List, Variables, Delimiters, In, Out).

mustache_impl(_, _, []) --> [], !.
mustache_impl(Variables, Delimiters, In) -->
    delimiters(Delimiters, Delimiters1, In, Out),
    mustache_impl( Variables, Delimiters1,  Out),
    !.
 mustache_impl(Variables, Delimiters, In) -->
    next( Variables, Delimiters, In, Out),
    mustache_impl( Variables, Delimiters,  Out).

negation(Variables, Delimiters, In, Out) --> 
    negation_start(Key, Delimiters, In, Rest1),
    (  { variables_key_value(Variables, Key, _) }
    ->  loop_elements(Key-[], Variables, Delimiters, Rest1, Out)
    ;   loop_elements(Key-[ [] ], Variables, Delimiters, Rest1, Out)
    ).

negation_start(Variable, Delimiters, In, Out) -->
    condition_placeholder(negation, Delimiters, Variable, In, Out).

next(Variables, Delimiters, In, Out) -->
    instruction(Variables, Delimiters, In, Out), !.
 next(_, _, [C|Out], Out) --> [C].

open_placeholder(Type, [DelimiterStart | _], In, Out) --> 
    {   memberchk(
            Type-Suffix, 
            [   block_end-[0'/], 
                comment-[0'!],
                condition-[0'#],
                delimiter-[0'=],
                negation-[0'^],
                normal-[],
                partial-[0'>],
                query-[0'?, 0'-]
            ]
        ),
        append(DelimiterStart, Suffix, Opener)
    },
    open_placeholder_impl(Opener, In, Out).

open_placeholder_impl([], In, In) --> [].
 open_placeholder_impl([ C| T], [C|In], Out) --> 
    open_placeholder_impl(T, In, Out).

partial( Delimiters, In, Out) -->
    condition_placeholder(partial, Delimiters, Base, In, Rest1),
    {   file_name_extension(Base, 'mustache', Path),
        exists_file(Path),
        read_file_to_codes(Path, Codes, [encoding(utf8)]),
        append(Codes, Rest1, Out)
    }.

placeholder(Variables, Delimiters, In, Out) -->
    open_placeholder(normal, Delimiters, In, Rest0),
    variable(Variable, Rest0, Rest1),    
    close_placeholder(Delimiters, Rest1, Out),
    push_variable(Variable, Variables).

push_variable(Key, Variables) --> 
    { variables_key_value(Variables, Key, Term) }, 
    push_variable_codes( Term ),
    !.
 push_variable(_, _) --> [].
   

push_variable_codes( Codes) -->
    {
        is_of_type(codes, Codes)
    },
    Codes.
 push_variable_codes( Atom) -->
    {
        is_of_type(atom, Atom),
        atom_codes(Atom, Codes)
    },
    Codes.
 push_variable_codes( String) -->
    {
        is_of_type(string, String),
        string_codes(String, Codes)
    },
    Codes.
 push_variable_codes( Term ) -->
    {
        format(codes(Codes), '~w', Term)
    },
    Codes.

query(Variables, Delimiters, In, Out) -->
    open_placeholder(query, Delimiters, In, R1), 
    { query_goal(Goal, R1, Rest0) },
    variable(Variable, Rest0, Rest1),    
    close_placeholder(Delimiters, Rest1, Out),
    { variables_key_value(Variables, Variable, Value) }, 
    {   once(call(Goal, Value, Result) ), 
        (   is_of_type(codes, Result)
        ->  ResultCodes = Result
        ;   atomic_list_concat([Result], Atom),
            atom_codes(Atom, ResultCodes)
        )
    },
    ResultCodes.
    
query_goal(Goal, In, Out) :-
    setup_call_cleanup(
        open_any(string(In), read, Stream, Close, []),
        (   read_term(Stream, Goal, [syntax_errors(fail)]),
            stream_property(Stream, position(Position) )
        ),
        close_any(Close)
    ),
    stream_position_data(char_count, Position, Offset),
    append(Prefix, Out, In),
    length(Prefix, Offset),
    !.

var_chars([]) --> [].
var_chars([C|Var]) -->
    [C],
    { 
        (
            code_type(C,csym) 
        ;
            memberchk(C, [ 0'-, 0'., 0'/, 0'(, 0'), 0'[, 0'], 0'\\ ] )
        )
    },
    var_chars(Var).

variable(Variable, In, Out) --> 
    {
        phrase(variable_impl(VariableC), In, Out),
        atom_codes(Variable, VariableC)
    }.

variable_impl(Var) -->
    whites,
    var_chars(Var),
    whites.

variables_key_value(Variables, Key, Value) :-
    is_dict(Variables),
    !,
    Value = Variables.get(Key),
    \+ memberchk(Value, [ [], undefined, null, false ]).
 variables_key_value(Variables, Key, Value) :-
    memberchk(Key-Value, Variables),
    \+ memberchk(Value, [ [], undefined, null, false ]).
 variables_key_value(Variables, Key, Value) :-
    memberchk(Key=Value, Variables),
    \+ memberchk(Value, [ [], undefined, null, false ]). % false values do not counts!


 

