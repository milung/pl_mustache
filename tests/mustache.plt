:- begin_tests(mustache).
:- use_module('../prolog/mustache').


test('simple render', 
 [
    forall(memberchk(Template = Expected, 
        [
            "Some {{ var-var }} " = "Some my value ",
            "Some {{var-var}}." = "Some my value.",
            "Some {{var-var}}" = "Some my value"
        ]))
 ]) 
 :-
    % given    
    Vars = ['var-var'='my value'],
    string_codes(Template, Codes),
    % when
    phrase(mustache:mustache(Vars, Codes), Rendered),
    % then
    string_codes(Expected, Rendered).

test('dict render', 
 [
    forall(memberchk(Template = Expected, 
        [
            "Some {{var-var}}." = "Some my value."
        ]))
 ]) 
 :-
    % given    
    Vars = _{ 'var-var': 'my value'},
    string_codes(Template, Codes),
    % when
    phrase(mustache:mustache(Vars, Codes), Rendered),
    % then
    string_codes(Expected, Rendered).

test(conditional,
 [
    forall(memberchk(Template = Expected, 
        [
            "Some {{#nonvar}}true{{/nonvar}}." = "Some .",
            "Some {{#myvar}}true{{/myvar}} " = "Some true ",
            "Some {{#nonvar}}true{{/nonvar}}." = "Some .",
            "Some {{#empty-var}}true{{/empty-var}} " = "Some true ",
            "Some {{#myvar}}{{myvar}}{{/myvar}} " = "Some value ",
            "Some {{#myvar}}{{myvar}} " = "Some value ",
            "Some {{#nonvar}}true " = "Some "
        ]))
 ]) 
 :-
    % given    
    Vars = [myvar=value, 'empty-var'=''],
    string_codes(Template, Codes),
    % when
    phrase(mustache:mustache(Vars, Codes), Rendered),
    % then
    string_codes(Expected, Rendered).

test(negation,
 [
    forall(memberchk(Template = Expected, 
        [
            "Some {{^nonvar}}true{{/nonvar}}." = "Some true.",
            "Some {{^myvar}}true{{/myvar}} " = "Some  "
        ]))
 ]) 
 :-
    % given    
    Vars = [myvar=value, 'empty-var'=''],
    string_codes(Template, Codes),
    % when
    phrase(mustache:mustache(Vars, Codes), Rendered),
    % then
    string_codes(Expected, Rendered).

test(loop, 
 [
    forall(memberchk(Template = Expected, 
        [
            
            "Some {{#myvar}}+ {{myvar}} {{/myvar}} " = "Some + value1 + value2 + value3  ",
            "Some {{#sublists}}{{a}} * {{b}}{{/sublists}}." = "Some 1 * 2 * 3."
        ]))
 ]) 
 :-
    % given    
    Vars = [myvar=[value1, value2, value3], 'empty-var'='', sublists=[[a-1, b-2], [b-3]] ],
    string_codes(Template, Codes),
    % when
    phrase(mustache:mustache(Vars, Codes), Rendered),
    % then
    string_codes(Expected, Rendered).

test(comments,
 [
    forall(memberchk(Template = Expected, 
        [
            "Some {{! to be ignored }} true." = "Some  true."
        ]))
 ]) 
 :-
    % given    
    Vars = [myvar=value, 'empty-var'=''],
    string_codes(Template, Codes),
    % when
    phrase(mustache:mustache(Vars, Codes), Rendered),
    % then
    string_codes(Expected, Rendered).

test(partial,
 [
    forall(memberchk(Template = Expected, 
        [
            "Some {{> tests/external}} true." = "Some which is passed true."
        ]))
 ]) 
 :-
    % given    
    Vars = [myvar=value, 'empty-var'='', inner='passed'],
    string_codes(Template, Codes),
    % when
    phrase(mustache:mustache(Vars, Codes), Rendered),
    % then
    string_codes(Expected, Rendered).

test(lambdas,
 [
    forall(memberchk(Template = Expected, 
        [
            "Some {{ my_call }} true." = "Some produce true."
        ]))
 ]) 
 :-
    % given    
    Vars = [myvar=value, 'empty-var'='', my_call= ?(passed) ],
    string_codes(Template, Codes),
    % when
    phrase(mustache:mustache(Vars, Codes), Rendered),
    % then
    string_codes(Expected, Rendered).

test(delimiters,
 [
    forall(memberchk(Template = Expected, 
        [
            "{{=<% %>=}}Some <% my_call %> true." = "Some produce true.",
            "{{=<% %>=}}Some <% myvar %> true.<%={{ }}=%> And {{ myvar}}" = "Some produce value. And value"
        ]))
 ]) 
 :-
    % given    
    Vars = [myvar=value, 'empty-var'='', my_call= ?(passed) ],
    string_codes(Template, Codes),
    % when
    phrase(mustache:mustache(Vars, Codes), Rendered),
    % then
    string_codes(Expected, Rendered).

test(extended,
 [
    forall(memberchk(Template = Expected, 
        [
            "Some {{?- down. myvar }} true." = "Some reinforced true."
        ]))
 ]) 
 :-
    % given    
    Vars = [myvar="ReInforCeD" ],
    string_codes(Template, Codes),
    % when
    phrase(mustache:mustache(Vars, Codes), Rendered),
    % then
    string_codes(Expected, Rendered).

:- end_tests(mustache).

passed(produce).

down(Value, Out) :-
    atom_string(AnyCase, Value),
    downcase_atom(AnyCase, Out).
    