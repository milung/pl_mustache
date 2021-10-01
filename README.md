# Mustache

Prolog implementation and predicates for [{{mustache}}](https://mustache.github.io/) templating system.

Supports all features of the mustache spec, with the following specifics:

* _Lambdas_: If the value associated with the property is term of the form `?(Goal)`
  then the output is produced by the execution of the predicate `once(call(Goal, Value, Result))`
  where result is consider as the actual value associated with the property.
* _Extensions_: The proeprty values may be passed to the goals and transformed before
  being placed to the output list. THis may be achieved by the syntax:

  ```mustache
  Some transformed {{?- my_goal. variable }} 
  ```

  In this case the output is produced by calling `once(call(my_goal, Value, Result))`. The goal itself
  is retrieved by the call to the `read_term/2` therefore it must be ended by a dot.

## USAGE

Use module `mustache`

Example:

```prolog
:- use_module(library(`mustache`)).

hallo :-
    mustache_from_file('index.mustache', _{ name: 'World' }, Output }, 
    writeOutput()
```

## Exported predicates

### `mustache(+Variables:list, +In:codes)//` is det

Converts mustache template into the rendered text
by replacing `{{ variable }}` placeholders with the  variables
specified in the `Variables` list. `Variables` is either list of terms
`Var - Value` or `Var = Value`, or `Variables` and properties may be made of dicts.
The value can be atomic, codes, string, or list of values, or if used with extension then
any valid prolog term

### `mustache_from_file(+FileSpec, +Variables:list, -Out:codes)` is det

Same as `mustache//2` but loads the template from the `FileSpec`.
`FileSpec` is  resolved by `absolute_file_name/3.`

## Testing

The script `run-tests.ps1` executes the tests

## Development

To debug the module, load the `debug.pl` file into prolog top.
