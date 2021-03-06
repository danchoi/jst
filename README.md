# jst

Simple json text templating.


items.jst

```
## {{.title}}
{{for item in .items-}}
* {{$index}}. {{item.name}}
{{end-}}

hello
```

items.json

```
{"items":[
  {"name":"apple"},
  {"name":"orange"}
  ],
 "title":"foods"}
```

Command:

    jst items.jst items.json

    or 

    jst items.jst - < items.json

output:

```
## foods

* 1. apple
* 2. orange

hello
```

## Template expressions 

All template expressions are enclosed by double braces.

    # interpolation
    {{ .foo }}

    # for block
    {{ for item in .foos }}{{ item.name }}{{ end }}

    # conditional block
    {{ if .foo }}YES{{ end }}

Use `-}}` instead of `}}` to close the expression to suppress all
whitespace after the template expression up to and including the
following newline. 

    {{ for item in .foo -}} 
    {{ item.name }}
    {{ end }}

This is useful when you don't want template control expressions to 
create blank lines but want the control syntax to readable. To
get the same output as the above without the whitespace suppression,
you'd have to write this:

    {{ for item in .foo }}{{ item.name }}
    {{ end }}

## Interpolation

Interpolation is handled through this syntax:

    # foo property of the top level json value
    {{ .foo }} 

    # foo property of a value aliased as `item` in a loop (see below)
    {{ item.foo }} 

## For loops

The `for in EXPR` loops over arrays. 

If the top-level value is an object, you have to specify a property
that is an array, like this:

    # at the top level 
    {{ for item in .items }}
   
    # nesting loops
    {{ for item in .items }}
      {{ for item2 in item.items }}
    
    # top level value is an array
    {{ for item in [] }}

Within a loop, these variables are in scope:

variable | type | desc
-- | -- | --
$index | number | iteration number, starting with 1
$length | number | total number of items for loop
$last | boolean | is on last iteration

## Conditional blocks

Conditional blocks are supported:

    {{ if .foo == 'bar' -}} 
    Foo is bar!
    {{ end -}}

    {{ if .foo == 'bar' -}} 
    Foo is bar!
    {{ else -}}
    Foo is not bar
    {{ end -}}

    {{ if .foo == 'bar' -}} 
    Foo is bar!
    {{ else if .foo == 'quux' -}}
    Foo is quux
    {{ else -}}
    Foo is not bar
    {{ end -}}


## Operators

Unary negation and the following binary operators are supported:

    + - * / == != && ||

You can also group expressions with parentheses.

    {{ if ! $last && ( .status == 'active' ) }}



