# jstmpl


items.tmpl

```
## {{.title}}
{{for item in .items-}}
{{if $last}}and {{end}}{{item.name}}{{if (! $last) && $index != 1}},{{end-}}
 {{end-}}

{{for item in .items-}}
* {{$index}}. {{item.name}} {{.title}} {{if $last}}LAST{{end}} {{$length}}
{{end-}}

hello
```

items.json

```
{"items":[
  {"name":"apple"},
  {"name":"orange"}
  ],
"title":"foods"
}
```

Command:

    jst items.tmpl items.json

output:

```
## foods
apple and orange

* 1. apple foods  2
* 2. orange foods LAST 2

hello
```


