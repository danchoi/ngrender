# ngrender

Pure Haskell rendering of AngularJS templates.

This project is alpha and not production ready.

## Use cases

Server-side AngularJS templating is useful in some situations.

E.g.,

1. Reusing AngularJS web templates for HTML email generation;
2. Rendering a very large array of JSON objects in an AngularJS ng-repeat loop 
   has bad performance and in cases no performance at all in a web browser -- not so using `ngrender`;
3. Because you like AngularJS's templating language 

## Prerequisites

* [Haskell platform](https://www.haskell.org/platform)

## Setup

```
cabal install
```

## Test

```
ngrender sample.html < sample.json
```

injects this JSON

```json
{
  "wizard": {
    "name": "Gandalf"
  },
  "party": [
    {
      "name": "Thorin",
      "species": "dwarf"
    },
    {
      "name": "Fili",
      "species": "dwarf"
    },
    {
      "name": "Kili",
      "species": "dwarf",
      "injuries": 13
    },
    {
      "name": "Balin",
      "species": "dwarf",
      "injuries": 12
    },
    {
      "name": "Dwalin",
      "species": "dwarf"
    },
    {
      "name": "Olin",
      "species": "dwarf"
    },
    {
      "name": "Gloin",
      "species": "dwarf",
      "injuries": 10
    },
    {
      "name": "Ori",
      "species": "dwarf"
    },
    {
      "name": "Dori",
      "species": "dwarf",
      "injuries": 8
    },
    {
      "name": "Nori",
      "species": "dwarf"
    },
    {
      "name": "Bifur",
      "species": "dwarf",
      "injuries": 13
    },
    {
      "name": "Bofur",
      "species": "dwarf",
      "injuries": 23
    },
    {
      "name": "Bombur",
      "species": "dwarf"
    },
    {
      "name": "Bilbo",
      "species": "hobbit"
    }
  ]
}
```

into this template

```html
<div>Wizard {{wizard.name}}</div>
<ul>
  <li class="party" ng-repeat="person in party" ng-hide="person.injuries && person.injuries > 9">
    <p>Name: <strong ng-bind="person.name"></strong></p>
    <p>Species: <em>{{person.species}}</em></p>
    <p ng-show="person.injuries">Injuries: {{person.injuries}}</p>
  </li>
</ul>
```

to generate this HTML:

```html
<ul>
  <li class="myclass">
    Hello one
    <br/> 
    Votes: 1
  </li>
  <li class="myclass">
    Hello two
    <br/> 
    Votes: 2
  </li>
  <li class="myclass">
    Hello three
    <br/> 
    Votes: 3
  </li>
</ul>
<div>Wizard Gandalf</div>
<ul>
  <li class="party">
    <p>Name: <strong>Thorin</strong></p>
    <p>Species: <em>dwarf</em></p>
  </li>
  <li class="party">
    <p>Name: <strong>Fili</strong></p>
    <p>Species: <em>dwarf</em></p>
  </li>
  <li class="party">
    <p>Name: <strong>Dwalin</strong></p>
    <p>Species: <em>dwarf</em></p>
  </li>
  <li class="party">
    <p>Name: <strong>Olin</strong></p>
    <p>Species: <em>dwarf</em></p>
  </li>
  <li class="party">
    <p>Name: <strong>Ori</strong></p>
    <p>Species: <em>dwarf</em></p>
  </li>
  <li class="party">
    <p>Name: <strong>Dori</strong></p>
    <p>Species: <em>dwarf</em></p>
    <p>Injuries: 8</p>
  </li>
  <li class="party">
    <p>Name: <strong>Nori</strong></p>
    <p>Species: <em>dwarf</em></p>
  </li>
  <li class="party">
    <p>Name: <strong>Bombur</strong></p>
    <p>Species: <em>dwarf</em></p>
  </li>
  <li class="party">
    <p>Name: <strong>Bilbo</strong></p>
    <p>Species: <em>hobbit</em></p>
  </li>
</ul>


```

## ng layouts

```
.cabal-sanbox/bin/ngrender layout.html items.html < items.json
```

If layout.html contains a `<div ng-view></div>`, the content from items.html
will be nested in it.


## Supported or partially supported:

* ng-repeat
* ng-hide, ng-show, ng-bind, ng-html-bind, 
* ng-href, ng-src
* ng-class
* {{ }} expressions
* Angular expressions in ng-attributes

## Not yet supported

* AngularJS filter expressions, e.g. `item.price | number:2`
* ng-model
* custom directives

This list is not exhaustive.


## Author

Daniel Choi (danchoi on github)



