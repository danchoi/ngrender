# ngrender

Pure Haskell rendering of AngularJS templates.

This project is alpha and not production ready.

## Use cases

Server-side AngularJS templating may useful in some situations, such as

* You want to reuse AngularJS web templates for HTML email generation.
* You want to render a very large array of JSON objects in an AngularJS ng-repeat loop.
  This has terrible performance in the web browser and in some cases can't even happen. You
  want a way to render the AngularJS templates with more horsepower and speed
  than inside the browser's pokey JavaScript engine. 
* You like AngularJS's templating language and want to use it for everything dynamic HTML.

## Prerequisites

* [Haskell platform](https://www.haskell.org/platform)

## Setup

```
cabal install
```

## Usage

```
ngrender sample.nghtml < sample.json
```

injects this JSON (sample.json)

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

into this template (sample.nghtml)

```html
<div>Wizard {{wizard.name}}</div>
<ul>
  <li ng-repeat="person in party" 
      ng-hide="person.injuries && person.injuries > 9"
      ng-class="{dwarf: person.species == 'dwarf', hobbit: person.species == 'hobbit', 
                injured: person.injuries}">
    <p>Name: <strong ng-bind="person.name"></strong></p>
    <p>Species: <em>{{person.species}}</em></p>
    <p ng-show="person.injuries">Injuries: {{person.injuries}}</p>
  </li>
</ul>
```

to generate this HTML:

```html
<div>Wizard Gandalf</div>
<ul>
  <li class="dwarf">
    <p>Name: <strong>Thorin</strong></p>
    <p>Species: <em>dwarf</em></p>
  </li>
  <li class="dwarf">
    <p>Name: <strong>Fili</strong></p>
    <p>Species: <em>dwarf</em></p>
  </li>
  <li class="dwarf">
    <p>Name: <strong>Dwalin</strong></p>
    <p>Species: <em>dwarf</em></p>
  </li>
  <li class="dwarf">
    <p>Name: <strong>Olin</strong></p>
    <p>Species: <em>dwarf</em></p>
  </li>
  <li class="dwarf">
    <p>Name: <strong>Ori</strong></p>
    <p>Species: <em>dwarf</em></p>
  </li>
  <li class="dwarf injured">
    <p>Name: <strong>Dori</strong></p>
    <p>Species: <em>dwarf</em></p>
    <p>Injuries: 8</p>
  </li>
  <li class="dwarf">
    <p>Name: <strong>Nori</strong></p>
    <p>Species: <em>dwarf</em></p>
  </li>
  <li class="dwarf">
    <p>Name: <strong>Bombur</strong></p>
    <p>Species: <em>dwarf</em></p>
  </li>
  <li class="hobbit">
    <p>Name: <strong>Bilbo</strong></p>
    <p>Species: <em>hobbit</em></p>
  </li>
</ul>

```

## ng layouts

`ngrender` can merge page templates into layout templates in a preprocessing
step like so:

```
ngrender layout.html page.html < items.json
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



