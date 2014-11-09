# ngrender

Pure server-side rendering of AngularJS templates.

This project is beta and not guaranteed to work with your AngularJS templates. YMMV.

## Use cases

Server-side AngularJS templating may useful in some situations, such as

* You want an easy way to serve your JavaScript MVC website content [for 
  web indexing crawlers](http://www.ng-newsletter.com/posts/serious-angular-seo.html). 
* You want to render a very large array of JSON objects in an AngularJS ng-repeat loop.
  This has terrible performance in the web browser and in some cases can't even happen. You
  want a way to render the AngularJS templates with more horsepower than the browser's pokey JavaScript engine. 
* You want to reuse AngularJS web templates for HTML email generation.
* You want to reuse AngularJS web templates for static HTML page generation.

## Prerequisites

* [Haskell platform](https://www.haskell.org/platform)

## Setup

From the project directory:

```
cabal install
```

This will likely install the `ngrender` executable in ~/.cabal/bin, which should be on your PATH.

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

`ngrender` can merge page templates into layout templates like so:

```
ngrender layout.html page.html < items.json
```

layout.html must contain a `<div ng-view></div>`.  The content from page.html
will be nested in it.


## Supported or partially supported:

* ng-repeat
* ng-hide, ng-show, ng-bind, ng-html-bind, 
* ng-href, ng-src
* ng-class
* {{ }} expressions
* Angular expressions in ng-attributes

## Not yet supported

* AngularJS filter expressions, e.g. `item.price | number:2`. Currently filters are ignored and the output happens as if the filters aren't there
* ng-model
* custom directives

This list is not exhaustive.

## How this interacts with AngularJS's client-side library

ngrender processes the ng-directives it knows how to deal with and strips them
from the HTML output.  Therefore you shouldn't expect the generated HTML to be
rebound and updated by AngularJS controllers loaded into the page by script
tags.


## Author

Daniel Choi ([danchoi](https://github.com/danchoi/) on github)



