# ngrender

Pure Haskell rendering of AngularJS templates.

This project is in progress and not production ready.

## Setup

```
cabal sandbox init
cabal install
```

## Test

```
.cabal-sanbox/bin/ngrender items.html < items.json
```

transforms this template

```html
<ul>
  <li class="myclass" ng-repeat="item in items">
    Hello {{item.name}}
    <br/> 
    Votes: {{item.votes}}
  </li>
</ul>
```

into 

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

```

## ng layouts

```
.cabal-sanbox/bin/ngrender layout.html items.html < items.json
```

If layout.html contains a `<div ng-view></div>`, the content from items.html
will be nested in it.


## Done

* ng-repeat
* ng-hide, ng-show, ng-bind, ng-html-bind
* ng-href, ng-src
* ng-class
* {{ }} expressions
* Angular expressions, sans filters

## TODO

* filters
* ng-model
