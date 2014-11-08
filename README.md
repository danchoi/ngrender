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
    Votes: 1.0
  </li>
  <li class="myclass">
    Hello two
    <br/> 
    Votes: 2.0
  </li>
  <li class="myclass">
    Hello three
    <br/> 
    Votes: 3.0
  </li>
</ul>

```



## Done

* ng-repeat
* ng-hide, ng-show, ng-bind, ng-html-bind
* {{ }} expressions
* Angular expressions, sans filters

## TODO

Lots including

* filters
* ng-model

