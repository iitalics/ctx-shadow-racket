# ctx-shadow-racket
Tests with using context shadowing to give the appearance of mutation

The desired behavior is that the value printed is different each time.

```
$ racket shadowing.rkt
100
80
60
runtime
$ racket shadowing-2-breaks.rkt
100
100
100
runtime
```

The exact changes to successfuly shadow the scopes are very subtle:

```diff
$ diff shadowing-2-breaks.rkt shadowing.rkt
48c48
<                       #'x)]
---
>                       (syntax-local-introduce #'x))]
57c57
<      #:with x (syntax-property this-syntax 'variable)
---
>      #:with x (syntax-local-introduce (syntax-property this-syntax 'variable))
60d59
<
```