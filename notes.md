# notes on eval

larceny, chez, gambit:

`(eval '(+ 1 2)`

scheme48:

`(eval '(+ 1 2) (interaction-environment))`

MIT:

`(eval '(+ 1 2) (the-environment))`
