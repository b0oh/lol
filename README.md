# Lol — Lisp On erLang

_Lol_ — and programming is fun again.

## Why?

Because the world strongly needs another programming language.

A little bit more seriously: the main idea is to make metaprogramming in erlang easier.<br>
It is not about replacing _Erlang_, it is about using the two together.

### Why not LFE? Lol has:

* Lisp-1
* Small and clean codebase (for now :))

### Why not Joxa?

* _Joxa_ is not developed anymore and it uses code generation for the erlang core.

## Build

```
$ git clone git://github.com/b0oh/lol.git
$ cd lol
$ ./rebar compile
$ echo '(defn hello (name) (io:format "Hello, ~s.~n" [name]))' > simple.lol
$ erl -pa ebin
> lol_compile:file("simple.lol", "ebin/simple.beam").
{ok,simple,[]}
> simple:hello("Dima").
Hello, Dima.
ok
```

## Example
[Lol application example](http://github.com/b0oh/lol_example)

## Lol language

### Forms

* Fixnum: ```1```
* Symbol: ```symbol```
* Simple string: ```"string"```
* S-expr: ```()```

### Special forms

* List: ```[1 2 3]``` expanding to ```(list 1 2 3)```
* Tuple: ```{1 2 3}``` expanding to ```(tuple 1 2 3)```
* Quote: ```'(1 2 3)``` expanding to ```(quote (1 2 3))```
* Define public function:
```
(defn public (arg1 arg2)
  (expr1)
  (expr2))
```
* Define private function:
```
  (defn- private (arg1 arg2)
    (expr1)
    (expr2))
```
* Lambda: ```(lambda (arg1 arg2) (expr1) (expr2))```
* Let with pattern matching:
```
  (let ({'ok file} (file:read_file "number")
        number (binary_to_integer file))
    (io:format "Number is: ~p~n" [number]))
```
*  Let with sequence in body:
```
  (let ({'ok file} (file:read_file("number"))
        number (binary_to_integer file))
    (do
      (file:write_file "number_copy" (integer_to_binary number))
      (io:format "Number is: ~p~n" [number])))
```
* Case:
```
(case (get_coords)
  ({x y} 'ok)
  ({x y z} (do (first_expr) (second_expr))))
```
* Call:
```
(call 'func)
(call 'func [arg1 arg2])
(call 'mod 'func)
(call 'mod 'func [arg1 arg2])
```
* Apply:
```
(func)
(mod:func)
((lambda (x) x) "some")
```

## Plans

* Full erlang forms support: float, binary, characters, strings with interpolation, records, maps, etc
* Full erlang syntax support: receive, clauses for functions and lambdas, etc
* Improve lol forms support: let, do, def, etc
* Extended importing and name aliasing
* Macroses
* ```deftransform``` form for providing macroses for _Erlang_ from _Lol_
* Translation right in Erlang sources
* stdlib
* Write Lol on Lol

It is dedicated to “Let Over Lambda” and “Land of Lisp”.

---

Have fun.
