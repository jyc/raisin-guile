# Raisin

This repository contains some experiments on replicating the interface of
OCaml's [Async][1] library in Guile (originally in CHICKEN). It does not go
anywhere near the scope of Async -- Async ([seems to][2]) encompasses its own
garbage collector, scheduler, and, importantly, integrated I/O modules. All I
have implemented is the basic Ivar/Deferred structures, a naive scheduler, and
an `after` procedure.

I made it for fun and to see how difficult such a thing would be.

# Tutorial

There are two fundamental types in Raisin: ivars and deferreds.

Ivars are structures that can contain a value. They are created with the
`(new-ivar)` procedure. An ivar can be in one of two states: filled or empty.
Ivars are initially empty. You can fill an ivar `i` with a value `x` using
`(ivar-fill! i x)`. Once an ivar is filled, it can never be filled again.
Calling `ivar-fill!` on a filled ivar causes a runtime exception.

What's the point of a fancy variable that can only be set once?  This is where
deferreds come in. Deferreds represent a value that may or may not become
determined at some point in time. You can create a deferred from an ivar `i`
using `(ivar-read i)`. All of the deferreds returned will become determined
with the value `x` when `(ivar-fill! i x)` is called.

There is also the `return` procedure. `(return x)` creates a deferred that has
already been filled with the value `x`. Note that `return` is not Scheme syntax
-- Scheme procedures return the value of the last expression the body of their
definition. `return` is just another procedure provided by Raisin.

There is a `(peek d)` procedure that returns `'empty` (the symbol `'empty`) if
the ivar to which the deferred `d` is bound is not yet filled or `('filled . x)`
(a pair, created with `(cons 'filled x)`) if the ivar has been filled with the
value `x`. But because deferreds represent values that may not yet be
determined, you normally don't access them directly. Instead, you bind
procedures to them to create new deferreds.

Let's write a procedure `f` that takes an integer argument and returns a new
deferred whose value is that integer plus one:

(define (f x)
  (return (+ x 1)))

Suppose we have a deferred `d` that will become determined to some integer at
some point in the future.  For example, we might be reading an integer from a
computer across the network.  Doing this takes time in which we'd like other
procedures to run, but we'd also like to be able to operate on this value even
before it becomes determined.  We can bind `f` to `d` to create a new deferred
`d*` representing the value to which `d` becomes determined, plus one.

    (define d* (bind d f))

If we want to print out the value of `d*` at the time *it* becomes determined
(that is, after `d` has become determined and the scheduler has executed `f` on
the value `d` became determined to) we can bind `d*` to another procedure:

    (bind d*
          (lambda (x)
            (print x)
            (return '())))

Note that here we've bound `d*` to an "anonymous" procedure created with
`lambda` instead of binding it to a procedure defined beforehand. Also note
that even though we only cared about this procedure for its side effects, we
still needed to have a `(return '())` at the end. This is because `bind` always
expects a deferred as its first argument and a one-argument procedure returning
a deferred as its second argument. If you forget to have some sort of deferred
as the last expression in the body of a procedure you have bound, the scheduler
will throw a runtime exception.

To recap, here is what what we've written looks like so far:

    (define (f x)
      (return (+ x 1)))

    (define d* (bind d f))

    (bind d*
          (lambda (x)
            (print x)
            (return '())))

Supposing the definition of some deferred `d` that returns a number, we will end
up with a deferred `d*` that becomes determined with the value that `d` becomes
determined to, plus one, and a procedure bound to `d*` that will print out that
value.

If we didn't care about defining a new deferred `d*`, we could rewrite this like
so:

    (define (f x)
      (return (+ x 1)))

    (bind (bind d f)
          (lambda (x)
            (print x)
            (return '())))

We could even avoid defining `f` and just use a `lambda`:

    (bind (bind d
               (lambda (x)
                 (return (+ x 1))))
          (lambda (x)
            (print x)
            (return '())))

This style tends to get unwieldy after more than a few `bind`s, though, so there
is a macro `>>=` that lets us chain binds more conveniently.

    (>>= d
         (lambda (x)
           (return (+ x 1)))
         (lambda (x)
           (print x)
           (return '())))

## Deferreds vs. callbacks

You might recognize this as similar to the callback pattern in other languages.
In JavaScript, this example might have looked like:

    getX(function (x) {
        addOne(x, function (y) {
            print(x);
        });
    });

There are some important differences. First, calling callbacks is the
responsibility of the procedure that is doing the asynchronous computation.
This has some important implications. First, suppose we want to store in a
variable a deferred corresponding to a value that may be filled in the future or
may even already be filled. Doing this with callbacks is tricky. If the value
is already determined, we can't register a new callback. Even if we're already
sure the value hasn't been determined but if we want to register multiple
callbacks, we have to implement this by wrapping the functions we want to bind.
And if the callback we register itself calls callbacks, we end up with a nested
callback execution path instead of a linear sequence of bound procedures being
called. And we cannot represent the value of an asynchronous computation.

In Raisin, procedures instead fill ivars, and the scheduler handles resolving
all of the bound procedures. It is guaranteed that the execution of a bound
procedure `f` will not be interrupted by the execution of another bound
procedure, or even bound procedures whose deferreds become determined by the
execution of `f`. Because binding a procedure to a deferred results in a new
deferred, you can pass deferreds around as values, put them in lists, return
them from functions, and more.


# Procedures

## Ivars

ivars are structures may contain a value and are in one of two states -- empty
or filled. The name comes from from Concurrent ML's IVar, which comes from Id's
I-Structure).

An ivar is created with `(new-ivar)`. It is initially in the empty state. In the
empty state a given ivar `i` can be filled with a value `x` using `(ivar-fill! i
x)`, after which `i` becomes filled. Once an ivar is filled, attempting to fill
it again will result in a non-continuable `(exn deferred ivar-stuffed)`
exception.

### new-ivar

`(new-ivar)`

`new-ivar` creates a new ivar in the empty state.

### ivar-fill!

`(ivar-fill! i x)`

`ivar-fill` fills the empty ivar `i` with the value `x` and sets it to the
filled state.

It is an error to call `ivar-fill` on an ivar that has already been filled.
Doing so will result in a non-continuable  `(exn deferred ivar-stuffed)`
exception.

### ivar-read

`(ivar-read i)`

`ivar-read` returns a deferred bound to the ivar `i`. Once the ivar `i` becomes
filled, the returned deferred will become determined.

## Deferreds

A deferred represents a value that will become available, or determined, at some
indeterminate time in the future.

Deferreds are created from and bound to ivars using the `ivar-read` procedure
and become determined once the ivar to which they are bound is filled.

### bind

`(bind d f)`

`bind` binds a procedure `f` itself returning a deferred can be bound to a
deferred `d` using . This calls `f` with the value that `d` becomes determined
to. The call to `bind` returns a deferred that becomes determined once the
deferred returned by `f` becomes determined. In this way, asynchronous values
can be composed in a monadic fashion.

No other bound procedures are executed until `f` returns, even if `f` fills an
ivar.

### return

`(return x)`

`return` returns a deferred bound to an ivar that has already been filled
with the value `x`.

### peek

`(peek d)`

`peek` peeks at the state of the ivar to which the deferred `d` is bound. It
returns `'empty` if the ivar is empty, and `('filled . x)` if the ivar has been
filled with the value `x`.

### any

`(any ds)`

`any` returns a deferred that becomes determined with the first value that any
of deferreds in the list `ds` becomes determined to.

### all

`(all ds)` 

`all` returns a deferred that becomes determined with the list of values to
which the deferreds in `ds` become defermined.

For example,

    (all (return 1) (return 2) (return 'a))

... will become determined to `'(1 2 a)`.

### never

`(never)`

`never` returns a deferred that never becomes determined.

### after

`(after s)`

`after` returns a deferred that becomes determined after `s` seconds.

## Macros

### >>=

`(>>= a b c ...)`

`>>=` binds the deferred `a` to the procedure `b`, then binds the resulting
deferred to the procedure `c`, and so on.

### async

`(async body ...)`

`async` executes `body ...` in a new thread and returns a deferred that becomes
determined when that thread has finished executing.

For example, the `after` procedure, which returns a deferred that becomes
determined after some number of seconds, could be defined as follows:

    (define (time-after s)
      (seconds->time (+ s (time->seconds (current-time)))))

    (define (after s)
      (async
        (thread-sleep! (time-after s))
        '()))

### >>$

`(>>$ (d1 f1 ...) (d2 f2 ...))`

`>>$` returns a deferred that would result `(>>= dn fn ...)` for the first `dn`
that becomes determined. Although the other deferreds can still become
determined, the other bound procedures will not run.

For example,

    (>>$ ((after 1)
          (lambda (_)
            (print "a")
            (return 'a)))
         ((after 2)
          (lambda (_)
            (print "b")
            (return 'b))))

... will become determined to `'a` after printing "a". "b" should never be
printed.

### seq

    (seq d
         x <- (f foo)
         _ <- (b x)
         y <* ((g zar) (h doz))
         _ <* ((i fizz) (h buzz))
         ** ((j no) (k exit)))

`seq` is syntactic sugar for `>>=`.

`<-` binds the value the expression to its right of becomes determined to to the
variable on its left. The following lines are then evaluated. `_` as a variable
names ignores the value. `<*` instead of `<-` wraps the expressions surrounded
by parentheses to its right with `(return (begin ...))`. `**` instead of a
variable name is equivalent to `_ <*`.

## Scheduler

To run a program using async, you must start the scheduler using
`(scheduler-start!)`. The call will block until a call to `(scheduler-stop!)`.

### scheduler-start!

`(scheduler-start! #!key (on-stop (lambda () #t)))`

`scheduler-start!` starts the scheduler which calls procedures bound to
deferreds when they become determined. It calls `on-stop` after the scheduler
has been stopped.

### scheduler-stop!

`(scheduler-stop!)`

`scheduler-stop!` signals the scheduler to stop. The scheduler will stop after
the procedure that called `scheduler-stop!` returns, if that procedure was
executed by the scheduler due to it being bound by a function, or after
`scheduler-stop!` obtains the scheduler mutex, if it is called externally.

# Example

There is a small example script in `example.scm`.

# Common Problems

## Why is my program aborting with `(exn arity)`?

This will happen with the following code:

    (bind (return 5)
          (lambda ()
            (return "foo")))

The reason is that the procedure bound to `(return 5)` expects no arguments,
when all procedures used as the second argument to bound should take one
argument, the value to which the deferred they are bound to becomes determined,
and return one value of type deferred.

The scheduler tries to call `(lambda () (return "foo"))` with `5` but cannot.

## Why is my program aborting with "Expected deferred result."?

This will happen with the following code:

    (bind (return 5)
          (lambda (x)
            (+ x 7)))

The reason is that procedures bound to deferreds must themselves return
deferreds. Why is this? Simply put, because it makes composition of deferreds
cleaner.

# Cleanliness

I hacked this out in a couple hours, I haven't tried to refactor it to make it
cleaner or test it very much.

# Performance

I have not tested performance. The scheduler is very naive -- it simply executes
everything that has become ready in the last timestep as soon as possible. I
have not attempted to make any optimizations.

# Portability

This code should run, with limited modification, on any R5RS-compliant Scheme
distribution supporting SRFI-18 (threading) and SRFI-35 (conditions). In fact,
this version was ported from CHICKEN to Guile with minimal effort.

[1]: https://realworldocaml.org/v1/en/html/concurrent-programming-with-async.html
[2]: https://github.com/janestreet/async_kernel/tree/master/src

// vim: set tw=80 :
