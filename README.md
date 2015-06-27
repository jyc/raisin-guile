
# Async for CHICKEN Scheme

This repository contains some experiments on replicating the interface of
OCaml's [Async][1] library in CHICKEN Scheme. It does not go anywhere near the
scope of Async -- Async ([seems to][2]) encompasses its own garbage collector,
scheduler, and, importantly, integrated I/O modules. All I have implemented is
the basic Ivar/Deferred structures, a naive scheduler, and an `after` procedure.

I made it for fun and to see how difficult such a thing would be.

# Procedures

## ivar

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

## deferred

A deferred represents a value that will become available, or determined, at some
indeterminate time in the future.

Deferreds are created from and bound to ivars using the `ivar-read` procedure
and become determined once the ivar to which they are bound is filled.

### bind

`(bind d f)`

`bind` binds a procedure `f` itself returning a deferred can be bound to a
deferred `d` using . This calls `f` with the value that `d` becomes determined
to.  The call to `bind` returns a deferred that becomes determined once the
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

## scheduler-stop!

`(scheduler-stop!)`

`scheduler-stop!` signals the scheduler to stop. The scheduler will stop after
it has finished executing all of the ready procedures.

# Example

There is a small example script in `example.scm`.

# Cleanliness

I hacked this out in a couple hours, I haven't tried to refactor it to make it
cleaner or test it very much.

# Performance

I have not tested performance. The scheduler is very naive -- it simply executes
everything that has become ready in the last timestep  as soon as possible. It
uses CHICKEN's SRFI-18 interface rather than accessing its low-level scheduler
primitives. I have not attempted to make any optimizations.

# Portability

This code should run, with limited modification, on any R5RS-compliant Scheme
distribution supporting SRFI-18 (threading) and SRFI-35 (conditions). I use
Chicken's `ir-macro-transformer` to define exceptions. That could easily be
stripped out, as well as the use of Chicken's `abort` procedure.

[1]: https://realworldocaml.org/v1/en/html/concurrent-programming-with-async.html
[2]: https://github.com/janestreet/async_kernel/tree/master/src
// vim: set tw=80 :
