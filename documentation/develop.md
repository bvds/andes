# Develop with Andes #

## Install ##

Probably the easiest way to develop with lisp is by using
emacs (on OS X, you can use [Aquamacs](http://aquamacs.org)) and [slime](https://common-lisp.net/project/slime/).
I have 
[installed slime from github](https://common-lisp.net/project/slime/doc/html/Installation.html#Installing-from-Git)
and put it into `/usr/local`.

You will also need to install [ImageMagick](http://www.imagemagick.org).
To verify that ImageMagick is installed use:  `identify --version`.



## The main Andes Solution-Graph-Generator commands. ##

Starting up and loading the System.

Immediately after starting up lisp:

    (rkb) ; load (or reload) Knowledge Base and exercises

Solving a problem:

    (s kt1a)            ; solve named problem

Examining results for current problem after solving:

    (ps)                  ; print report of each solution found 
    (pg)                  ; print bubble graph	
    (pgn N)               ; print graph node N -- shows PSM graph (maybe long) 
    (pgv)                 ; print index of all variables
    (pge)                 ; print index of all equations 
    (pe)                  ; print equation subsets constituting a solution
    (pep N)               ; print contents of equation set numbered N
  
Controlling trace output -- Set to `T` for on, `NIL` for off:
  
    (watch op1 op2...)    ;; turn on tracing within named operators
    (unwatch op1 op2...)  ;; remove given ops from the trace list
    (unwatch)             ;; remove all ops from the trace list
    (setf *actions* T)         ;; print verbose trace of all problem solving
    (setf *debug* T)           ;; print debug output inside operators
    (setf *s-print-steps* NIL) ;; suppress printing of substep results

Writing problem files

    (spf)                 ; store .prb problem file for current problem
    (make-prbs)           ; make all problems w/features working & andes2
    (make-prbs 'kinematics 'statics ...) 
			  ; make all working problems w/specified feature tags

Force recompilation of all sgg sources (shouldn't be necessary, but
harmless)

    (ra)                  ; recompile-all

## Solver ##

Andes lisp communicates with the solver through a socket.
Logging of this communcation can be turned on via:

    (setf *solver-logging* t)

This will create a log file `Solver.log`.
See [`Algebra/solver.cl`](../Algebra/solver.cl) for details.
