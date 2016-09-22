> find n epsilon bound = [(x,y,z) | x <- [1 .. bound], y <- [x+1 .. bound], z <- [y+1 .. x + y - 1], abs(z ** n - x ** n - y ** n) <= epsilon]
> pisagor n = [(x,y,z) | x <- [1..n], y <- [x+1..n], z <- [y+1..x+y-1], x /= y, x*x + y*y == z*z, (null $ take 1 [k | k <- [2..div x 2], 0 == mod x k, 0 == mod y k, 0 == mod z k]) == True ]
> find0 bound = pisagor bound
> find0' bound = find 2 0.01 bound
> find1 n = find n 1.01 20
> find2 n epsilon = find n epsilon 20

> error2 n x y z = z**n - x**n - y**n

Note: There is Prelude.error. Hence the 2.

The error goes up with the power..

> error3 x y z n = error2 n x y z


error3 3 4 5 2
error3 3 4 5 3
error3 3 4 5 4
error3 3 4 5 22
etc..

The rest is historical, results, ... Look for "epsilon bound"


GHCi, version 7.8.3: http://www.haskell.org/ghc/  :? for help
Loading package ghc-prim ... linking ... done.
Loading package integer-gmp ... linking ... done.
Loading package base ... linking ... done.
Prelude> import numeric-prelude

<interactive>:1:8: parse error on input ‘numeric’
Prelude> import NumericPrelude

<no location info>:
    Could not find module ‘NumericPrelude’
    It is not a module in the current program, or in any known package.
Prelude> :s path
IO error:  "path" does not exist
Prelude> :s
syntax:  :script <filename>
Prelude> :show
options currently set: none.
base language is: Haskell2010
with the following modifiers:
  -XNoDatatypeContexts
  -XNondecreasingIndentation
GHCi-specific dynamic flag settings:
other dynamic, non-language, flag settings:
  -fimplicit-import-qualified
warning settings:
Prelude> :h
 Commands available from the prompt:

   <statement>                 evaluate/run <statement>
   :                           repeat last command
   :{\n ..lines.. \n:}\n       multiline command
   :add [*]<module> ...        add module(s) to the current target set
   :browse[!] [[*]<mod>]       display the names defined by module <mod>
                               (!: more details; *: all top-level names)
   :cd <dir>                   change directory to <dir>
   :cmd <expr>                 run the commands returned by <expr>::IO String
   :complete <dom> [<rng>] <s> list completions for partial input string
   :ctags[!] [<file>]          create tags file for Vi (default: "tags")
                               (!: use regex instead of line number)
   :def <cmd> <expr>           define command :<cmd> (later defined command has
                               precedence, ::<cmd> is always a builtin command)
   :edit <file>                edit file
   :edit                       edit last module
   :etags [<file>]             create tags file for Emacs (default: "TAGS")
   :help, :?                   display this list of commands
   :info[!] [<name> ...]       display information about the given names
                               (!: do not filter instances)
   :issafe [<mod>]             display safe haskell information of module <mod>
   :kind[!] <type>             show the kind of <type>
                               (!: also print the normalised type)
   :load [*]<module> ...       load module(s) and their dependents
   :main [<arguments> ...]     run the main function with the given arguments
   :module [+/-] [*]<mod> ...  set the context for expression evaluation
   :quit                       exit GHCi
   :reload                     reload the current module set
   :run function [<arguments> ...] run the function with the given arguments
   :script <filename>          run the script <filename>
   :type <expr>                show the type of <expr>
   :undef <cmd>                undefine user-defined command :<cmd>
   :!<command>                 run the shell command <command>

 -- Commands for debugging:

   :abandon                    at a breakpoint, abandon current computation
   :back                       go back in the history (after :trace)
   :break [<mod>] <l> [<col>]  set a breakpoint at the specified location
   :break <name>               set a breakpoint on the specified function
   :continue                   resume after a breakpoint
   :delete <number>            delete the specified breakpoint
   :delete *                   delete all breakpoints
   :force <expr>               print <expr>, forcing unevaluated parts
   :forward                    go forward in the history (after :back)
   :history [<n>]              after :trace, show the execution history
   :list                       show the source code around current breakpoint
   :list <identifier>          show the source code for <identifier>
   :list [<module>] <line>     show the source code around line number <line>
   :print [<name> ...]         prints a value without forcing its computation
   :sprint [<name> ...]        simplifed version of :print
   :step                       single-step after stopping at a breakpoint
   :step <expr>                single-step into <expr>
   :steplocal                  single-step within the current top-level binding
   :stepmodule                 single-step restricted to the current module
   :trace                      trace after stopping at a breakpoint
   :trace <expr>               evaluate <expr> with tracing on (see :history)

 -- Commands for changing settings:

   :set <option> ...           set options
   :seti <option> ...          set options for interactive evaluation only
   :set args <arg> ...         set the arguments returned by System.getArgs
   :set prog <progname>        set the value returned by System.getProgName
   :set prompt <prompt>        set the prompt used in GHCi
   :set prompt2 <prompt>       set the continuation prompt used in GHCi
   :set editor <cmd>           set the command used for :edit
   :set stop [<n>] <cmd>       set the command to run when a breakpoint is hit
   :unset <option> ...         unset options

  Options for ':set' and ':unset':

    +m            allow multiline commands
    +r            revert top-level expressions after each evaluation
    +s            print timing/memory stats after each evaluation
    +t            print type after evaluation
    -<flags>      most GHC command line flags can also be set here
                         (eg. -v2, -XFlexibleInstances, etc.)
                    for GHCi-specific flags, see User's Guide,
                    Flag reference, Interactive-mode options

 -- Commands for displaying information:

   :show bindings              show the current bindings made at the prompt
   :show breaks                show the active breakpoints
   :show context               show the breakpoint context
   :show imports               show the current imports
   :show linker                show current linker state
   :show modules               show the currently loaded modules
   :show packages              show the currently active package flags
   :show paths                 show the currently active search paths
   :show language              show the currently active language flags
   :show <setting>             show value of <setting>, which is one of
                                  [args, prog, prompt, editor, stop]
   :showi language             show language flags for interactive evaluation

Prelude> :show packages
active package flags: none
Prelude> :show modules
Prelude> :show paths
current working directory: 
  C:\Users\bbasaran\documents\fp\wx
module import search paths:
  .
Prelude> 5 * 5 * 5
125
Prelude> root

<interactive>:18:1: Not in scope: ‘root’
Prelude> sqrt

<interactive>:19:1:
    No instance for (Show (a0 -> a0)) arising from a use of ‘print’
    In a stmt of an interactive GHCi command: print it
Prelude> :t sqrt
sqrt :: Floating a => a -> a
Prelude> :t pow

<interactive>:1:1: Not in scope: ‘pow’
Prelude> :t exp
exp :: Floating a => a -> a
Prelude> exp 10 2

<interactive>:23:1:
    Could not deduce (Num a0) arising from the ambiguity check for ‘it’
    from the context (Num a, Floating (a -> t))
      bound by the inferred type for ‘it’:
                 (Num a, Floating (a -> t)) => t
      at <interactive>:23:1-8
    The type variable ‘a0’ is ambiguous
    When checking that ‘it’
      has the inferred type ‘forall a t. (Num a, Floating (a -> t)) => t’
    Probable cause: the inferred type is ambiguous
Prelude> exp 10
22026.465794806718
Prelude> sqrt 10
3.1622776601683795
Prelude> 3 ** 10
59049.0
Prelude> logBase 9 3
0.5
Prelude> logBase 10 100
2.0
Prelude> 10 ** 2
100.0
Prelude> 10 ** 10
1.0e10
Prelude> 10 ** 100
1.0000000000000002e100
Prelude> logBase 2 1024
10.0
Prelude> 3 ** 3
27.0

Prelude> :t \n epsilon bound -> [(x,y,z,n) | x <- [1 .. bound], y <- [x .. bound], z <- [y .. x + y], z ** n - x ** n - y ** n < epsilon]
\n epsilon bound -> [(x,y,z,n) | x <- [1 .. bound], y <- [x .. bound], z <- [y .. x + y], z ** n - x ** n - y ** n < epsilon]
  :: (Ord t, Floating t, Enum t) => t -> t -> t -> [(t, t, t, t)]

Prelude> (\n epsilon bound -> [(x,y,z,n) | x <- [1 .. bound], y <- [x .. bound], z <- [y .. x + y], z ** n - x ** n - y ** n < epsilon]) 2 0.01 10
[(1.0,1.0,1.0,2.0),(1.0,2.0,2.0,2.0),(1.0,3.0,3.0,2.0),(1.0,4.0,4.0,2.0),(1.0,5.0,5.0,2.0),(1.0,6.0,6.0,2.0),(1.0,7.0,7.0,2.0),(1.0,8.0,8.0,2.0),(1.0,9.0,9.0,2.0),(1.0,10.0,10.0,2.0),(2.0,2.0,2.0,2.0),(2.0,3.0,3.0,2.0),(2.0,4.0,4.0,2.0),(2.0,5.0,5.0,2.0),(2.0,6.0,6.0,2.0),(2.0,7.0,7.0,2.0),(2.0,8.0,8.0,2.0),(2.0,9.0,9.0,2.0),(2.0,10.0,10.0,2.0),(3.0,3.0,3.0,2.0),(3.0,3.0,4.0,2.0),(3.0,4.0,4.0,2.0),(3.0,4.0,5.0,2.0),(3.0,5.0,5.0,2.0),(3.0,6.0,6.0,2.0),(3.0,7.0,7.0,2.0),(3.0,8.0,8.0,2.0),(3.0,9.0,9.0,2.0),(3.0,10.0,10.0,2.0),(4.0,4.0,4.0,2.0),(4.0,4.0,5.0,2.0),(4.0,5.0,5.0,2.0),(4.0,5.0,6.0,2.0),(4.0,6.0,6.0,2.0),(4.0,6.0,7.0,2.0),(4.0,7.0,7.0,2.0),(4.0,7.0,8.0,2.0),(4.0,8.0,8.0,2.0),(4.0,9.0,9.0,2.0),(4.0,10.0,10.0,2.0),(5.0,5.0,5.0,2.0),(5.0,5.0,6.0,2.0),(5.0,5.0,7.0,2.0),(5.0,6.0,6.0,2.0),(5.0,6.0,7.0,2.0),(5.0,7.0,7.0,2.0),(5.0,7.0,8.0,2.0),(5.0,8.0,8.0,2.0),(5.0,8.0,9.0,2.0),(5.0,9.0,9.0,2.0),(5.0,9.0,10.0,2.0),(5.0,10.0,10.0,2.0),(5.0,10.0,11.0,2.0),(6.0,6.0,6.0,2.0),(6.0,6.0,7.0,2.0),(6.0,6.0,8.0,2.0),(6.0,7.0,7.0,2.0),(6.0,7.0,8.0,2.0),(6.0,7.0,9.0,2.0),(6.0,8.0,8.0,2.0),(6.0,8.0,9.0,2.0),(6.0,8.0,10.0,2.0),(6.0,9.0,9.0,2.0),(6.0,9.0,10.0,2.0),(6.0,10.0,10.0,2.0),(6.0,10.0,11.0,2.0),(7.0,7.0,7.0,2.0),(7.0,7.0,8.0,2.0),(7.0,7.0,9.0,2.0),(7.0,8.0,8.0,2.0),(7.0,8.0,9.0,2.0),(7.0,8.0,10.0,2.0),(7.0,9.0,9.0,2.0),(7.0,9.0,10.0,2.0),(7.0,9.0,11.0,2.0),(7.0,10.0,10.0,2.0),(7.0,10.0,11.0,2.0),(7.0,10.0,12.0,2.0),(8.0,8.0,8.0,2.0),(8.0,8.0,9.0,2.0),(8.0,8.0,10.0,2.0),(8.0,8.0,11.0,2.0),(8.0,9.0,9.0,2.0),(8.0,9.0,10.0,2.0),(8.0,9.0,11.0,2.0),(8.0,9.0,12.0,2.0),(8.0,10.0,10.0,2.0),(8.0,10.0,11.0,2.0),(8.0,10.0,12.0,2.0),(9.0,9.0,9.0,2.0),(9.0,9.0,10.0,2.0),(9.0,9.0,11.0,2.0),(9.0,9.0,12.0,2.0),(9.0,10.0,10.0,2.0),(9.0,10.0,11.0,2.0),(9.0,10.0,12.0,2.0),(9.0,10.0,13.0,2.0),(10.0,10.0,10.0,2.0),(10.0,10.0,11.0,2.0),(10.0,10.0,12.0,2.0),(10.0,10.0,13.0,2.0),(10.0,10.0,14.0,2.0)]
Prelude> (\n epsilon bound -> [(x,y,z) | x <- [1 .. bound], y <- [x+1 .. bound], z <- [y .. x + y], z ** n - x ** n - y ** n < epsilon]) 2 0.01 10
[(1.0,2.0,2.0),(1.0,3.0,3.0),(1.0,4.0,4.0),(1.0,5.0,5.0),(1.0,6.0,6.0),(1.0,7.0,7.0),(1.0,8.0,8.0),(1.0,9.0,9.0),(1.0,10.0,10.0),(2.0,3.0,3.0),(2.0,4.0,4.0),(2.0,5.0,5.0),(2.0,6.0,6.0),(2.0,7.0,7.0),(2.0,8.0,8.0),(2.0,9.0,9.0),(2.0,10.0,10.0),(3.0,4.0,4.0),(3.0,4.0,5.0),(3.0,5.0,5.0),(3.0,6.0,6.0),(3.0,7.0,7.0),(3.0,8.0,8.0),(3.0,9.0,9.0),(3.0,10.0,10.0),(4.0,5.0,5.0),(4.0,5.0,6.0),(4.0,6.0,6.0),(4.0,6.0,7.0),(4.0,7.0,7.0),(4.0,7.0,8.0),(4.0,8.0,8.0),(4.0,9.0,9.0),(4.0,10.0,10.0),(5.0,6.0,6.0),(5.0,6.0,7.0),(5.0,7.0,7.0),(5.0,7.0,8.0),(5.0,8.0,8.0),(5.0,8.0,9.0),(5.0,9.0,9.0),(5.0,9.0,10.0),(5.0,10.0,10.0),(5.0,10.0,11.0),(6.0,7.0,7.0),(6.0,7.0,8.0),(6.0,7.0,9.0),(6.0,8.0,8.0),(6.0,8.0,9.0),(6.0,8.0,10.0),(6.0,9.0,9.0),(6.0,9.0,10.0),(6.0,10.0,10.0),(6.0,10.0,11.0),(7.0,8.0,8.0),(7.0,8.0,9.0),(7.0,8.0,10.0),(7.0,9.0,9.0),(7.0,9.0,10.0),(7.0,9.0,11.0),(7.0,10.0,10.0),(7.0,10.0,11.0),(7.0,10.0,12.0),(8.0,9.0,9.0),(8.0,9.0,10.0),(8.0,9.0,11.0),(8.0,9.0,12.0),(8.0,10.0,10.0),(8.0,10.0,11.0),(8.0,10.0,12.0),(9.0,10.0,10.0),(9.0,10.0,11.0),(9.0,10.0,12.0),(9.0,10.0,13.0)]
Prelude> 3 ** 3 - 2 ** 3
19.0
Prelude> 27 - 8
19
Prelude> (\n epsilon bound -> [(x,y,z) | x <- [1 .. bound], y <- [x+1 .. bound], z <- [y .. x + y], z ** n - x ** n - y ** n < epsilon]) 2 0.0001 10
[(1.0,2.0,2.0),(1.0,3.0,3.0),(1.0,4.0,4.0),(1.0,5.0,5.0),(1.0,6.0,6.0),(1.0,7.0,7.0),(1.0,8.0,8.0),(1.0,9.0,9.0),(1.0,10.0,10.0),(2.0,3.0,3.0),(2.0,4.0,4.0),(2.0,5.0,5.0),(2.0,6.0,6.0),(2.0,7.0,7.0),(2.0,8.0,8.0),(2.0,9.0,9.0),(2.0,10.0,10.0),(3.0,4.0,4.0),(3.0,4.0,5.0),(3.0,5.0,5.0),(3.0,6.0,6.0),(3.0,7.0,7.0),(3.0,8.0,8.0),(3.0,9.0,9.0),(3.0,10.0,10.0),(4.0,5.0,5.0),(4.0,5.0,6.0),(4.0,6.0,6.0),(4.0,6.0,7.0),(4.0,7.0,7.0),(4.0,7.0,8.0),(4.0,8.0,8.0),(4.0,9.0,9.0),(4.0,10.0,10.0),(5.0,6.0,6.0),(5.0,6.0,7.0),(5.0,7.0,7.0),(5.0,7.0,8.0),(5.0,8.0,8.0),(5.0,8.0,9.0),(5.0,9.0,9.0),(5.0,9.0,10.0),(5.0,10.0,10.0),(5.0,10.0,11.0),(6.0,7.0,7.0),(6.0,7.0,8.0),(6.0,7.0,9.0),(6.0,8.0,8.0),(6.0,8.0,9.0),(6.0,8.0,10.0),(6.0,9.0,9.0),(6.0,9.0,10.0),(6.0,10.0,10.0),(6.0,10.0,11.0),(7.0,8.0,8.0),(7.0,8.0,9.0),(7.0,8.0,10.0),(7.0,9.0,9.0),(7.0,9.0,10.0),(7.0,9.0,11.0),(7.0,10.0,10.0),(7.0,10.0,11.0),(7.0,10.0,12.0),(8.0,9.0,9.0),(8.0,9.0,10.0),(8.0,9.0,11.0),(8.0,9.0,12.0),(8.0,10.0,10.0),(8.0,10.0,11.0),(8.0,10.0,12.0),(9.0,10.0,10.0),(9.0,10.0,11.0),(9.0,10.0,12.0),(9.0,10.0,13.0)]
Prelude> 1 * 3 * 5 < 3
False
Prelude> 1 * 3 * 5 < 6
False
Prelude> abs ( - 1 )
1
Prelude> abs (-10)
10
Prelude> abs -10

<interactive>:46:1:
    No instance for (Show (a0 -> a0)) arising from a use of ‘print’
    In a stmt of an interactive GHCi command: print it
Prelude> abs - 10

<interactive>:47:1:
    No instance for (Show (a0 -> a0)) arising from a use of ‘print’
    In a stmt of an interactive GHCi command: print it
Prelude> abs (- 10)
10
Prelude> (\n epsilon bound -> [(x,y,z) | x <- [1 .. bound], y <- [x+1 .. bound], z <- [y .. x + y], abs(z ** n - x ** n - y ** n) < epsilon]) 2 0.0001 10
[(3.0,4.0,5.0),(6.0,8.0,10.0)]
Prelude> (\n epsilon bound -> [(x,y,z) | x <- [1 .. bound], y <- [x+1 .. bound], z <- [y .. x + y], abs(z ** n - x ** n - y ** n) < epsilon]) 3 0.0001 10
[]
Prelude> (\n epsilon bound -> [(x,y,z) | x <- [1 .. bound], y <- [x+1 .. bound], z <- [y .. x + y], abs(z ** n - x ** n - y ** n) < epsilon]) 3 0.01 10
[]
Prelude> (\n epsilon bound -> [(x,y,z) | x <- [1 .. bound], y <- [x+1 .. bound], z <- [y .. x + y], abs(z ** n - x ** n - y ** n) < epsilon]) 3 0.1 10
[]
Prelude> (\n epsilon bound -> [(x,y,z) | x <- [1 .. bound], y <- [x+1 .. bound], z <- [y .. x + y], abs(z ** n - x ** n - y ** n) < epsilon]) 4 0.1 10
[]
Prelude> (\n epsilon bound -> [(x,y,z) | x <- [1 .. bound], y <- [x+1 .. bound], z <- [y .. x + y], abs(z ** n - x ** n - y ** n) < epsilon]) 5 0.1 10
[]
Prelude> (\n epsilon bound -> [(x,y,z) | x <- [1 .. bound], y <- [x+1 .. bound], z <- [y .. x + y], abs(z ** n - x ** n - y ** n) < epsilon]) 5 1 10
[]
Prelude> (\n epsilon bound -> [(x,y,z) | x <- [1 .. bound], y <- [x+1 .. bound], z <- [y .. x + y], abs(z ** n - x ** n - y ** n) < epsilon]) 10 1 10
[]
Prelude> (\n epsilon bound -> [(x,y,z) | x <- [1 .. bound], y <- [x+1 .. bound], z <- [y .. x + y], abs(z ** n - x ** n - y ** n) < epsilon]) 2 1 10
[(3.0,4.0,5.0),(6.0,8.0,10.0)]
Prelude> (\n epsilon bound -> [(x,y,z) | x <- [1 .. bound], y <- [x+1 .. bound], z <- [y .. x + y], abs(z ** n - x ** n - y ** n) < epsilon]) 2 1 100
[(3.0,4.0,5.0),(5.0,12.0,13.0),(6.0,8.0,10.0),(7.0,24.0,25.0),(8.0,15.0,17.0),(9.0,12.0,15.0),(9.0,40.0,41.0),(10.0,24.0,26.0),(11.0,60.0,61.0),(12.0,16.0,20.0),(12.0,35.0,37.0),(13.0,84.0,85.0),(14.0,48.0,50.0),(15.0,20.0,25.0),(15.0,36.0,39.0),(16.0,30.0,34.0),(16.0,63.0,65.0),(18.0,24.0,30.0),(18.0,80.0,82.0),(20.0,21.0,29.0),(20.0,48.0,52.0),(20.0,99.0,101.0),(21.0,28.0,35.0),(21.0,72.0,75.0),(24.0,32.0,40.0),(24.0,45.0,51.0),(24.0,70.0,74.0),(25.0,60.0,65.0),(27.0,36.0,45.0),(28.0,45.0,53.0),(28.0,96.0,100.0),(30.0,40.0,50.0),(30.0,72.0,78.0),(32.0,60.0,68.0),(33.0,44.0,55.0),(33.0,56.0,65.0),(35.0,84.0,91.0),(36.0,48.0,60.0),(36.0,77.0,85.0),(39.0,52.0,65.0),(39.0,80.0,89.0),(40.0,42.0,58.0),(40.0,75.0,85.0),(40.0,96.0,104.0),(42.0,56.0,70.0),(45.0,60.0,75.0),(48.0,55.0,73.0),(48.0,64.0,80.0),(48.0,90.0,102.0),(51.0,68.0,85.0),(54.0,72.0,90.0),(56.0,90.0,106.0),(57.0,76.0,95.0),(60.0,63.0,87.0),(60.0,80.0,100.0),(60.0,91.0,109.0),(63.0,84.0,105.0),(65.0,72.0,97.0),(66.0,88.0,110.0),(69.0,92.0,115.0),(72.0,96.0,120.0),(75.0,100.0,125.0),(80.0,84.0,116.0)]

Prelude> length $ (\n epsilon bound -> [(x,y,z) | x <- [1 .. bound], y <- [x+1 .. bound], z <- [y .. x + y], abs(z ** n - x ** n - y ** n) < epsilon]) 2 1 100
63
Prelude> length $ (\n epsilon bound -> [(x,y,z) | x <- [1 .. bound], y <- [x+1 .. bound], z <- [y .. x + y], abs(z ** n - x ** n - y ** n) < epsilon]) 2 0.001 100
63
Prelude> length $ (\n epsilon bound -> [(x,y,z) | x <- [1 .. bound], y <- [x+1 .. bound], z <- [y .. x + y], abs(z ** n - x ** n - y ** n) < epsilon]) 2 10 100
986
Prelude> length $ (\n epsilon bound -> [(x,y,z) | x <- [1 .. bound], y <- [x+1 .. bound], z <- [y .. x + y], abs(z ** n - x ** n - y ** n) < epsilon]) 3 10 100
209
Prelude> length $ (\n epsilon bound -> [(x,y,z) | x <- [1 .. bound], y <- [x+1 .. bound], z <- [y .. x + y], abs(z ** n - x ** n - y ** n) < epsilon]) 3 9 100
209
Prelude> length $ (\n epsilon bound -> [(x,y,z) | x <- [1 .. bound], y <- [x+1 .. bound], z <- [y .. x + y], abs(z ** n - x ** n - y ** n) < epsilon]) 3 8 100
105
Prelude> length $ (\n epsilon bound -> [(x,y,z) | x <- [1 .. bound], y <- [x+1 .. bound], z <- [y .. x + y], abs(z ** n - x ** n - y ** n) < epsilon]) 3 7 100
105
Prelude> length $ (\n epsilon bound -> [(x,y,z) | x <- [1 .. bound], y <- [x+1 .. bound], z <- [y .. x + y], abs(z ** n - x ** n - y ** n) < epsilon]) 3 6 100
104
Prelude> length $ (\n epsilon bound -> [(x,y,z) | x <- [1 .. bound], y <- [x+1 .. bound], z <- [y .. x + y], abs(z ** n - x ** n - y ** n) < epsilon]) 3 5 100
104
Prelude> length $ (\n epsilon bound -> [(x,y,z) | x <- [1 .. bound], y <- [x+1 .. bound], z <- [y .. x + y], abs(z ** n - x ** n - y ** n) < epsilon]) 3 3 100
104
Prelude> length $ (\n epsilon bound -> [(x,y,z) | x <- [1 .. bound], y <- [x+1 .. bound], z <- [y .. x + y], abs(z ** n - x ** n - y ** n) < epsilon]) 3 2 100
102
Prelude> length $ (\n epsilon bound -> [(x,y,z) | x <- [1 .. bound], y <- [x+1 .. bound], z <- [y .. x + y], abs(z ** n - x ** n - y ** n) < epsilon]) 3 1 100
0
Prelude> length $ (\n epsilon bound -> [(x,y,z) | x <- [1 .. bound], y <- [x+1 .. bound], z <- [y .. x + y], abs(z ** n - x ** n - y ** n) < epsilon]) 3 1.5 100
102
Prelude> length $ (\n epsilon bound -> [(x,y,z) | x <- [1 .. bound], y <- [x+1 .. bound], z <- [y .. x + y], abs(z ** n - x ** n - y ** n) < epsilon]) 3 1.25 100
102
Prelude> length $ (\n epsilon bound -> [(x,y,z) | x <- [1 .. bound], y <- [x+1 .. bound], z <- [y .. x + y], abs(z ** n - x ** n - y ** n) < epsilon]) 3 1.001 100
102
Prelude> length $ (\n epsilon bound -> [(x,y,z) | x <- [1 .. bound], y <- [x+1 .. bound], z <- [y .. x + y], abs(z ** n - x ** n - y ** n) < epsilon]) 3 1.001 10
11
Prelude> (\n epsilon bound -> [(x,y,z) | x <- [1 .. bound], y <- [x+1 .. bound], z <- [y .. x + y], abs(z ** n - x ** n - y ** n) < epsilon]) 3 1.001 10
[(1.0,2.0,2.0),(1.0,3.0,3.0),(1.0,4.0,4.0),(1.0,5.0,5.0),(1.0,6.0,6.0),(1.0,7.0,7.0),(1.0,8.0,8.0),(1.0,9.0,9.0),(1.0,10.0,10.0),(6.0,8.0,9.0),(9.0,10.0,12.0)]
Prelude> (\n epsilon bound -> [(x,y,z) | x <- [1 .. bound], y <- [x+1 .. bound], z <- [y+1 .. x + y], abs(z ** n - x ** n - y ** n) < epsilon]) 3 1.001 10
[(6.0,8.0,9.0),(9.0,10.0,12.0)]
Prelude> (\n epsilon bound -> [(x,y,z) | x <- [1 .. bound], y <- [x+1 .. bound], z <- [y+1 .. x + y], abs(z ** n - x ** n - y ** n) < epsilon]) 3 1.0001 10
[(6.0,8.0,9.0),(9.0,10.0,12.0)]
Prelude> (\n epsilon bound -> [(x,y,z) | x <- [1 .. bound], y <- [x+1 .. bound], z <- [y+1 .. x + y], abs(z ** n - x ** n - y ** n) <= epsilon]) 3 1.0001 10
[(6.0,8.0,9.0),(9.0,10.0,12.0)]
Prelude> (\n epsilon bound -> [(x,y,z) | x <- [1 .. bound], y <- [x+1 .. bound], z <- [y+1 .. x + y], abs(z ** n - x ** n - y ** n) <= epsilon]) 3 1.0 10
[(6.0,8.0,9.0),(9.0,10.0,12.0)]
Prelude> (\n epsilon bound -> [(x,y,z) | x <- [1 .. bound], y <- [x+1 .. bound], z <- [y+1 .. x + y], abs(z ** n - x ** n - y ** n) <= epsilon]) 3 0.99 10
[]
Prelude> (\n epsilon bound -> [(x,y,z) | x <- [1 .. bound], y <- [x+1 .. bound], z <- [y+1 .. x + y], abs(z ** n - x ** n - y ** n) <= epsilon]) 3 1 10
[(6.0,8.0,9.0),(9.0,10.0,12.0)]
Prelude> 9 ** 3
729.0
Prelude> 8 ** 3
512.0
Prelude> 6 ** 3
216.0
Prelude> (\n z x y -> (z**n - x**n - y**n)) 3 9 8 6
1.0
Prelude> (\n x y z -> (z**n - x**n - y**n)) 3 6 8 9
1.0
Prelude> (\n x y z -> (z**n - x**n - y**n)) 3 9 10 12
-1.0
Prelude> (\n epsilon bound -> [(x,y,z) | x <- [1 .. bound], y <- [x+1 .. bound], z <- [y+1 .. x + y], abs(z ** n - x ** n - y ** n) <= epsilon]) 4 1 10
[]
Prelude> (\n epsilon bound -> [(x,y,z) | x <- [1 .. bound], y <- [x+1 .. bound], z <- [y+1 .. x + y], abs(z ** n - x ** n - y ** n) <= epsilon]) 4 2 10
[]
Prelude> (\n epsilon bound -> [(x,y,z) | x <- [1 .. bound], y <- [x+1 .. bound], z <- [y+1 .. x + y], abs(z ** n - x ** n - y ** n) <= epsilon]) 4 3 10
[]
Prelude> (\n epsilon bound -> [(x,y,z) | x <- [1 .. bound], y <- [x+1 .. bound], z <- [y+1 .. x + y], abs(z ** n - x ** n - y ** n) <= epsilon]) 4 3 100
[]
Prelude> (\n epsilon bound -> [(x,y,z) | x <- [1 .. bound], y <- [x+1 .. bound], z <- [y+1 .. x + y], abs(z ** n - x ** n - y ** n) <= epsilon]) 4 3 200
[]
Prelude> (\n epsilon bound -> [(x,y,z) | x <- [1 .. bound], y <- [x+1 .. bound], z <- [y+1 .. x + y], abs(z ** n - x ** n - y ** n) <= epsilon]) 4 10 100
[]
Prelude> (\n epsilon bound -> [(x,y,z) | x <- [1 .. bound], y <- [x+1 .. bound], z <- [y+1 .. x + y], abs(z ** n - x ** n - y ** n) <= epsilon]) 4 20 100
[]
Prelude> (\n epsilon bound -> [(x,y,z) | x <- [1 .. bound], y <- [x+1 .. bound], z <- [y+1 .. x + y], abs(z ** n - x ** n - y ** n) <= epsilon]) 4 100 100
[(1.0,2.0,3.0),(7.0,8.0,9.0),(21.0,36.0,37.0)]
Prelude> (\n x y z -> (z**n - x**n - y**n)) 4 1 2 3
64.0
Prelude> (\n x y z -> (z**n - x**n - y**n)) 4 7 8 9
64.0
Prelude> (\n x y z -> (z**n - x**n - y**n)) 4 21 36 37
64.0
Prelude> (\n epsilon bound -> [(x,y,z) | x <- [1 .. bound], y <- [x+1 .. bound], z <- [y+1 .. x + y], abs(z ** n - x ** n - y ** n) <= epsilon]) 4 64 37
[(1.0,2.0,3.0),(7.0,8.0,9.0),(21.0,36.0,37.0)]
Prelude> (\n epsilon bound -> [(x,y,z) | x <- [1 .. bound], y <- [x+1 .. bound], z <- [y+1 .. x + y], abs(z ** n - x ** n - y ** n) <= epsilon]) 4 64 100
[(1.0,2.0,3.0),(7.0,8.0,9.0),(21.0,36.0,37.0)]
Prelude> (\n epsilon bound -> [(x,y,z) | x <- [1 .. bound], y <- [x+1 .. bound], z <- [y+1 .. x + y], abs(z ** n - x ** n - y ** n) <= epsilon]) 4 64 200
[(1.0,2.0,3.0),(7.0,8.0,9.0),(21.0,36.0,37.0)]
Prelude> (\n epsilon bound -> [(x,y,z) | x <- [1 .. bound], y <- [x+1 .. bound], z <- [y+1 .. x + y], abs(z ** n - x ** n - y ** n) <= epsilon]) 4 64 400
[(1.0,2.0,3.0),(7.0,8.0,9.0),(21.0,36.0,37.0)]
Prelude> (\n epsilon bound -> [(x,y,z) | x <- [1 .. bound], y <- [x+1 .. bound], z <- [y+1 .. x + y], abs(z ** n - x ** n - y ** n) <= epsilon]) 5 200 100
[(13.0,16.0,17.0)]
Prelude> (\n epsilon bound -> [(x,y,z) | x <- [1 .. bound], y <- [x+1 .. bound], z <- [y+1 .. x + y], abs(z ** n - x ** n - y ** n) <= epsilon]) 5 100 100
[(13.0,16.0,17.0)]
Prelude> (\n x y z -> (z**n - x**n - y**n)) 5 13 16 17
-12.0
Prelude> (\n epsilon bound -> [(x,y,z) | x <- [1 .. bound], y <- [x+1 .. bound], z <- [y+1 .. x + y], abs(z ** n - x ** n - y ** n) <= epsilon]) 5 12 100
[(13.0,16.0,17.0)]
Prelude> (\n epsilon bound -> [(x,y,z) | x <- [1 .. bound], y <- [x+1 .. bound], z <- [y+1 .. x + y], abs(z ** n - x ** n - y ** n) <= epsilon]) 5 12 200
[(13.0,16.0,17.0)]
Prelude> (\n epsilon bound -> [(x,y,z) | x <- [1 .. bound], y <- [x+1 .. bound], z <- [y+1 .. x + y], abs(z ** n - x ** n - y ** n) <= epsilon]) 6 12 200
[]
Prelude> (\n epsilon bound -> [(x,y,z) | x <- [1 .. bound], y <- [x+1 .. bound], z <- [y+1 .. x + y], abs(z ** n - x ** n - y ** n) <= epsilon]) 6 100 200
[]
Prelude> (\n epsilon bound -> [(x,y,z) | x <- [1 .. bound], y <- [x+1 .. bound], z <- [y+1 .. x + y], abs(z ** n - x ** n - y ** n) <= epsilon]) 6 1000 100
[(1.0,2.0,3.0)]
Prelude> (\n x y z -> (z**n - x**n - y**n)) 6 1 2 3
664.0
Prelude> (\n epsilon bound -> [(x,y,z) | x <- [1 .. bound], y <- [x+1 .. bound], z <- [y+1 .. x + y - 1], abs(z ** n - x ** n - y ** n) <= epsilon]) 6 664 100
[]
Prelude> (\n epsilon bound -> [(x,y,z) | x <- [1 .. bound], y <- [x+1 .. bound], z <- [y+1 .. x + y - 1], abs(z ** n - x ** n - y ** n) <= epsilon]) 6 2000 100
[]
Prelude> (\n epsilon bound -> [(x,y,z) | x <- [1 .. bound], y <- [x+1 .. bound], z <- [y+1 .. x + y - 1], abs(z ** n - x ** n - y ** n) <= epsilon]) 6 3000 100
[]
Prelude> (\n epsilon bound -> [(x,y,z) | x <- [1 .. bound], y <- [x+1 .. bound], z <- [y+1 .. x + y - 1], abs(z ** n - x ** n - y ** n) <= epsilon]) 8 3000 100
[]
Prelude> (\n epsilon bound -> [(x,y,z) | x <- [1 .. bound], y <- [x+1 .. bound], z <- [y+1 .. x + y - 1], abs(z ** n - x ** n - y ** n) <= epsilon]) 8 3000 200
[]
Prelude> (\n epsilon bound -> [(x,y,z) | x <- [1 .. bound], y <- [x+1 .. bound], z <- [y+1 .. x + y - 1], abs(z ** n - x ** n - y ** n) <= epsilon]) 9 3000 200
[]
Prelude> (\n epsilon bound -> [(x,y,z) | x <- [1 .. bound], y <- [x+1 .. bound], z <- [y+1 .. x + y - 1], abs(z ** n - x ** n - y ** n) <= epsilon]) 10 3000 200
[]
Prelude> (\n epsilon bound -> [(x,y,z) | x <- [1 .. bound], y <- [x+1 .. bound], z <- [y+1 .. x + y - 1], abs(z ** n - x ** n - y ** n) <= epsilon]) 10 30000 200
[]
Prelude> (\n epsilon bound -> [(x,y,z) | x <- [1 .. bound], y <- [x+1 .. bound], z <- [y+1 .. x + y - 1], abs(z ** n - x ** n - y ** n) <= epsilon]) 10 300000 200
[]
Prelude> (\n epsilon bound -> [(x,y,z) | x <- [1 .. bound], y <- [x+1 .. bound], z <- [y+1 .. x + y - 1], abs(z ** n - x ** n - y ** n) <= epsilon]) 11 300000 200
[]
Prelude> (\n epsilon bound -> [(x,y,z) | x <- [1 .. bound], y <- [x+1 .. bound], z <- [y+1 .. x + y - 1], abs(z ** n - x ** n - y ** n) <= epsilon]) 11 3000000 200
[]
Prelude> (\n epsilon bound -> [(x,y,z) | x <- [1 .. bound], y <- [x+1 .. bound], z <- [y+1 .. x + y - 1], abs(z ** n - x ** n - y ** n) <= epsilon]) 11 30000000 200
[(2.0,3.0,4.0)]
Prelude> (\n x y z -> (z**n - x**n - y**n)) 11 2 3 4
4015109.0
Prelude> 

