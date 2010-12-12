## General

This is hamush - I figured that learning earling is a big task so a not
trivial program would be nice as a project. Hamush was born, a mush server
that is supposed to offer high stability and fault tollerance - yay for
Erlang/OTP ;) Other then that I just want to toy around. To be frank I've
little hope anyone will ever use this really then again if someone does let me
know.

## Features

* Lisp like programming.
* @repl for playing around.
* Functions can be defined in erlang or directly entered in the server.
* Objects and attributes are persistant over restart.
* Execution of code is tied to the objects and happens in paralell.
* Objects can 'crash' without harming the Server, they are just restarted.
* Commands are defined in Erlang and can be loaded during runtime without
reboot.
* Minimal build in commands as of yet (more to come).

## Todo

* Improve LISP interpreter.
* Add channel communication.
* Add many important commands.
* Add capability of in game command coding.
* Tons of other things.