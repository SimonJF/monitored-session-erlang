Monitored Session Erlang
=========================

Erlang, but with dynamic monitoring of session types.
Check the [mse-examples](http://github.com/SimonJF/mse-examples)
repository for more.

Session types specify the structure of communication between parties.
This can be enforced statically in the type system, requiring a
substructural linear type system, or monitored dynamically at runtime.

This project aims to implement the work on dynamic monitoring of session
types (see Monitoring Networks through Multiparty Session Types, Bocchi
et al, 2013; Multiparty Session Types Meet Communicating Automata,
Denielou and Yoshida, 2012) with Erlang, largely based on the work on
Multiparty Session Actors (Neykova and Yoshida, 2014). 

Primary focuses of the project are on examination of the integration of
OTP concepts such as supervision, formalisation of the system, and
investigating patterns that can't currently be specified.

Essentially, the system is currently a wrapper around gen\_server, with
monitoring built in. 

Session types are written in the Scribble protocol description language.

More documentation to come when everything's finalised.

