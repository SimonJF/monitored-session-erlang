Monitored Session Erlang
=========================

** Disclaimer: This is very much a research project in progress.
Everything can and will change! This contains many unexplored and
unformalised ideas. The code is a mess in places, but will eventually be
cleaned up :) **

Session types specify the structure of communication between parties.
This can be enforced statically in the type system, requiring a
substructural linear type system, or monitored dynamically at runtime.

This project aims to implement the work on dynamic monitoring of session
types (see Monitoring Networks through Multiparty Session Types, Bocchi
et al, 2013; Multiparty Session Types Meet Communicating Automata,
Denielou and Yoshida, 2012) with Erlang, largely based on the work on
Multiparty Session Actors (Neykova and Yoshida, 2014). Primary focuses
of the project are on examination of the integration of OTP concepts
such as supervision, formalisation of the system, and investigating
patterns that can't currently be specified.

Essentially, the system is currently a wrapper around gen\_server, with
monitoring built in. Check the examples folder for some examples -- full
documentation to come when everything is finalised.

Session types are written in the Scribble protocol description language,
or at least a dialect thereof -- there are a couple of features in my
implementation that aren't in Scribble, and conversely I haven't
implemented all of the features of Scribble (interruptible and parallel
scopes).

A primary objective is to see how dynamic monitoring of session types
interacts with the concept of supervision trees. My guess is that it's
largely orthogonal, combined with the Let It Fail mentality of Erlang /
OTP. Work on showing this is ongoing.

