Monitored Session Erlang
=========================

** Disclaimer: This is very much a research project in progress.
Everything can and will change! This contains many unexplored and
unformalised ideas. The code is a mess in places, but will eventually be
cleaned up :) **

This project aims to implement the work on dynamic monitoring of session
types (see Monitoring Networks through Multiparty Session Types, Bocchi
et al, 2013; Multiparty Session Types Meet Communicating Automata,
Denielou and Yoshida, 2012) with Erlang, largely based on the work on
Multiparty Session Actors (Neykova and Yoshida, 2014).

Essentially, the system is currently a wrapper around gen\_server, with
monitoring built in. Check the examples folder for some examples -- full
documentation to come when everything is finalised.

Session types are written in the Scribble protocol description language,
or at least a dialect thereof -- there are a couple of features in my
implementation that aren't in Scribble, and conversely I haven't
implemented all of the features of Scribble (interruptible and parallel
scopes).

One new feature I've implemented is *transient roles* -- roles which are
not auto-populated upon session initiation and are only valid within an
*invitation scope*. This captures some patterns that weren't expressible
beforehand.

A primary objective is to see how dynamic monitoring of session types
interacts with the concept of supervision trees. My guess is that it's
largely orthogonal, combined with the Let It Fail mentality of Erlang /
OTP. Work on showing this is ongoing.

