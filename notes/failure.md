# Failure Handling

The fact we've got abstract roles and concrete participants, coupled
with Erlang's characteristic failure handling model, gives us an
interesting space to explore regarding failure handling.

In Erlang, actors are typically arranged in hierarchies known as
supervision trees. When an error occurs in an actor, the most common
thing to do is "Let It Fail", and let the actor die. This is then
trapped by the supervisor, which knows to restart the actor, and the
system continues.

In many circumstances, this fault tolerance is largely orthogonal to
session types, which ensure the correctness of communications between
different communicating processes. Take, for example, a session
instigated by a request from a user. If the actor governing access to
the database server in this case failed, for example, then it would be
reasonable for the requestor to fail, the request to fail, and for the
client to just restart the request.

For more long-running interactions (for example, persistent sessions
among different services / web service choreographies), it's worth
having some method of repairing the interaction. The fact that we've got
a role -> actor mapping and the possibility of inviting actors to fulfil
roles in this make it slightly easier to repair conversations, but we
need to take some care with this.

## Failure Types
There are a few different types of failures:

* Monitor failures
* An Erlang node goes offline
* A process exits / is restarted

## Failure Points
Node semantics dictate that if a node goes offline, there's no way of
immediately knowing. Unfortunately, we can't rely on a notification from
`terminate`!

In practice, we'll find out about failures in the following
circumstances:

* A monitor fails when sending a message
* A monitor fails when receiving a message (note that this can be
  because of an assertion failure as well as a message being malformed)
* A monitor succeeds, but the send to the remote actor fails
* We get an explicit termination notification

## Failure Handling Strategies
Due to the different ways we find out about the failures, the failure
handling strategies for each must differ.

### Monitor Failure (Send)
We find out about send errors directly after the send has been
executed.

If the monitor fails, this means that the message didn't pass the
check and shouldn't be delivered. This can be due to an assertion
failing, or just because of a malformed / misplaced message.

In this circumstance, we can do the following:
  * Throw an exception, allowing the application to either fail and be
    restarted by a supervisor, or allowing the user to catch the
    exception.
  * Don't throw an exception, but return an error code
  * Broadcast a conversation termination request
  * Send a message to the mailbox which calls an exception handler in
    the actor
  * Notify another actor about the failure, and allow that to dictate
    what happens

### Monitor Failure (Receive)
We find out about receive errors upon receipt of an erroneous message.

Note that the handler is not executed when a bad message is received:
currently, it's as if the message is never delivered.

In this circumstance, we can:
  * Send a message to the actor to handle the erroneous message
  * Send a message to another actor about the failure, and allow that to
    dictate what happens
  * Assuming only an assertion has failed, ignore, and deliver anyway.

### Nonexistent process (Send)
In this case, the monitor will have succeeded, but the delivery attempt
will have failed. This is somewhat complicated by the fact that we can
have multiple recipients!

The way I'm currently doing it is to ping everything prior to sending,
then if everything is ok, perform the sends then.

This has a couple of drawbacks. Firstly, it's all-or-nothing: if one
endpoint is unavailable, then the entire send fails. Secondly, it's
2-staged: if a ping succeeds, it assumes a send succeeds. This second
problem could be alleviated by doing a call instead of a cast on
delivery, perhaps, but then we face the problem of only certain messages
having gone through.

Solution: Ping it, send the message, ping it again. Assume if the ping
fails on the second time that the message wasn't received. Deliver to
each actor, have them hold it in a queue until everything's delivered
successfully to all participants in the multicast, and then send another
message to detail that it's been received successfully by all
participants.

Causality constraints in multiparty sessions are very conservative, and
we have an ordered FIFO input queue as required, which is fine.

Now, at the end of this, we'll have 2 sets: a set of roles for which the
send was successful, and a set of roles for which the send was
unsuccessful. What do we do at this point?

  * Wait and retry, falling back to another strategy if this fails.
  * Nullify messages sent to other actors, notifying of failure, and
    allow sender to take remedial action, including explicit invitaitons.
  * Implicitly invite other actors to fulfil the roles. This will
    require migration of the monitors -- or centralised monitoring.
      * There will also likely be some effort needed to disambiguate,
        allowing the actor to take off at the right place. We'll likely
        help by allowing some state to be passed. Without being
        overly restrictive on the protocol, some of the onus must be on
        the developer to ensure that this happens. Of course, fidelity
        will be preserved by the monitor in the case that it *does* come
        back in the wrong place.
  * Kill the conversation.
  * Delegate to another actor which can decide what to do at this point.
