(Note to anyone unfortunate enough to be reading this: this is an
unstrucutred brain-dump of thoughts I've had while thinking about how
session types interact with Erlang's supervision method. Here be
dragons. Don't judge me.)

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


## When is it safe to re-populate a role?
This is an important question. Let us consider, abstractly, the state of
an actor prior to handling a message:

  * A queue of (checked) messages to be processed
  * A monitor consistent with the queue of checked messages
  * User actor state
  * (Proposed) per-conversation-role state

We can treat user actor state as orthogonal. If the user wants to
share state between actors, then they should replace it in the standard
Erlang ways (think reloading from error kernels, etc). Conversation-role
state needs to be preserved, however.

Now, we must consider the points at which a process may die.
  * **Case 1** When not handling a message, for example due to a node
    going offline, or a one_for_all restart by a supervisor
  * **Case 2** During the handling of a message, prior to any messages
    being sent
  * **Case 3** During the handling of a message, after sending other
    messages

Now, how do we handle each case?

### Case 1 (Not during handler)
This is the simplest case. If something is idle, it is not in the middle
of processing any messages (and, most likely, won't have any in its
queue either).

In this case, we can simply "switch out" the actor -- we invite
something else to fulfil the role, and resend the messages in the queue
(but 99.999% of the time there won't be any).

### Case 2 (During handler, nothing sent yet)
The second case isn't too much worse. In this case, we will need to
migrate the monitor, and re-send the messsage which was previously sent
in order to re-trigger the handler, as well as any queued messages.

### Case 3 (During handler, some messages sent)
This is a lot more difficult, because it essentially means that control
must resume from the point at which the last message was sent.

I'm in two minds about this. One the one hand, we could just say that
this is unrecoverable, and abort the conversation.

On the other, we'd have to consider how to proceed. Of course, the
monitor state will be at the correct point, guaranteeing session
fidelity, but we need to ensure we'd come back in (at a sub-handler
level) at the correct place. This is Hard To Do.

For now, I think I'm going to say that we'll count this as
un-recoverable, at least until we have the basic infrastructure set up.

## Implementation

So, I'm going to treat this bit as a to-do list-y thing.

We need some way of keeping track of the state of a role in a session.

State ::= Idle | HandlerStarted | MessageSent

Idle: Either no handlers have run, or a handler has successfully run and
saved all state, etc.

HandlerStarted: The handler is "in action" (tell me if for some reason
you're reading this and you get the joke, and I'll buy you a pint), but
hasn't yet sent a message.

MessageSent: The handler is "in action", but a message has been sent,
meaning that the monitor has advanced.

We need to keep track of *all messages which have been processed by the
monitor but not yet processed by the actor*. We queue messages when they
have been processed by the monitor, and dequeue them when they have been
completely processed by the handler. I want to say to keep this on the
role\_monitor, but it might be necessary to keep it on a separate
process bidirectionally linked to the monitor. We'll see.

Todo tomorrow:
  * Extend role\_monitors with role state
  * Modify callbacks in ssa\_gen\_server to save role state
  * Recording and detection of state within the handler (ie Idle / HS /
    MS)
  * (Possibly, if time?) work on first pass of implementing this.
