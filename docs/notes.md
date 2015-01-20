Notes
=====

Notes on (Unsupervised) standard session actors:
------------------------------------------------

  * Session actors are containers for multiple roles
    * Where these roles are parts which are played in protocols

  * There are different *types* of session actors. In the DNS case, those
    would be things like UDPServer, UDPHandlerSup, DNSQueryResolver,
    DNSZoneRegServer, DNSZoneDataServer.

  * Of each of these different types of session actors, there can be
    multiple instances. The actor role discovery mechanism / invitation
    system instantiates roles in protocols with actual actors, thus
    creating a Role |-> Actor Instance mapping PER CONVERSATION.

  * We can define a conversation state as:
    * A conversation ID
    * A Role |-> Actor Instance mapping
    * A Role |-> Monitor mapping

  * The conversation state is created upon conversation initiation. The
    state remains **constant** throughout the entirety of the
    conversation.


In the case of **supervised** session actors:
---------------------------------------------

  * **Actor types** are arranged in a supervision hierarchy, and may die
    / be restarted by their supervisors in some cases.

    * We can only statically specify supervision relations on actor
      types, as opposed to instances. I suppose we could think of this
      more like "A *can supervise* B".

    * And from there, have the *actual supervisions*, ie, a mapping from
      actor instances to other actor instances, recorded in some kind of
      global state.

  * If this is the case, then (at least in the first strategy I'm going
    to try) the **role monitor remains constant**, and the role is
    populated by another actor. In essence, we re-run part of the role
    discovery process, in order to find another inhabitant for the role.

  * In this strategy, it is assumed that any runtime data is copied as
    part of an error kernel storing any state that is needed, and that
    the replacement role will be able to 'drop in' at the point at which
    the old actor died.

    * Something to think about: conversation behaviour. What if the
      implementation of the actor is to send out a message as soon as it
      launches?

    * The copied state **must** dictate the point at which the actor
      re-enters the conversation.

      * As a corollary of this, the actor must save the state for each
        active role.

      * It might be nice to semi-formalise this as an FSM-like
        behaviour? In which some standardised form of state (ie, which
        comm action are we at?) is encapsulated, in order to aid
        development in this particular style. It's a nice-to-have, and
        would aid developers, but not crucial for the theory behind the
        project.

  * So basically there are two orthogonal concerns that are introduced:
    * Updating the conversation state when an actor is restarted, in
      order to ensure that all relevant conversations have up-to-date
      role |-> actor instance mappings.
    * Ensuring actors re-enter the conversation at the correct point.


  * We now look at the two concerns in turn, and look at possible
    solutions.


### Updating Conversation States when an Actor is Restarted

  * When an actor dies, 
