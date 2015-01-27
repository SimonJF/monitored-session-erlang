-module(session_actor).

-export([behaviour_info/1]).

% This is the behaviour for basic session actors.
% The behaviour requires two callbacks:
%  * ssactor_init, which returns the initial user state
%  * ssactor_handle_msg, which handles incoming messages.
%  * ssactor_first_msg, which is called when we're the first participant in
%    the conversation, meaning we've got to send a message to get the ball rolling.
%
% The session actor itself has certain bits of internal state, which we
% leverage in order to perform monitoring and message routing. This is:
%  * Conversation process PID: We use this in order to relay messages to
%    the conversation process, which performs Role |-> Endpoint routing.
%
%  * Roles the actor is playing in the current conversation
%
%  * The currently-active role in the actor
%
%  * And finally, a Role |-> Monitor mapping.


% Workflow:
%  * Listen for invitation messages.
%    * Actors can be invited for multiple roles. This means we've got
%      to treat this a bit like an FSM:
%        Idle -> Setup -> Working -> Idle, where...
%          - Idle is when the actor is not active in any conversation;
%          - Setup is entered when the first invitation message is received,
%            and waits for either further invitation messages *from the same CID*
%            or for a system message indicating that the conversation setup is
%            complete -- this will be sent by the protocol actor when all roles
%            have been fulfilled. This transitions us into the working state...
%          - Working is the state in which the Actual Communication occurs...
%          - And finally, once the conversation is over, we clear the
%            conversation-specific state and go back to the idle state.
%            How do we know it's over? When both monitors are in final states?
%            Do we wait on a message from the protocol process?
%
%    Idle State
%    ==========
%    When an invitation message is received and we're in the idle state, then
%    we need to do the Conversation Setup. This is...
%
%      * Create conversation state with conversation ID contained in invite msg
%      * Do role setup
%
%      Role Setup
%      ----------
%        * Add the role to which we've been invited into the CurrentRoles list
%        * Retrieve the monitor for the role from the protocol / actor type process
%        * Add the new monitor into the Role |-> Monitor mapping
%
%    Setup State
%    ===========
%    In this state, we await more role invitation messages.
%      * If we get a role invitation message for a conversation we're already in,
%        for a role we've not fulfilled, then we'll need to do the role setup for
%        that particular role.
%      * If we've fulfilled that role already, {error, role_already_fulfilled}
%         - This shouldn't really happen; it's more an internal error
%      * If it's for a different conversation, {error, already_occupied}
%         - If this happens, then that's fine -- it just means that the protocol
%           actor should delegate this invitation to a different actor instance instead.
%
%    Once the protocol instance has populated all roles with actor instances, then
%    we'll be needing to transition into the Working state.
%
%    Working State
%    =============
%    The working state is the main body of the conversation logic. This means that
%    we'll be carrying out the main communication here.
%    If we're the conversation initiator, the protocol process will send us a start_protocol
%    system message, which will set the active role and call the first_message callback.
%
%    We first check to see whether the message that has been received is a system message.
%    If so, then we'll need to handle that and go back to the start.
%
%    If not then we'll check the incoming message against the monitor.
%


behaviour_info(callbacks) ->
    [{ssactor_init,1},
     {ssactor_handle_msg, 1}];
behaviour_info(_Other) ->
    undefined.
