-module(message).
-compile(export_all).

% A representation of a message which has been sent.

message(Id, SenderRole, RecipientRoles, MessageName, PayloadTypes, Payload) ->
  {message, SenderRole, RecipientRoles, MessageName, PayloadTypes, Payload}.

message_id(Msg = {Id, _, _, _, _}) -> Id.
message_sender(Msg = {_, SenderRole, _, _, _}) -> SenderRole.
message_recipients(Msg = {_, _, RecipientRoles, _, _}) -> RecipientRoles.
message_name(Msg = {_, _, _, MessageName, _}) -> MessageName.
message_payload_types(Msg = {_, _, _, _, PayloadTypes, _}) -> PayloadTypes.
message_payload(Msg = {_, _, _, _, _, Payload}) -> Payload.
