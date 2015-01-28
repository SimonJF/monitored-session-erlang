-module(message).
-compile(export_all).

% A representation of a message which has been sent.

message(Id, SenderRole, RecipientRoles, MessageName, PayloadTypes, Payload) ->
  {message, Id, SenderRole, RecipientRoles, MessageName, PayloadTypes, Payload}.

message_id(_Msg = {message, Id, _, _, _, _, _}) -> Id.
message_sender(_Msg = {message, _, SenderRole, _, _, _, _}) -> SenderRole.
message_recipients(_Msg = {message, _, _, RecipientRoles, _, _, _}) -> RecipientRoles.
%  called as message_recipients({message,"Role1",["Role2"],"Request",[],[]})


message_name(_Msg = {message, _, _, _, MessageName, _, _}) -> MessageName.
message_payload_types(_Msg = {message, _, _, _, _, PayloadTypes, _}) -> PayloadTypes.
message_payload(_Msg = {message, _, _, _, _, _, Payload}) -> Payload.

