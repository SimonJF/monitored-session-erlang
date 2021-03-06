-module(message).
-compile(export_all).

-record(message_record, {
                  message_id,
                  message_sender_role,
                  message_recipient_roles,
                  message_name,
                  message_payload_types,
                  message_payload}).

% A representation of a message which has been sent.

message(ID, SenderRole, RecipientRoles, MessageName, PayloadTypes, Payload) ->
  add_message_id(message(SenderRole, RecipientRoles, MessageName,
                         PayloadTypes, Payload), ID).
message(SenderRole, RecipientRoles, MessageName, PayloadTypes, Payload) ->
  #message_record{
           message_id=undefined,
           message_sender_role=SenderRole,
           message_recipient_roles=RecipientRoles,
           message_name=MessageName,
           message_payload_types=PayloadTypes,
           message_payload=Payload}.

add_message_id(Msg, MsgID) ->
  Msg#message_record{message_id=MsgID}.

message_id(Msg) -> Msg#message_record.message_id.
message_sender(Msg) -> Msg#message_record.message_sender_role.
message_recipients(Msg) -> Msg#message_record.message_recipient_roles.
message_name(Msg) -> Msg#message_record.message_name.
message_payload_types(Msg) -> Msg#message_record.message_payload_types.
message_payload(Msg) -> Msg#message_record.message_payload.

