-module(tbp_logger).
-export([info/3, warn/3, err/3]).


log_msg(ActorName, Func, Format, Args) ->
  Func("Actor ~p: " ++ Format, [ActorName] ++ Args).

info(ActorName, Format, Args) ->
  log_msg(ActorName, fun error_logger:info_message/2, Format, Args).

warn(ActorName, Format, Args) ->
  log_msg(ActorName, fun error_logger:warn_message/2, Format, Args).

err(ActorName, Format, Args) ->
  log_msg(ActorName, fun error_logger:error_message/2, Format, Args).

