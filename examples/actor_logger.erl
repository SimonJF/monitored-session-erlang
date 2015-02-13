-module(actor_logger).
-export([info/3, warn/3, err/3]).


log_msg(ActorName, Func, Format, Args) ->
  Func("Actor ~p: " ++ Format ++ "~n", [ActorName] ++ Args).

info(ActorName, Format, Args) ->
  log_msg(ActorName, fun error_logger:info_msg/2, Format, Args).

warn(ActorName, Format, Args) ->
  log_msg(ActorName, fun error_logger:warning_msg/2, Format, Args).

err(ActorName, Format, Args) ->
  log_msg(ActorName, fun error_logger:error_msg/2, Format, Args).

