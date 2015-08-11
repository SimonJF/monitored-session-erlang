{application, monitored_session_erlang,
  [{vsn, "0.1"},
  {modules, [bidirectional_map, conversation, conversation_instance,
             conversation_runtime_sup,
             message, monitor, monitor_gen, protocol_loader,
             protocol_registry, scribble_ast, scribble_lexer,
             scribble_parser, scribble_tokens, ssa_gen_server, util,
             gen_server2, priority_queue, actor_monitor,
             monitor_tests, conversation_instance_sup, actor_registry
            ]},
  {registered, [conv_runtime_sup, ssa_actor_type_registry, ssa_protocol_registry]}
]}.
