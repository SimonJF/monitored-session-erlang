
-record(outer_monitor_gen_state, {nested_fsms,             % Nested FSM ID |-> monitor_gen_state
                                  running_nested_fsm_id}). % Nested FSM ID source

-record(monitor_gen_state, {states,        % States in the FSM
                            transitions,   % Transitions in the FSM
                            running_id}).  % Running ID


-record(outer_monitor_instance, {protocol_name,
                                 role_name,
                                 monitors,
                                 monitor_instances,
                                 reachability_dicts % FSM ID |-> (State ID |-> Involved roles, Involved FSMs)
                                }).

-record(monitor_instance, {fsm_id,
                           current_state = 0,
                           states,
                           transitions
                          }).
