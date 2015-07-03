
-record(outer_monitor_gen_state, {nested_fsms,             % Nested FSM ID |-> monitor_gen_state
                                  running_nested_fsm_id}). % Nested FSM ID source

-record(monitor_gen_state, {states,        % States in the FSM
                            transitions,   % Transitions in the FSM
                            running_id}).  % Running ID


-record(outer_monitor_instance, {protocol_name,
                                 role_name,
                                 monitors,
                                 monitor_instances
                                }).

-record(monitor_instance, {current_state = 0,
                           states,
                           transitions,
                           reachability_dict
                          }).
