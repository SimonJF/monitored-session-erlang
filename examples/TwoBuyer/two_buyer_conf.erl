-module(two_buyer_conf).
-export([config/0]).
% Config file
%
% Contains the protocols / roles in those protocols that each actor type
% is registed for.

% List of the following form:
% {module_atom_name, ActorTypeName, [{Protocol, Role}]}
% Where module_atom_name is the atom name of the module
% ActorTypeName is the string human-readable name of the actor type
% And the {Protocol, Role} list is a list of Protocol |-> Role mappings.

config() ->
  [{buyer1, [{"TwoBuyers", ["A"]}]},
   {buyer2, [{"TwoBuyers", ["B"]}]},
   {seller, [{"TwoBuyers", ["S"]}]}].
