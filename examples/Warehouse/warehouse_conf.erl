-module(warehouse_conf).
-export([config/0]).

config() ->
  [{dealer, [{"StoreLoad", ["Dealer"]}]},
   {warehouse, [{"StoreLoad", ["Store"]}, {"Purchase", ["Seller"]}]},
   {customer, [{"Purchase", ["Buyer"]}]}].
