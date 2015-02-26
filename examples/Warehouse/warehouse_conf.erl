-module(warehouse_conf).
-export([config/0]).

config() ->
  [{dealer, "Dealer", [{"StoreLoad", "Dealer"}]},
   {warehouse, "Warehouse", [{"StoreLoad", "Store"}, {"Purchase", "Seller"}]},
   {customer, "Customer", [{"Purchase", "Buyer"}]}].
