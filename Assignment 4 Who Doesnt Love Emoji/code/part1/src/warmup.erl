-module(warmup).
-export([]).

% direction is one of the atoms north, south, east or west

% move(north, {X, Y}) -> {X, Y+1};
% move(west,  {X, Y}) -> {X-1, Y}.
% complete the definition
move(north, {X, Y}) -> {X, Y+1};
move(south, {_, 0}) -> throw("ups cant go south");
move(south, {X, Y}) -> {X, Y-1};
move(east, {X, Y}) -> {X+1, Y};
move(west, {0, _}) -> throw("ups cant go west");
move(west, {X, Y}) -> {X-1, Y};
move(north_east, {X, Y}) -> {X+1, Y+1};
move(north_west, {0, _}) -> throw("ups cant go west");
move(north_west, {X, Y}) -> {X-1, Y+1};
move(south_east, {_, 0}) -> throw("ups cant go south");
move(south_east, {X, Y}) -> {X+1, Y-1};
move(south_west, {_, 0}) -> throw("ups cant go south");
move(south_west, {0, _}) -> throw("ups cant go west");
move(south_west, {X, Y}) -> {X-1, Y-1}.

% A binary search tree is either
%      the atom leaf
%   or a tuple {node, Key, Value, Left, Right}
%      where Left and Right are binary search trees, and all the keys
%      in Left are smaller than Key and all the keys in Right are
%      larger than Key


% insert inserts a key and a value into a binary search tree. If the
% key is already there the value is updated.

%insert(Key, Value, Tree) -> undefined.
% complete the definition.
insert({K, V}, leaf) -> {node, K, V, leaf, leaf};
insert({K, V}, {node, NodeK, NodeV, Left, Right}) ->
  if K == NodeK -> {node, K, V, Left, Right}  
   ; K <  NodeK -> {node, NodeK, NodeV, insert({K, V}, Left), Right}
   ; K >  NodeK ->{node, NodeK, NodeV, Left, insert({K, V}, Right)}
  end.

% lookup find the value associated to a key in a binary search
% tree. Returns {ok, Value} if the key is in the tree; or none if the
% key is not in the tree.

% lookup(Key, Tree) -> undefined.
% complete the definition.

lookup(_, leaf) -> none;
lookup(K, {node, NodeK, NodeV, Left, Right}) -> 
  if K == NodeK -> {ok, NodeV}  % replace or keep original??
   ; K <  NodeK -> lookup(K, Left)
   ; K >  NodeK -> lookup(K, Right)
  end.

% TESTS
%% move/2 %%
% {0,1}, warmup:move(north, {0,0}).
% error south, warmup:move(south, {0,0}).
% {1,0}, warmup:move(east, {0,0}).
% error west, warmup:move(west, {0,0}).
% {1,1}, warmup:move(north_east, {0,0}).
% {0,1}, warmup:move(north_west, {1,0}).
% error west, warmup:move(north_west, {0,0}).
% {1,0}, warmup:move(south_east, {0,1}).
% error south, warmup:move(south_east, {0,0}).
% {0,0}, warmup:move(south_east, {1,1}).
% error west, warmup:move(south_west, {0,1}).
% error south, warmup:move(south_west, {0,0}).

%% insert/2 %%
% {node,0,"Begona",leaf,leaf}, warmup:insert({0,"Begona"},leaf). 
% {node,1,"Begona",leaf,leaf}, warmup:insert({1,"Begona"},{node,1,"Marko",leaf, leaf}).
% warmup:insert({0,"Begona"},{node,1,"Marko",leaf,{node,2,"Robert",leaf,leaf}}).  
%{node,{1,"Marko"},
%      {node,0,"Begona",leaf,leaf},
%      {node,2,"Robert",leaf,leaf}}

%% lookup/2 %%
% none, warmup:lookup(4,leaf).
% none, warmup:lookup(1,{node,{4,"Super"}, leaf, leaf}). 
% {ok,"Super"}, warmup:lookup(4,{node,4,"Super", leaf, leaf}). 
% {ok,"Super2"}, warmup:lookup(5,{node,4,"Super", leaf, {node,5,"Super2", leaf, leaf}}).
% {ok,"Bad 2"}, warmup:lookup(-1,{node,4,"Super", {node,0,"Bad", {node,-1,"Bad 2", leaf, leaf}, leaf}, {node,5,"Super2", leaf, leaf}}).  