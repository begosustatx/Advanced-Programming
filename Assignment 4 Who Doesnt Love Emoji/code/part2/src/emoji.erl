-module(emoji).

-export([start/1, new_shortcode/3, alias/3, delete/2, lookup/2,
         analytics/5, get_analytics/2, remove_analytics/3,
         stop/1]).

-export([checkUnique/1, deleteAliases/2]).

-type shortcode() :: string().
-type emoji() :: binary().
-type analytic_fun(State) :: fun((shortcode(), State) -> State).

checkUnique([]) -> ok;
checkUnique([{S, _}|T]) ->
    L = [X || {X,_} <- T, X==S],
    if L==[] -> checkUnique(T);
       true  -> throw(S)
    end.

start(Initial) ->
    try
        checkUnique(Initial),
        {ok, spawn(fun () -> loop(Initial) end)}
    catch
        S -> {error, "The shortcode " ++ S ++ " is not unique."}
    end.

new_shortcode(E, Short, Emo) -> 
    request_reply(E, {new, {Short, Emo}}).

alias(E, Short1, Short2) -> 
    request_reply(E, {alias, {Short1, Short2}}).

deleteAliases([], NewMap) -> NewMap;
deleteAliases([Short|T], InitialMap) ->
    NewMap = maps:remove(Short, InitialMap),
    deleteAliases(T, NewMap).

delete(E, Short) ->
    request_reply(E, {delete, {Short}}).

lookup(E, Short) -> 
    request_reply(E, {lookup, Short}).

analytics(_, _, _, _, _) -> not_implemented.

get_analytics(_, _) -> not_implemented.

remove_analytics(_, _, _) -> not_implemented.

%% TODO Check if it is good to implement it like this
stop(E) ->
    request_reply(E, stop).

%%% Internal implementation

request_reply(Pid, Request) ->
    Pid ! {self(), Request},
    receive
        {Pid, Response} -> Response
    end.

loop(Initial) ->
    receive
        {From, {stop}}-> ok;

        {From, {new, {Short, Emo}}} ->
            InitialMap = maps:from_list(Initial),
            IsKey = maps:is_key(Short, InitialMap),
            if 
               IsKey =:=  true -> From ! {self(), {error,  "The shortcode " ++ Short ++ " is already registered."}},
                                  loop(Initial);
               IsKey =:= false -> ShortCode = {Short, Emo},
                                  From ! {self(), ok},
                                  loop([ShortCode | Initial])
            end;

        {From, {alias, {Short1, Short2}}} ->
            %% TODO add logic to find short1 if exists add another short2
            From ! {self(), ok},
            loop(Initial);
        
        {From, {delete, {Short}}} ->
            %% TODO FIX it, doesnt work loop???
            try
                io:fwrite("loop"),
                InitialMap = maps:from_list(Initial),
                io:fwrite("loop 1"),
                Emo = maps:get(Short, InitialMap),
                io:fwrite("loop 2"),
                Shortcodes = [A || {A,E} <- Initial, E==Emo],
                io:fwrite("loop 3"),
                NewMap = deleteAliases(Shortcodes, InitialMap),
                io:fwrite("loop 4"),
                % From ! {self(), ok}, always succeed (return value is unspecified)
                loop(maps:to_list(NewMap))
            catch
                _ -> io:fwrite("loop 5"),loop(Initial)
            end;

        {From, {lookup, Short}} ->
            %% TODO add logic to find short and return emoji, from your datastructure initial ??
            InitialMap = maps:from_list(Initial),
            IsKey = maps:is_key(Short, InitialMap),
            if 
               IsKey =:=  true -> Emo = maps:get(Short, InitialMap),
                                  From ! {self(), {ok, Emo}},
                                  loop(Initial);
               IsKey =:= false -> From ! {self(), no_emoji},
                                  loop(Initial)
            end;

        {From, Other} ->
            From ! {self(), {error,unknow_request, Other}},
            loop(Initial)
    end.
