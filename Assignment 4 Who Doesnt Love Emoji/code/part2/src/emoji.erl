-module(emoji).

-export([start/1, new_shortcode/3, alias/3, delete/2, lookup/2,
         analytics/5, get_analytics/2, remove_analytics/3,
         stop/1, checkUnique/1, checkInitial/2]).

-export([search/2, deleteEmo/2]).


-type shortcode() :: string().
-type emoji() :: binary().
-type analytic_fun(State) :: fun((shortcode(), State) -> State).



start(Initial) -> 
    Bool = checkUnique (Initial),
    if  Bool =:= true -> {ok, spawn(fun () -> loop(Initial) end)};
        Bool =:= false -> {error, "The shortcode is not unique"}
    end.

new_shortcode(E, Short, Emo) -> 
    request_reply(E, {new,{Short, Emo}}).

alias(E, Short1, Short2) -> 
    request_reply(E, {alias, {Short1, Short2}}).

delete(E, Short) -> 
    nonblocking(E,{delete, Short}).

lookup(E, Short) -> 
    request_reply(E,{lookup, Short}).

analytics(_, _, _, _, _) -> not_implemented.

get_analytics(_, _) -> not_implemented.

remove_analytics(_, _, _) -> not_implemented.

stop(E) -> 
    request_reply(E,{stop}).


request_reply(Pid, Request) ->
    Pid ! {self(), Request},
    receive
        {Pid, Response} -> Response
    end.

nonblocking(Pid, Msg) ->
	Pid ! Msg.

loop(Initial) ->
    receive
        {From, {new, {S1,E1}}} ->
            Bool = checkInitial(S1,Initial),
            if  Bool=:= true ->
                    {Res, NewInitial} = {ok, [{S1,E1}|Initial]},
                    From ! {self(), Res},
                    loop(NewInitial);
                Bool =:= false ->
                    From ! {self(),{error, "The shortcode already exists"}},
                    loop(Initial)
            end;

        {From, {alias, {S1,S2}}} ->
            E1 = search(S1,Initial),
            if  E1==S1 ->
                    From ! {self(),{error, "The shortcode"++S1++"doesnt exist"}},
                    loop(Initial);
                E1/=S1 ->
                    Bool = checkInitial(S2,Initial),
                    if  Bool=:= false ->
                            From ! {self(),{error, "The shortcode"++S2++" already exist"}},
                            loop(Initial);
                        Bool=:= true ->
                            {Res, NewInitial} = {ok, [{S2,E1}|Initial]},
                            From ! {self(), Res},
                            loop(NewInitial)
                    end
            end;

        {delete, S1} ->
            E1 = search(S1,Initial),
            if  E1 == S1 ->
                	loop(Initial);
            	E1 /= S1 ->
                    NewInitial= deleteEmo(E1,Initial),
                    loop(NewInitial)
            end;

        {From, {lookup, S1}} ->
            E1 = search(S1,Initial),
            if  E1 == S1 ->
                    Res = no_emoji;
                E1 /= S1 ->
                    Res = {ok, E1}
            end,
            From ! {self(), Res},
            loop(Initial);

        {From, {stop}}-> stop

    end.

checkUnique([])-> true;
checkUnique([{S,_}|T]) ->
    L = [X || {X,_} <- T, X==S],
    %Bool = checkInitial({S,E},T),
    if  L==[] -> checkUnique(T);
        true -> false
    end.

checkInitial(_,[]) -> true;
checkInitial(S1,[{S2,_}|T]) ->
    if  S1==S2 -> false;
        S1/=S2 -> checkInitial(S1,T)
    end.

deleteShort(_,[]) ->[];
deleteShort(S1,[{S2,E2}|T]) ->
    if  S1==S2 -> T;
        S1/=S2 -> [{S2,E2}|deleteShort(S1,T)]
    end.

deleteEmo(_,[])->[];
deleteEmo(E1,[{S2,E2}|T]) ->
    if  E1==E2 -> deleteEmo(E1,T);
        E1/=E2 -> [{S2,E2}|deleteEmo(E1,T)]
    end.
search(S1,[]) -> S1;
search(S1,[{S2,E2}|T]) ->
    if  S1==S2 -> E2;
        S1/=S2 -> search(S1,T)
    end.