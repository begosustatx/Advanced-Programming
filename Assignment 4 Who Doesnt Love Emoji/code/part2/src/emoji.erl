-module(emoji).

-export([start/1, new_shortcode/3, alias/3, delete/2, lookup/2,
         analytics/5, get_analytics/2, remove_analytics/3,
         stop/1, checkUnique/1, checkInitial/2]).

-type shortcode() :: string().
-type emoji() :: binary().
-type analytic_fun(State) :: fun((shortcode(), State) -> State).

-export([search/2]).

convertToNewInitial([], NewInitial) -> NewInitial;  
convertToNewInitial([{S,E}|T], H) ->
    convertToNewInitial(T, [{S,E,[]}|H]).

start(Initial) -> 
    Bool = checkUnique (Initial),
    if  Bool =:= true -> NewInitial = convertToNewInitial(Initial, []),
                         {ok, spawn(fun () -> loop(NewInitial) end)};
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

analytics(E, Short, Fun, Label, Init) -> 
    request_reply(E,{analytics, Short, Fun, Label, Init}).

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

checkUnique([])-> true;
checkUnique([{S,_}|T]) ->
    L = [X || {X,_} <- T, X==S],
    if  L==[] -> checkUnique(T);
        true -> false
    end.

checkInitial(_,[]) -> true;
checkInitial(S1,[{S2,_,_}|T]) ->
    if  S1==S2 -> false;
        S1/=S2 -> checkInitial(S1,T)
    end.

deleteEmo(_,[])->[];
deleteEmo(E1,[{S2,E2,A}|T]) ->
    if  E1==E2 -> deleteEmo(E1,T);
        E1/=E2 -> [{S2,E2,A}|deleteEmo(E1,T)]
    end.

search(_,[]) -> not_found;
search(S1,[{S2,E2,A}|T]) ->
    if  S1==S2 -> {E2, A};
        S1/=S2 -> search(S1,T)
    end.

addAnalytics(_,_,[])->[];
addAnalytics(Emo, A, [{S1,E1,A1}|T])->
	if 	Emo == E1 ->[{S1,E1,[A1|A]}|addAnalytics(Emo,A,T)];
		Emo /= E1 -> [{S1,E1,A}|addAnalytics(Emo,A,T)]
	end.	


loop(Initial) ->
    receive
        {From, {new, {S1,E1}}} ->
            Bool = checkInitial(S1,Initial),
            if  Bool=:= true ->
                    {Res, NewInitial} = {ok, [{S1,E1,[]}|Initial]},
                    From ! {self(), Res},
                    loop(NewInitial);
                Bool =:= false ->
                    From ! {self(),{error, "The shortcode already exists"}},
                    loop(Initial)
            end;

        {From, {alias, {S1,S2}}} ->
            Found = search(S1,Initial),
            if  Found==not_found ->
                    From ! {self(),{error, "The shortcode"++S1++"doesnt exist"}},
                    loop(Initial);
                Found/=not_found ->
                    Bool = checkInitial(S2,Initial),
                    if  Bool=:= false ->
                            From ! {self(),{error, "The shortcode"++S2++" already exist"}},
                            loop(Initial);
                        Bool=:= true ->
                            {E1, A} = Found,
                            {Res, NewInitial} = {ok, [{S2,E1,A}|Initial]},
                            From ! {self(), Res},
                            loop(NewInitial)
                    end
            end;

        {delete, S1} ->
            Found = search(S1,Initial),
            if  Found == not_found ->
                	loop(Initial);
            	Found /= not_found ->
                    {E1, _} = Found,
                    NewInitial= deleteEmo(E1,Initial),
                    loop(NewInitial)
            end;

        {From, {lookup, S1}} ->
            Found = search(S1,Initial),
            if  Found == not_found ->
                    Res = no_emoji;
                Found /= not_found ->
                    {E1, _} = Found, 
                    Res = {ok, E1}
            end,
            From ! {self(), Res},
            loop(Initial);

        {From, {stop}}-> From ! {self(), ok};

        {From,{analytics, Short, Fun, Label, Init}} ->
        	Found = search(Short,Initial),
            if  Found==not_found ->
                    From ! {self(),{error, "The shortcode"++Short++"doesnt exist"}},
                    loop(Initial);
                Found/=not_found ->
					{E1,A1} = Found,
					LabelFound = search(Label, A1),
					if 	LabelFound ==not_found -> 
							NewInitial = addAnalytics(E1, {Label,Fun,Init},Initial), 
							From ! {self(), ok},
							loop(NewInitial);
						LabelFound /= not_found -> 
							From ! {self(),{error, "The Label"++Label++"is already registered"}},
                    		loop(Initial)
					end
            end

    end.