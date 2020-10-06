-module(emoji).

-export([start/1, new_shortcode/3, alias/3, delete/2, lookup/2,
         analytics/5, get_analytics/2, remove_analytics/3,
         stop/1]).

-compile(export_all).

-type shortcode() :: string().
-type emoji() :: binary().
-type analytic_fun(State) :: fun((shortcode(), State) -> State).

convertToNewInitial([], NewInitial) -> NewInitial;  
convertToNewInitial([{S,E}|T], H) ->
    convertToNewInitial(T, [{S,E,[],[]}|H]).

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

deleteShort(_,[])->[];
deleteShort(S1,[{S2,E2,Al,A}|T]) ->
    if  S1==S2 -> deleteShort(S1,T);
        S1/=S2 -> [{S2,E2,Al,A}|deleteShort(S1,T)]
    end.

search(_,[]) -> not_found;
search(S1,[{S2,E2,Al,A}|T]) ->
    if  S1==S2 -> {E2,Al,A};
        S1/=S2 -> search(S1,T)
    end.

searchInAliases(_,[]) -> not_found;
searchInAliases(S1,[A|T]) ->
    if  S1==A -> found;
        S1/=A -> search(S1,T)
    end.

addAlias(_,_,[])->[];
addAlias(S, A, [{S1,E1,Al,An}|T])->
	if 	S == S1 ->[{S1,E1,[A|Al], An}|T];
		S /= S1 -> [{S1,E1,Al,An}|addAlias(S,A,T)]
	end.	

getShortcodeForAlias(_, []) -> no_shortcode;
getShortcodeForAlias(A, [{S1,E1,Al,An}|T]) ->
    Member = lists:member(A, Al),
    if Member =:= true -> {S1,E1,Al,An};
       Member =:= false -> getShortcodeForAlias(A, T)
    end.

checkRegistered(_,[]) -> not_registered;
checkRegistered(S,[{S1,E1,Al,An}|T]) ->
    if S =:= S1 -> registered;
       true -> Member = lists:member(S, Al),
               if Member =:= true -> registered;
                  Member =:= false -> checkRegistered(S, T)
               end
    end.

loop(Initial) ->

%    io:fwrite("Initial : ~w", [Initial])
    receive
        {From, {new, {S1,E1}}} ->
            Found = search(S1,Initial),
            if  Found /= not_found ->
                    From ! {self(),{error, "The shortcode 1 already exists"}},
                    loop(Initial);
                Found == not_found ->
                    ShortCode = getShortcodeForAlias(S1,Initial),
                    if ShortCode /= no_shortcode ->
                            From ! {self(),{error, "The shortcode already exists"}},
                            loop(Initial);
                       ShortCode == no_shortcode ->
                            {Res, NewInitial} = {ok, [{S1,E1,[],[]}|Initial]},
                            From ! {self(), Res},
                            loop(NewInitial)
                    end
            end;
            
    {From, {alias, {S1,S2}}} ->
            Found = search(S1,Initial),
            if  Found==not_found ->
                    ShortCode = getShortcodeForAlias(S1,Initial),
                    if ShortCode == no_shortcode ->
                            From ! {self(),{error, "The shortcode does not exists"}},
                            loop(Initial);
                       ShortCode /= no_shortcode ->
                            {Short, _, Al, _} = ShortCode,
                            FoundAlias = lists:member(S2,Al),
                            if  FoundAlias =:= false ->
                                    NewInitial = addAlias(Short, S2, Initial),
                                    From ! {self(), ok},
                                    loop(NewInitial);
                                FoundAlias =:= true ->
                                    From ! {self(),{error, "The alias already exists"}},
                                    loop(Initial)
                            end
                    end;
                Found/=not_found ->
                    Registered = checkRegistered(S2,Initial),
                    if Registered == registered ->
                            From ! {self(),{error, "The shortcode 3 already exist"}},
                            loop(Initial);
                       Registered == not_registered ->     
                            {E1, Al, A} = Found,
                            FoundAlias = search(S2,Al),
                            if  FoundAlias /= not_found ->
                                    From ! {self(),{error, "The shortcode 3 already exist"}},
                                    loop(Initial);
                                FoundAlias == not_found ->
                                    {E1, Al, A} = Found,
                                    NewInitial = addAlias(S1, S2, Initial),
                                    From ! {self(), ok},
                                    loop(NewInitial)
                            end
                    end
            end;

        {delete, S1} ->
            Found = search(S1,Initial),
            if  Found == not_found ->
                    ShortCode = getShortcodeForAlias(S1, Initial),
                    if ShortCode == no_shortcode ->
                                        loop(Initial);
                       ShortCode /= no_shortcode ->
                            {S,_,_,_} = ShortCode,
                            NewInitial = deleteShort(S,Initial),
                            loop(NewInitial)
                    end;
            	Found /= not_found ->
                    {E1, _, _} = Found,
                    NewInitial = deleteShort(S1,Initial),
                    loop(NewInitial)
            end;

        {From, {lookup, S1}} ->                       
            Found = search(S1,Initial),
            if  Found == not_found ->
                    ShortCode = getShortcodeForAlias(S1, Initial),
                    if ShortCode == no_shortcode ->
                                Res = no_emoji;
                       ShortCode /= no_shortcode ->
                                {_, E1, _, _} = ShortCode, 
                                Res = {ok, E1}
                    end;
                Found /= not_found ->
                    {E1, _, _} = Found, 
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
							%NewInitial = addAnalytics(E1, {Label,Fun,Init},Initial), 
							From ! {self(), ok},
							loop(Initial);
						LabelFound /= not_found -> 
							From ! {self(),{error, "The Label"++Label++"is already registered"}},
                    		loop(Initial)
					end
            end
    end.