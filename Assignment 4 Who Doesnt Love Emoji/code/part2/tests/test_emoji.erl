-module(test_emoji).
-export([test_all/0]).

% Feel free to use eunit
-include_lib("eunit/include/eunit.hrl").

test_all() -> eunit:test(testsuite(), [verbose]).

testsuite() ->
    [ {"Basic behaviour", spawn,
        [ test_start_server(), 
          test_new_shortcode(),
          %test_delete(),
          %test_alias(),
          test_lookup() ] } ].

test_start_server() ->
    [{"We can call start/1 with empty Initial and it returns {ok,E}",
    fun () ->
      Initial = [],
      ?assertMatch({ok, _}, emoji:start(Initial))
    end },
    {"We can call start/1 with unique one value Initial and it returns {ok,E}",
    fun () ->
      Initial = [{"smiley", <<240, 159, 152, 131>>}],
      ?assertMatch({ok, _}, emoji:start(Initial))
    end },
    {"We can call start/1 with unique two values Initial and it returns {ok,E}",
    fun () ->
      Initial = [{"smiley", <<240, 159, 152, 131>>}, {"happy", <<222, 222, 222, 222>>}],
      ?assertMatch({ok, _}, emoji:start(Initial))
    end },
    {"We can call start/1 with unique two alias values Initial and it returns {ok,E}",
    fun () ->
      Initial = [{"smiley", <<240, 159, 152, 131>>}, {"super_smiley", <<240, 159, 152, 131>>}],
      ?assertMatch({ok, _}, emoji:start(Initial))
    end },
    {"We can call start/1 with not unique two values Initial and it returns {ok,E}",
    fun () ->
      Initial = [{"smiley", <<240, 159, 152, 131>>}, {"smiley", <<240, 159, 152, 131>>}],
      ?assertMatch({error, _}, emoji:start(Initial))
    end }].

test_new_shortcode() ->
    [{"Register new shortcode to empty Initial",
    fun () ->
        Initial = [],
        {ok, S} = emoji:start(Initial),
        ?assertEqual(ok, emoji:new_shortcode(S, "smiley", <<240,159,152,131>>))
    end },
    {"Register new shortcode to one unique value Initial",
    fun () ->
        Initial = [{"smiley", <<240, 159, 152, 131>>}],
        {ok, S} = emoji:start(Initial),
        ?assertEqual(ok, emoji:new_shortcode(S, "happy", <<222, 222, 222, 222>>))
    end },
    {"Register new shortcode to two unique value Initial",
    fun () ->
        Initial = [{"smiley", <<240, 159, 152, 131>>}, {"happy", <<222, 222, 222, 222>>}],
        {ok, S} = emoji:start(Initial),
        ?assertEqual(ok, emoji:new_shortcode(S, "funny", <<77, 77, 77, 77>>))
    end },
    {"Register same shortcode to one unique value Initial",
    fun () ->
        Initial = [{"smiley", <<240, 159, 152, 131>>}, {"happy", <<222, 222, 222, 222>>}],
        {ok, S} = emoji:start(Initial),
        ?assertMatch({error, _}, emoji:new_shortcode(S, "smiley", <<77, 77, 77, 77>>))
    end },
    {"Register same first shortcode to two unique value Initial",
    fun () ->
        Initial = [{"smiley", <<240, 159, 152, 131>>}, {"happy", <<222, 222, 222, 222>>}],
        {ok, S} = emoji:start(Initial),
        ?assertMatch({error, _}, emoji:new_shortcode(S, "smiley", <<77, 77, 77, 77>>))
    end },
    {"Register same second shortcode to two unique value Initial",
    fun () ->
        Initial = [{"smiley", <<240, 159, 152, 131>>}, {"happy", <<222, 222, 222, 222>>}],
        {ok, S} = emoji:start(Initial),
        ?assertMatch({error, _}, emoji:new_shortcode(S, "happy", <<77, 77, 77, 77>>))
    end }].

test_delete() ->
    [{"Delete shortcode from empty Initial",
    fun () ->
        Initial = [],
        {ok, S} = emoji:start(Initial),
        emoji:delete(S, "smiley")
    end },
    {"Delete shortcode from one unique value Initial",
    fun () ->
        Initial = [{"smiley", <<240, 159, 152, 131>>}],
        {ok, S} = emoji:start(Initial),
        emoji:delete(S, "smiley")
    end }].

test_alias() -> 
    [{"Alias of shortcode which doesnt exist",
    fun () ->
      Initial = [],
      {ok, S} = emoji:start(Initial),
      ?assertMatch({error, _}, emoji:alias(S, "smiley", "other_smiley"))
    end },
    {"Alias of shortcode which exists",
    fun () ->
      Initial = [{"smiley", <<240, 159, 152, 131>>}],
      {ok, S} = emoji:start(Initial),
      ?assertMatch({ok, _}, emoji:alias(S, "smiley", "other_smiley"))
    end },
    {"Alias of shortcode 2 which exists",
    fun () ->
      Initial = [{"smiley", <<240, 159, 152, 131>>},{"other_smiley", <<240, 159, 152, 131>>}],
      {ok, S} = emoji:start(Initial),
      ?assertMatch({error, _}, emoji:alias(S, "smiley", "other_smiley"))
    end },
    {"Alias of alias of shortcode which exists",
    fun () ->
      Initial = [{"smiley", <<240, 159, 152, 131>>},{"other_smiley", <<240, 159, 152, 131>>}],
      {ok, S} = emoji:start(Initial),
      ?assertMatch({ok, _}, emoji:alias(S, "other_smiley", "cute_smiley"))
    end }].

test_lookup() -> 
    [{"Lookup of shortcode which doesnt exist",
    fun () ->
      Initial = [],
      {ok, S} = emoji:start(Initial),
      ?assertEqual(no_emoji, emoji:lookup(S, "smiley"))
    end },
    {"Lookup of shortcode which exists",
    fun () ->
      Initial = [{"smiley", <<240, 159, 152, 131>>}],
      {ok, S} = emoji:start(Initial),
      ?assertEqual({ok, <<240, 159, 152, 131>>}, emoji:lookup(S, "smiley"))
    end },
    {"Lookup of shortcode with alias which exists",
    fun () ->
      Initial = [{"smiley", <<240, 159, 152, 131>>},{"other_smiley", <<240, 159, 152, 131>>}],
      {ok, S} = emoji:start(Initial),
      ?assertEqual({ok, <<240, 159, 152, 131>>}, emoji:lookup(S, "other_smiley"))
    end },
    {"Lookup of shortcode in two shortcodes which exists",
    fun () ->
      Initial = [{"smiley", <<240, 159, 152, 131>>},{"happy", <<222, 222, 222, 222>>}],
      {ok, S} = emoji:start(Initial),
      ?assertEqual({ok, <<222, 222, 222, 222>>}, emoji:lookup(S, "happy"))
    end },
    {"Lookup of shortcode in two shortcodes which doesnt exists",
    fun () ->
      Initial = [{"smiley", <<240, 159, 152, 131>>},{"happy", <<222, 222, 222, 222>>}],
      {ok, S} = emoji:start(Initial),
      ?assertEqual(no_emoji, emoji:lookup(S, "love"))
    end }].