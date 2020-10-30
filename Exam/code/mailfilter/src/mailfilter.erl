-module(mailfilter).

% You are allowed to split your Erlang code in as many files as you
% find appropriate.
% However, you MUST have a module (this file) called mailfilter.

% Export at least the API:
-export(
  [ start/1
  , stop/1
  , default/4
  , add_mail/2
  , get_config/1
  , enough/1
  , add_filter/4
  ]).

% You may have other exports as well
-export([]).

% API :

start(_Cap) ->
  not_implemented.

stop(_MS) ->
  not_implemented.

add_mail(_MS, _Mail) ->
  not_implemented.

get_config(_MR) ->
  not_implemented.

default(_MS, _Label, _Filt, _Data) ->
  not_implemented.

enough(_MR) ->
  not_implemented.

add_filter(_MR, _Label, _Filt, _Data) ->
  not_implemented.
