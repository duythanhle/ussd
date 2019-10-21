%%%-------------------------------------------------------------------
%%% @author thanhld19
%%% @copyright (C) 2019, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 17. Oct 2019 1:29 PM
%%%-------------------------------------------------------------------
-module(ussd_main).
-author("thanhld19").


%% API
-export([start/1, load_config/0, test1/0]).

-include("../include/ussd_handler.hrl").

start(Str) ->
  check_key(Str).

