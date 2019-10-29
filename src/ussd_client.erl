%%%-------------------------------------------------------------------
%%% @author thanhld19
%%% @copyright (C) 2019, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 17. Oct 2019 1:29 PM
%%%-------------------------------------------------------------------
-module(ussd_client).
-author("thanhld19").


%% API
-export([load_config/0, test_all/0, send_ussd/2, test_10000/0, test_performance/2, test_ussd/0, test_1000000/0, test_100000/0]).

-include("../include/ussd_handler.hrl").

send_ussd(String, call) ->
  ussd_server:do_call({ussd, String});
send_ussd(String, cast) ->
  ussd_server:do_cast({ussd, String, self()}),
  receive
    Mgs ->
      Mgs
  end.

random_valid_ussd(Maps) ->
  Values = maps:values(Maps),
  Functions = lists:nth(rand:uniform(length(Values)), Values),
  ?value(_, _, UssdCode, Description) = lists:nth(rand:uniform(length(Functions)), Functions),
  case Description of
    [] ->
      UssdCode;
    [TempDes] ->
      lists:foldl(fun(X, A) ->
        re:replace(A, X, binary_to_list(base64:encode(crypto:strong_rand_bytes(9))), [global, {return, list}])
                  end, UssdCode, string:tokens(TempDes, " "))
  end.

random_ussd(Maps) ->
  lists:nth(rand:uniform(4), ["*", "*#", "#", binary_to_list(crypto:strong_rand_bytes(1))]) ++
    lists:nth(rand:uniform(length(maps:keys(Maps))), maps:keys(Maps)) ++
    lists:nth(rand:uniform(2), ["", binary_to_list(crypto:strong_rand_bytes(1))]) ++
    lists:nth(rand:uniform(2), ["#", binary_to_list(crypto:strong_rand_bytes(1))]).

test_performance(N, Option) ->
  Maps = load_config(),
  T1 = erlang:monotonic_time(),
  ListValidUssd = [random_valid_ussd(Maps) || _X <- lists:seq(1, N)],
  T2 = erlang:monotonic_time(),
  ListRandomString = [binary_to_list(base64:encode(crypto:strong_rand_bytes(9))) || _X <- lists:seq(1, N)],
  T3 = erlang:monotonic_time(),
  ListRandomUssd = [random_ussd(Maps) || _X <- lists:seq(1, N)],
  T4 = erlang:monotonic_time(),
  Map = #{active => 0, query =>0, deactive=>0, invalid=>0},
  Map1 = lists:foldr(fun(X, MapTemp) -> case send_ussd(X, Option) of
                                          {_, active, _} ->
                                            maps:update_with(active, fun(V) -> V + 1 end, MapTemp);
                                          {_, query, _} ->
                                            maps:update_with(query, fun(V) -> V + 1 end, MapTemp);
                                          {_, deactive, _} ->
                                            maps:update_with(deactive, fun(V) -> V + 1 end, MapTemp);
                                          invalid ->
                                            maps:update_with(invalid, fun(V) -> V + 1 end, MapTemp)
                                        end
                     end, Map, ListValidUssd),
  io:format("Map1: ~p~n", [Map1]),
  T5 = erlang:monotonic_time(),
  %lists:foreach(fun(X) -> invalid = send_ussd(X, Option) end, ListRandomString),
  Map2 = lists:foldr(fun(X, MapTemp) -> case send_ussd(X, Option) of
                                          {_, active, _} ->
                                            maps:update_with(active, fun(V) -> V + 1 end, MapTemp);
                                          {_, query, _} ->
                                            maps:update_with(query, fun(V) -> V + 1 end, MapTemp);
                                          {_, deactive, _} ->
                                            maps:update_with(deactive, fun(V) -> V + 1 end, MapTemp);
                                          invalid ->
                                            maps:update_with(invalid, fun(V) -> V + 1 end, MapTemp)
                                        end
                     end, Map1, ListRandomString),
  io:format("Map2: ~p~n", [Map2]),
  T6 = erlang:monotonic_time(),
  %lists:foreach(fun(X) -> send_ussd(X, Option) end, ListRandomUssd),
  Map3 = lists:foldr(fun(X, MapTemp) -> case send_ussd(X, Option) of
                                          {_, active, _} ->
                                            maps:update_with(active, fun(V) -> V + 1 end, MapTemp);
                                          {_, query, _} ->
                                            maps:update_with(query, fun(V) -> V + 1 end, MapTemp);
                                          {_, deactive, _} ->
                                            maps:update_with(deactive, fun(V) -> V + 1 end, MapTemp);
                                          invalid ->
                                            maps:update_with(invalid, fun(V) -> V + 1 end, MapTemp)
                                        end
                     end, Map2, ListRandomUssd),
  io:format("Map3: ~p~n", [Map3]),
  T7 = erlang:monotonic_time(),

  Time1 = erlang:convert_time_unit(T2 - T1, native, millisecond),
  Time2 = erlang:convert_time_unit(T3 - T2, native, millisecond),
  Time3 = erlang:convert_time_unit(T4 - T3, native, millisecond),
  Time4 = erlang:convert_time_unit(T5 - T4, native, millisecond),
  Time5 = erlang:convert_time_unit(T6 - T5, native, millisecond),
  Time6 = erlang:convert_time_unit(T7 - T6, native, millisecond),
  io:format("Time for initializing ~p valid ussd: ~p milliseconds~n", [N, Time1]),
  io:format("Time for initializing ~p invalid ussd: ~p milliseconds~n", [N, Time2]),
  io:format("Time for initializing ~p random ussd: ~p milliseconds~n", [N, Time3]),
  io:format("Time for processing ~p valid ussd: ~p milliseconds~n", [N, Time4]),
  io:format("Time for processing ~p invalid ussd: ~p milliseconds~n", [N, Time5]),
  io:format("Time for processing ~p random ussd: ~p milliseconds~n", [N, Time6]),
  io:format("Total time for processing ~p ussd: ~p milliseconds~n", [3 * N, Time4 + Time5 + Time6]),
  io:format("Time for processing 1 ussd: ~p milliseconds~n", [(Time4 + Time5 + Time6) / (3 * N)]).

test_all() ->
  test_check_key(),
  test_normalize(),
  test_ussd().

%%Test check_key function
test_check_key() ->
  ?assertEqual("1", check_key("*1#")),
  ?assertEqual("1", check_key("*#1#*")),
  ?assertEqual("1", check_key("a1#")),
  ?assertEqual("1", check_key("aa1aaaa")),
  ?assertEqual("123", check_key("*#@123*####")),
  ?assertEqual("123", check_key("*123*1#1##")),
  ?assertEqual(invalid, check_key("123*####")),
  ?assertEqual(invalid, check_key("****123")),
  ?assertEqual(invalid, check_key("123")).

%%Test normalize function
test_normalize() ->
  ?assertEqual("^11(?<TN>.*)$", normalize("11TN", ["TN"])),
  ?assertEqual("^[*]11[*](?<TN>.*)$", normalize("*11*TN", ["TN"])),
  ?assertEqual("^[*][*]11[#](?<TN>.*)$", normalize("**11#TN", ["TN"])),
  ?assertEqual("^[*]11[*](?<TN>.*)[#]$", normalize("*11*TN#", ["TN"])),
  ?assertEqual("^[*]11[*](?<TN>.*)[*](?<MN>.*)$", normalize("*11*TN*MN", ["TN", "MN"])).

test_ussd() ->
  MapsServices = load_config(),

  %%Test 43-cw service
  ?assertEqual({cw, active, []}, handle_ussd("*43#", MapsServices)),
  ?assertEqual({cw, query, []}, handle_ussd("*#43#", MapsServices)),
  ?assertEqual({cw, deactive, []}, handle_ussd("#43#", MapsServices)),
  ?assertEqual(invalid, handle_ussd("43", MapsServices)),
  ?assertEqual(invalid, handle_ussd("43555", MapsServices)),

  %%Test 21-cfu service
  ?assertEqual({cfu, active, ["viettel2019"]}, handle_ussd("*21*viettel2019#", MapsServices)),
  ?assertEqual({cfu, query, []}, handle_ussd("*#21#", MapsServices)),
  ?assertEqual({cfu, deactive, []}, handle_ussd("#21#", MapsServices)),
  ?assertEqual(invalid, handle_ussd("**21*0987654321#", MapsServices)),

  %%Test 67-cfb service
  ?assertEqual({cfb, active, ["viettel2019"]}, handle_ussd("*67*viettel2019#", MapsServices)),
  ?assertEqual({cfb, query, []}, handle_ussd("*#67#", MapsServices)),
  ?assertEqual({cfb, deactive, []}, handle_ussd("#67#", MapsServices)),
  ?assertEqual(invalid, handle_ussd("**67*0987654367#", MapsServices)),

  %%Test 61-cfnr service
  ?assertEqual({cfnr, active, ["viettel2019"]}, handle_ussd("*61*viettel2019#", MapsServices)),
  ?assertEqual({cfnr, query, []}, handle_ussd("*#61#", MapsServices)),
  ?assertEqual({cfnr, deactive, []}, handle_ussd("#61#", MapsServices)),
  ?assertEqual(invalid, handle_ussd("**61*0987654321#", MapsServices)),

  %%Test 62-cfnrc service
  ?assertEqual({cfnrc, active, ["viettel2019"]}, handle_ussd("*62*viettel2019#", MapsServices)),
  ?assertEqual({cfnrc, query, []}, handle_ussd("*#62#", MapsServices)),
  ?assertEqual({cfnrc, deactive, []}, handle_ussd("#62#", MapsServices)),
  ?assertEqual(invalid, handle_ussd("**62*0987654321#", MapsServices)),

  %%Test 63-cfnl service
  ?assertEqual({cfnl, active, ["viettel2019"]}, handle_ussd("*63*viettel2019#", MapsServices)),
  ?assertEqual({cfnl, query, []}, handle_ussd("*#63#", MapsServices)),
  ?assertEqual({cfnl, deactive, []}, handle_ussd("#63#", MapsServices)),
  ?assertEqual(invalid, handle_ussd("**63*0987654321#", MapsServices)),

  %%Test 95-conf3pty service
  ?assertEqual({conf3pty, active, []}, handle_ussd("*95#", MapsServices)),
  ?assertEqual({conf3pty, query, []}, handle_ussd("*#95#", MapsServices)),
  ?assertEqual({conf3pty, deactive, []}, handle_ussd("#95#", MapsServices)),
  ?assertEqual(invalid, handle_ussd("*95##", MapsServices)),








  %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
  %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
  %% Test invalid ussd code with invalid key
  ?assertEqual(invalid, handle_ussd("", MapsServices)),
  ?assertEqual(invalid, handle_ussd("43", MapsServices)),
  ?assertEqual(invalid, handle_ussd("*43", MapsServices)),
  ?assertEqual(invalid, handle_ussd("43#", MapsServices)),
  ?assertEqual(invalid, handle_ussd("a43#", MapsServices)),
  ?assertEqual(invalid, handle_ussd("a43a", MapsServices)),

  %%Test invalid ussd code with not define in config file
  ?assertEqual(invalid, handle_ussd("*33#", MapsServices)),
  ?assertEqual(invalid, handle_ussd("*11*", MapsServices)),
  ?assertEqual(invalid, handle_ussd("*00*", MapsServices)),
  ?assertEqual(invalid, handle_ussd("*22*112#", MapsServices)),
  ?assertEqual(invalid, handle_ussd("*633*63*##", MapsServices)),


  % Invalid ussd code - active
  ?assertEqual(invalid, handle_ussd("*43##", MapsServices)),
  ?assertEqual(invalid, handle_ussd("**43#", MapsServices)),
  ?assertEqual(invalid, handle_ussd("*43#*43#", MapsServices)),
  ?assertEqual(invalid, handle_ussd("*43#1", MapsServices)),
  ?assertEqual(invalid, handle_ussd("*43*1#", MapsServices)),
  % Invalid ussd code - query
  ?assertEqual(invalid, handle_ussd("**#43#", MapsServices)),
  ?assertEqual(invalid, handle_ussd("*##43#", MapsServices)),
  ?assertEqual(invalid, handle_ussd("*1#43#", MapsServices)),
  ?assertEqual(invalid, handle_ussd("*#43#1", MapsServices)),
  ?assertEqual(invalid, handle_ussd("*#43##", MapsServices)),
  ?assertEqual(invalid, handle_ussd("1*#43#", MapsServices)),
  ?assertEqual(invalid, handle_ussd("*#43", MapsServices)),
  ?assertEqual(invalid, handle_ussd("*#43#*#43#", MapsServices)),
  % Invalid ussd code - deactive
  ?assertEqual(invalid, handle_ussd("#43##", MapsServices)),
  ?assertEqual(invalid, handle_ussd("##43#", MapsServices)),
  ?assertEqual(invalid, handle_ussd("1#43#", MapsServices)),
  ?assertEqual(invalid, handle_ussd("#43#1", MapsServices)),
  ?assertEqual(invalid, handle_ussd("#43##43#", MapsServices)),
  ?assertEqual(invalid, handle_ussd("#43*#", MapsServices)),
  ?assertEqual(invalid, handle_ussd("#43*1#", MapsServices)),

  %%Test 21-cfu service
  % Valid ussd code
  ?assertEqual({cfu, active, ["0987654321"]}, handle_ussd("*21*0987654321#", MapsServices)),
  ?assertEqual({cfu, active, ["010102"]}, handle_ussd("*21*010102#", MapsServices)),
  ?assertEqual({cfu, active, ["*010102"]}, handle_ussd("*21**010102#", MapsServices)),
  ?assertEqual({cfu, active, ["*010102#"]}, handle_ussd("*21**010102##", MapsServices)),
  ?assertEqual({cfu, active, ["TN"]}, handle_ussd("*21*TN#", MapsServices)),
  ?assertEqual({cfu, active, ["viettel2019"]}, handle_ussd("*21*viettel2019#", MapsServices)),
  ?assertEqual({cfu, query, []}, handle_ussd("*#21#", MapsServices)),
  ?assertEqual({cfu, deactive, []}, handle_ussd("#21#", MapsServices)),
  % Invalid ussd code - active
  ?assertEqual(invalid, handle_ussd("**21*0987654321#", MapsServices)),
  ?assertEqual(invalid, handle_ussd("*21*0987654321", MapsServices)),
  ?assertEqual(invalid, handle_ussd("*21a0987654321#", MapsServices)),
  ?assertEqual(invalid, handle_ussd("$21*0987654321#", MapsServices)),
  % Invalid ussd code - query
  ?assertEqual(invalid, handle_ussd("**#21#", MapsServices)),
  ?assertEqual(invalid, handle_ussd("*##21#", MapsServices)),
  ?assertEqual(invalid, handle_ussd("*1#21#", MapsServices)),
  ?assertEqual(invalid, handle_ussd("*#21#1", MapsServices)),
  ?assertEqual(invalid, handle_ussd("*#21##", MapsServices)),
  ?assertEqual(invalid, handle_ussd("1*#21#", MapsServices)),
  ?assertEqual(invalid, handle_ussd("*#21", MapsServices)),
  ?assertEqual(invalid, handle_ussd("*#21#*#21#", MapsServices)),
  % Invalid ussd code - deactive
  ?assertEqual(invalid, handle_ussd("#21##", MapsServices)),
  ?assertEqual(invalid, handle_ussd("##21#", MapsServices)),
  ?assertEqual(invalid, handle_ussd("1#21#", MapsServices)),
  ?assertEqual(invalid, handle_ussd("#21#1", MapsServices)),
  ?assertEqual(invalid, handle_ussd("#21##21#", MapsServices)),
  ?assertEqual(invalid, handle_ussd("#21*#", MapsServices)),
  ?assertEqual(invalid, handle_ussd("#21*1#", MapsServices)),

  %%Test 67-cfb service
  % Valid ussd code
  ?assertEqual({cfb, active, ["0987654367"]}, handle_ussd("*67*0987654367#", MapsServices)),
  ?assertEqual({cfb, active, ["010102"]}, handle_ussd("*67*010102#", MapsServices)),
  ?assertEqual({cfb, active, ["*010102"]}, handle_ussd("*67**010102#", MapsServices)),
  ?assertEqual({cfb, active, ["*010102#"]}, handle_ussd("*67**010102##", MapsServices)),
  ?assertEqual({cfb, active, ["TN"]}, handle_ussd("*67*TN#", MapsServices)),
  ?assertEqual({cfb, active, ["viettel2019"]}, handle_ussd("*67*viettel2019#", MapsServices)),
  ?assertEqual({cfb, query, []}, handle_ussd("*#67#", MapsServices)),
  ?assertEqual({cfb, deactive, []}, handle_ussd("#67#", MapsServices)),
  % Invalid ussd code - active
  ?assertEqual(invalid, handle_ussd("**67*0987654367#", MapsServices)),
  ?assertEqual(invalid, handle_ussd("*67*0987654367", MapsServices)),
  ?assertEqual(invalid, handle_ussd("*67a0987654367#", MapsServices)),
  ?assertEqual(invalid, handle_ussd("$67*0987654367#", MapsServices)),
  % Invalid ussd code - query
  ?assertEqual(invalid, handle_ussd("**#67#", MapsServices)),
  ?assertEqual(invalid, handle_ussd("*##67#", MapsServices)),
  ?assertEqual(invalid, handle_ussd("*1#67#", MapsServices)),
  ?assertEqual(invalid, handle_ussd("*#67#1", MapsServices)),
  ?assertEqual(invalid, handle_ussd("*#67##", MapsServices)),
  ?assertEqual(invalid, handle_ussd("1*#67#", MapsServices)),
  ?assertEqual(invalid, handle_ussd("*#67", MapsServices)),
  ?assertEqual(invalid, handle_ussd("*#67#*#67#", MapsServices)),
  % Invalid ussd code - deactive
  ?assertEqual(invalid, handle_ussd("#67##", MapsServices)),
  ?assertEqual(invalid, handle_ussd("##67#", MapsServices)),
  ?assertEqual(invalid, handle_ussd("1#67#", MapsServices)),
  ?assertEqual(invalid, handle_ussd("#67#1", MapsServices)),
  ?assertEqual(invalid, handle_ussd("#67##67#", MapsServices)),
  ?assertEqual(invalid, handle_ussd("#67*#", MapsServices)),
  ?assertEqual(invalid, handle_ussd("#67*1#", MapsServices)),


  %%Test 61-cfnr service
  % Valid ussd code
  ?assertEqual({cfnr, active, ["0987654321"]}, handle_ussd("*61*0987654321#", MapsServices)),
  ?assertEqual({cfnr, active, ["010102"]}, handle_ussd("*61*010102#", MapsServices)),
  ?assertEqual({cfnr, active, ["*010102"]}, handle_ussd("*61**010102#", MapsServices)),
  ?assertEqual({cfnr, active, ["*010102#"]}, handle_ussd("*61**010102##", MapsServices)),
  ?assertEqual({cfnr, active, ["TN"]}, handle_ussd("*61*TN#", MapsServices)),
  ?assertEqual({cfnr, active, ["viettel2019"]}, handle_ussd("*61*viettel2019#", MapsServices)),
  ?assertEqual({cfnr, query, []}, handle_ussd("*#61#", MapsServices)),
  ?assertEqual({cfnr, deactive, []}, handle_ussd("#61#", MapsServices)),
  % Invalid ussd code - active
  ?assertEqual(invalid, handle_ussd("**61*0987654321#", MapsServices)),
  ?assertEqual(invalid, handle_ussd("*61*0987654361", MapsServices)),
  ?assertEqual(invalid, handle_ussd("*61a0987654361#", MapsServices)),
  ?assertEqual(invalid, handle_ussd("$61*0987654361#", MapsServices)),
  % Invalid ussd code - query
  ?assertEqual(invalid, handle_ussd("**#61#", MapsServices)),
  ?assertEqual(invalid, handle_ussd("*##61#", MapsServices)),
  ?assertEqual(invalid, handle_ussd("*1#61#", MapsServices)),
  ?assertEqual(invalid, handle_ussd("*#61#1", MapsServices)),
  ?assertEqual(invalid, handle_ussd("*#61##", MapsServices)),
  ?assertEqual(invalid, handle_ussd("1*#61#", MapsServices)),
  ?assertEqual(invalid, handle_ussd("*#61", MapsServices)),
  ?assertEqual(invalid, handle_ussd("*#61#*#61#", MapsServices)),
  % Invalid ussd code - deactive
  ?assertEqual(invalid, handle_ussd("#61##", MapsServices)),
  ?assertEqual(invalid, handle_ussd("##61#", MapsServices)),
  ?assertEqual(invalid, handle_ussd("1#61#", MapsServices)),
  ?assertEqual(invalid, handle_ussd("#61#1", MapsServices)),
  ?assertEqual(invalid, handle_ussd("#61##61#", MapsServices)),
  ?assertEqual(invalid, handle_ussd("#61*#", MapsServices)),
  ?assertEqual(invalid, handle_ussd("#61*1#", MapsServices)),

  %%Test 62-cfnrc service
  % Valid ussd code
  ?assertEqual({cfnrc, active, ["0987654321"]}, handle_ussd("*62*0987654321#", MapsServices)),
  ?assertEqual({cfnrc, active, ["010102"]}, handle_ussd("*62*010102#", MapsServices)),
  ?assertEqual({cfnrc, active, ["*010102"]}, handle_ussd("*62**010102#", MapsServices)),
  ?assertEqual({cfnrc, active, ["*010102#"]}, handle_ussd("*62**010102##", MapsServices)),
  ?assertEqual({cfnrc, active, ["TN"]}, handle_ussd("*62*TN#", MapsServices)),
  ?assertEqual({cfnrc, active, ["viettel2019"]}, handle_ussd("*62*viettel2019#", MapsServices)),
  ?assertEqual({cfnrc, query, []}, handle_ussd("*#62#", MapsServices)),
  ?assertEqual({cfnrc, deactive, []}, handle_ussd("#62#", MapsServices)),
  % Invalid ussd code - active
  ?assertEqual(invalid, handle_ussd("**62*0987654321#", MapsServices)),
  ?assertEqual(invalid, handle_ussd("*62*0987654362", MapsServices)),
  ?assertEqual(invalid, handle_ussd("*62a0987654362#", MapsServices)),
  ?assertEqual(invalid, handle_ussd("$62*0987654362#", MapsServices)),
  % Invalid ussd code - query
  ?assertEqual(invalid, handle_ussd("**#62#", MapsServices)),
  ?assertEqual(invalid, handle_ussd("*##62#", MapsServices)),
  ?assertEqual(invalid, handle_ussd("*1#62#", MapsServices)),
  ?assertEqual(invalid, handle_ussd("*#62#1", MapsServices)),
  ?assertEqual(invalid, handle_ussd("*#62##", MapsServices)),
  ?assertEqual(invalid, handle_ussd("1*#62#", MapsServices)),
  ?assertEqual(invalid, handle_ussd("*#62", MapsServices)),
  ?assertEqual(invalid, handle_ussd("*#62#*#62#", MapsServices)),
  % Invalid ussd code - deactive
  ?assertEqual(invalid, handle_ussd("#62##", MapsServices)),
  ?assertEqual(invalid, handle_ussd("##62#", MapsServices)),
  ?assertEqual(invalid, handle_ussd("1#62#", MapsServices)),
  ?assertEqual(invalid, handle_ussd("#62#1", MapsServices)),
  ?assertEqual(invalid, handle_ussd("#62##62#", MapsServices)),
  ?assertEqual(invalid, handle_ussd("#62*#", MapsServices)),
  ?assertEqual(invalid, handle_ussd("#62*1#", MapsServices)),


  %%Test 63-cfnl service
  % Valid ussd code
  ?assertEqual({cfnl, active, ["0987654321"]}, handle_ussd("*63*0987654321#", MapsServices)),
  ?assertEqual({cfnl, active, ["010102"]}, handle_ussd("*63*010102#", MapsServices)),
  ?assertEqual({cfnl, active, ["*010102"]}, handle_ussd("*63**010102#", MapsServices)),
  ?assertEqual({cfnl, active, ["*010102#"]}, handle_ussd("*63**010102##", MapsServices)),
  ?assertEqual({cfnl, active, ["TN"]}, handle_ussd("*63*TN#", MapsServices)),
  ?assertEqual({cfnl, active, ["viettel2019"]}, handle_ussd("*63*viettel2019#", MapsServices)),
  ?assertEqual({cfnl, query, []}, handle_ussd("*#63#", MapsServices)),
  ?assertEqual({cfnl, deactive, []}, handle_ussd("#63#", MapsServices)),
  % Invalid ussd code - active
  ?assertEqual(invalid, handle_ussd("**63*0987654321#", MapsServices)),
  ?assertEqual(invalid, handle_ussd("*63*0987654363", MapsServices)),
  ?assertEqual(invalid, handle_ussd("*63a0987654363#", MapsServices)),
  ?assertEqual(invalid, handle_ussd("$63*0987654363#", MapsServices)),
  % Invalid ussd code - query
  ?assertEqual(invalid, handle_ussd("**#63#", MapsServices)),
  ?assertEqual(invalid, handle_ussd("*##63#", MapsServices)),
  ?assertEqual(invalid, handle_ussd("*1#63#", MapsServices)),
  ?assertEqual(invalid, handle_ussd("*#63#1", MapsServices)),
  ?assertEqual(invalid, handle_ussd("*#63##", MapsServices)),
  ?assertEqual(invalid, handle_ussd("1*#63#", MapsServices)),
  ?assertEqual(invalid, handle_ussd("*#63", MapsServices)),
  ?assertEqual(invalid, handle_ussd("*#63#*#63#", MapsServices)),
  % Invalid ussd code - deactive
  ?assertEqual(invalid, handle_ussd("#63##", MapsServices)),
  ?assertEqual(invalid, handle_ussd("##63#", MapsServices)),
  ?assertEqual(invalid, handle_ussd("1#63#", MapsServices)),
  ?assertEqual(invalid, handle_ussd("#63#1", MapsServices)),
  ?assertEqual(invalid, handle_ussd("#63##63#", MapsServices)),
  ?assertEqual(invalid, handle_ussd("#63*#", MapsServices)),
  ?assertEqual(invalid, handle_ussd("#63*1#", MapsServices)),


  %%Test 95-conf3pty service
  % Valid ussd code
  ?assertEqual({conf3pty, active, []}, handle_ussd("*95#", MapsServices)),
  ?assertEqual({conf3pty, query, []}, handle_ussd("*#95#", MapsServices)),
  ?assertEqual({conf3pty, deactive, []}, handle_ussd("#95#", MapsServices)),

  % Invalid ussd code - active
  ?assertEqual(invalid, handle_ussd("*95##", MapsServices)),
  ?assertEqual(invalid, handle_ussd("**95#", MapsServices)),
  ?assertEqual(invalid, handle_ussd("*95#*95#", MapsServices)),
  ?assertEqual(invalid, handle_ussd("*95#1", MapsServices)),
  ?assertEqual(invalid, handle_ussd("*95*1#", MapsServices)),
  % Invalid ussd code - query
  ?assertEqual(invalid, handle_ussd("**#95#", MapsServices)),
  ?assertEqual(invalid, handle_ussd("*##95#", MapsServices)),
  ?assertEqual(invalid, handle_ussd("*1#95#", MapsServices)),
  ?assertEqual(invalid, handle_ussd("*#95#1", MapsServices)),
  ?assertEqual(invalid, handle_ussd("*#95##", MapsServices)),
  ?assertEqual(invalid, handle_ussd("1*#95#", MapsServices)),
  ?assertEqual(invalid, handle_ussd("*#95", MapsServices)),
  ?assertEqual(invalid, handle_ussd("*#95#*#95#", MapsServices)),
  % Invalid ussd code - deactive
  ?assertEqual(invalid, handle_ussd("#95##", MapsServices)),
  ?assertEqual(invalid, handle_ussd("##95#", MapsServices)),
  ?assertEqual(invalid, handle_ussd("1#95#", MapsServices)),
  ?assertEqual(invalid, handle_ussd("#95#1", MapsServices)),
  ?assertEqual(invalid, handle_ussd("#95##95#", MapsServices)),
  ?assertEqual(invalid, handle_ussd("#95*#", MapsServices)),
  ?assertEqual(invalid, handle_ussd("#95*1#", MapsServices)),

  %% Additional test
  ?assertEqual({test, active, ["*010102", "33#"]}, handle_ussd("*99**010102*33##", MapsServices)).

loop(0, _) ->
  ok;
loop(Count, F) ->
  F(),
  loop(Count - 1, F).
tc(F, N) ->
  T1 = erlang:monotonic_time(),
  loop(N, F),
  T2 = erlang:monotonic_time(),
  erlang:convert_time_unit(T2 - T1, native, millisecond).
test_10000() ->
  Maps = load_config(),
  Time = tc(fun() -> handle_ussd("*99**010102*33##", Maps) end, 10000),
  io:format("Testing 10.000 cases with: ~p milliseconds~n", [Time]),
  io:format("Avg time: ~p milliseconds~n", [Time/10000]).
test_100000() ->
  Maps = load_config(),
  Time = tc(fun() -> handle_ussd("*99**010102*33##", Maps) end, 100000),
  io:format("Testing 100.000 cases with: ~p milliseconds~n", [Time]),
  io:format("Avg time: ~p milliseconds~n", [Time/100000]).

test_1000000() ->
  Maps = load_config(),
  Time = tc(fun() -> handle_ussd("*99**010102*33##", Maps) end, 1000000),
  io:format("Testing 1.000.000 cases with: ~p milliseconds~n", [Time]),
  io:format("Avg time: ~p milliseconds~n", [Time/1000000]).

