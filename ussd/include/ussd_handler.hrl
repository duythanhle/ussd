%%%-------------------------------------------------------------------
%%% @author thanhld19
%%% @copyright (C) 2019, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 17. Oct 2019 11:06 AM
%%%-------------------------------------------------------------------
-author("thanhld19").

-include("ussd_define.hrl").
-include_lib("xmerl/include/xmerl.hrl").
-include_lib("eunit/include/eunit.hrl").


load_config() ->
  {Element, _} = xmerl_scan:file("config.xml", [{space, normalize}]),
  [RemoveWhitespace] = xmerl_lib:remove_whitespace([Element]),
  %io:format("Element without clean ~n~p~n", [Clean]),
  SimplifyElement = xmerl_lib:simplify_element(RemoveWhitespace),
%%  io:format("Simplify element ~n~p~n", [SimplifyElement]),
  Rules = SimplifyElement#rule.value,
%%  io:format("Rules ~n~p~n", [Rules]),
  lists:foldl(fun({service, [{key, Key}], Value}, MapsTemp) -> maps:put(Key, Value, MapsTemp)
              end, #{}, Rules).
  %io:format("Maps ~n~p~n", [Maps]).


%%Function to check the key of ussd string
%%Ex: *123354# => 123354
%%    123354*# => invalid
%%    *123354*#asdf  => 123354
check_key(Str)->
  check_key(Str, [], []).
check_key([], _ReturnValue, _NotInteger) ->
  invalid;
check_key([H|T], ReturnValue, NotInteger) ->
  case (H>=$0 andalso H =<$9) of
    true ->
      case NotInteger of
        [] ->
          invalid;
        _ ->
          check_key(T,[H|ReturnValue], NotInteger)
      end;
    _ ->
      case ReturnValue of
        [] ->
          check_key(T, ReturnValue, [H|NotInteger]);
        _ ->
          lists:reverse(ReturnValue)
      end
  end.

handle_ussd1(Str, MapsServices) ->
  case check_key(Str) of
    invalid ->
      invalid;
    Key ->
      case maps:get(Key, MapsServices, invalid) of
        invalid ->
          invalid;
        List ->
          traverse_list(List, Str)
      end
  end.

traverse_list([], _Str) ->
  invalid;
traverse_list([?value(Function, Module, USSDCode, Description)|T], Str) ->
  case Description of
    [] ->
      case Str of
        USSDCode ->
          {Module, Function, []};
        _ ->
          traverse_list(T, Str)
      end;
    [StringDescription] ->
      ListDescription = string:tokens(StringDescription, " "),
      case re:run(Str, nomalize(USSDCode, ListDescription), [{capture, ListDescription, list}]) of
        {match, ReturnParams} ->
          {Module, Function, ReturnParams};
        _ ->
          traverse_list(T, Str)
      end;
    _ ->
      io:format("Description Error!~n")
  end.

nomalize(USSDCode, Description) ->
  nomalize(USSDCode, Description, []).
nomalize([], Description, List) ->
  Temp = lists:reverse(List),
  lists:foldl(fun(X, A) -> re:replace(A, X, ["(?<",X,">.*)"], [global, {return, list}])
              end, Temp, Description);
  %re:replace(Temp, Description, "(?<FOO>.*)", [global, {return, list}]);
nomalize([H|T], Des, List) ->
  case (H) of
    $* ->
      nomalize(T, Des, [$],H,$[|List]);
    $# ->
      nomalize(T, Des, [$],H,$[|List]);
    _ ->
      nomalize(T, Des, [H|List])
  end.

loop(0, _) ->
  ok;
loop(Count, F) ->
  F(),
  loop(Count-1, F).

tc(F) ->
  T1 = erlang:monotonic_time(),
  loop(100000, F),
  T2 = erlang:monotonic_time(),
  Time = erlang:convert_time_unit(T2 - T1, native, microsecond),
  Time.

test1() ->
  test_check_key(),
  test_load_config1().

test_check_key() ->
  ?assertEqual("1", check_key("*#1#")),
  ?assertEqual("1", check_key("a1#")),
  ?assertEqual("1", check_key("a1aaaa")),
  ?assertEqual("123", check_key("*#@123*####")),
  ?assertEqual("123", check_key("*123*1#1##")),
  ?assertEqual(invalid, check_key("123*####")),
  ?assertEqual(invalid, check_key("****123")),
  ?assertEqual(invalid, check_key("123")).

test_load_config1() ->
  MapsServices = load_config(),

  ?assertEqual("[*]11[*](?<TN>.*)", nomalize("*11*TN",["TN"])),
  ?assertEqual("[*]11[*](?<TN>.*)[#]", nomalize("*11*TN#",["TN"])),
  ?assertEqual("[*]11[*](?<TN>.*)[*](?<MN>.*)", nomalize("*11*TN*MN",["TN", "MN"])),

  ?assertEqual(invalid, handle_ussd1("*633*63*##", MapsServices)),

  ?assertEqual({cfu, active, ["010102"]}, handle_ussd1("*21*010102#", MapsServices)),
  ?assertEqual({cfu, active, ["*010102"]}, handle_ussd1("*21**010102#", MapsServices)),
  ?assertEqual({cfu, active, ["*010102#"]}, handle_ussd1("*21**010102##", MapsServices)),
  ?assertEqual(invalid, handle_ussd1("*33**010102##", MapsServices)),
  ?assertEqual({cfu, active, ["TN"]}, handle_ussd1("*21*TN#", MapsServices)),
  ?assertEqual({test, activate, ["*010102", "33#"]}, handle_ussd1("*99**010102*33##", MapsServices)).

  %F = fun() -> handle_ussd1("*99**010102*33##", MapsServices) end,
  %tc(F).


