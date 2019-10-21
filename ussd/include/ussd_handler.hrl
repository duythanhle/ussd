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
-include_lib("C:\\Program Files\\erl9.3\\lib\\xmerl-1.3.16\\include\\xmerl.hrl").
-include_lib("eunit/include/eunit.hrl").

load_config() ->
  {Element, _} = xmerl_scan:file("config.xml", [{space, normalize}]),
  [Clean] = xmerl_lib:remove_whitespace([Element]),
  %io:format("Element without clean ~n~p~n", [Clean]),
  A = xmerl_lib:simplify_element(Clean),
  io:format("Element with clean ~n~p~n", [A]),
  Rules = A#rule.value,
  [Services] = [X#services.value || X<-Rules, is_record(X, services)],
  [Params] = [X#params.value || X<-Rules, is_record(X, params)],

  io:format("Services: ~n~p~n", [Services]),
  Map = maps:new(),
  lists:foreach(fun(X) -> maps:put(creat_key(X#service.key),creat_record(X#service.info), Map) end, Services),
  io:format("Map: ~n~p~n", [Map]).

creat_record(List) ->
  A = #info{},
  lists:foreach(fun(X) -> case X of
                            {name, [], [Value]} -> A#info{name = Value};
                            {activate, [], [Value]} -> A#info{activate = Value};
                            {query, [], [Value]} -> A#info{query = Value};
                            {cancel, [], [Value]} -> A#info{cancel = Value};
                            {description, [], [Value]} -> A#info{description = Value};
                            _Other -> _Other
                          end
                end, List),
  erlang:display(A),
  A.

creat_key([H|_T]) ->
  H#key.value.


%%Funtion to check the key of ussd string
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

%% Function to get service from 1stCode and 2ndCode
%% 43 => "cw"
%% 11 => invalid
get_service(FirstCode, SecondCode) ->
  maps:get(binary_to_list(<<FirstCode,SecondCode>>), ?UssdData, invalid).

get_service_with_telephone(FirstCode, SecondCode) ->
  maps:get(binary_to_list(<<FirstCode,SecondCode>>), ?UssdDataWithTelephone, invalid).
get_service_without_telephone(FirstCode, SecondCode) ->
  maps:get(binary_to_list(<<FirstCode,SecondCode>>), ?UssdDataWithoutTelephone, invalid).


test1() ->
  ?assertEqual("1", check_key("*#1#")),
  ?assertEqual("1", check_key("a1#")),
  ?assertEqual("1", check_key("a1aaaa")),
  ?assertEqual("123", check_key("*#@123*####")),
  ?assertEqual("123", check_key("*123*1#1##")),
  ?assertEqual(invalid, check_key("123*####")),
  ?assertEqual(invalid, check_key("****123")),
  ?assertEqual(invalid, check_key("123")).

