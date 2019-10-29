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

%%Function to check the key of ussd string
%%Ex: *123354# => 123354
%%    123354*# => invalid
%%    *123354*#asdf  => 123354
check_key(Str) ->
  check_key(Str, [], []).
check_key([], _ReturnValue, _NotInteger) ->
  invalid;
check_key([H | T], ReturnValue, NotInteger) ->
  case (H >= $0 andalso H =< $9) of
    true ->
      case NotInteger of
        [] ->
          invalid;
        _ ->
          check_key(T, [H | ReturnValue], NotInteger)
      end;
    _ ->
      case ReturnValue of
        [] ->
          check_key(T, ReturnValue, [H | NotInteger]);
        _ ->
          lists:reverse(ReturnValue)
      end
  end.


handle_ussd(Str, MapsServices) ->
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
traverse_list([?value(Function, Module, USSDCode, Description) | T], Str) ->
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
      case re:run(Str, normalize(USSDCode, ListDescription), [{capture, ListDescription, list}]) of
        {match, ReturnParams} ->
          {Module, Function, ReturnParams};
        _ ->
          traverse_list(T, Str)
      end;
    _ ->
      io:format("Description Error!~n")
  end.

normalize(USSDCode, Description) ->
  normalize(USSDCode, Description, []).
normalize([], Description, List) ->
  Temp = "^" ++ lists:reverse(List) ++ "$",
  lists:foldl(fun(X, A) -> re:replace(A, X, ["(?<", X, ">.*)"], [global, {return, list}])
              end, Temp, Description);
%re:replace(Temp, Description, "(?<FOO>.*)", [global, {return, list}]);
normalize([H | T], Des, List) ->
  case (H) of
    $* ->
      normalize(T, Des, [$], H, $[ | List]);
    $# ->
      normalize(T, Des, [$], H, $[ | List]);
    _ ->
      normalize(T, Des, [H | List])
  end.






