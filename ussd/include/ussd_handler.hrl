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

  MapsServices = lists:foldl(fun(X, MapTemp) -> maps:put(creat_key(X#service.key),creat_service_info_record(X#service.info), MapTemp)
                   end, #{} ,Services),
  MapsParams = lists:foldl(fun(X, MapTemp) -> maps:put(creat_key(X#param.key),creat_param_info_record(X#param.info), MapTemp)
                   end, #{} ,Params),
  io:format("MapSv: ~n~p~n", [MapsServices]),
  io:format("MapPr: ~n~p~n", [MapsParams]).

creat_service_info_record(List) ->
  lists:foldl(fun(X, Record) ->
                          case X of
                            {name, [], [Value]} -> Record#service_info{name = Value};
                            {activate, [], [Value]} -> Record#service_info{activate = Value};
                            {query, [], [Value]} -> Record#service_info{query = Value};
                            {cancel, [], [Value]} -> Record#service_info{cancel = Value};
                            {use, [], [Value]} -> Record#service_info{use = Value};
                            {description, [], [Value]} -> Record#service_info{description = string:tokens(Value, " ")};
                            {param, [], [Value]} -> Record#service_info{param = Value};
                            _Other -> _Other
                          end
                end, #service_info{}, List).

creat_param_info_record(List) ->
  lists:foldl(fun(X, Record) ->
                          case X of
                            {activate, [], [Value]} -> Record#param_info{activate = Value};
                            {query, [], [Value]} -> Record#param_info{query = Value};
                            {cancel, [], [Value]} -> Record#param_info{cancel = Value};
                            {use, [], [Value]} -> Record#param_info{use = Value};
                            _Other -> _Other
                          end
              end, #param_info{}, List).

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
%%
%%
get_service(Key, MapsService) ->
  maps:get(Key, MapsService, invalid).

handle_ussd(Str, MapsServices, MapsParams) ->
  case check_key(Str) of
    invalid ->
      invalid;
    Key ->
      case maps:get(Key, MapsServices, invalid) of
        invalid ->
          invalid;
        RecordService ->
          case maps:get(RecordService#service_info.param, MapsParams, invalid) of
            invalid ->
              invalid;
            Param ->
              handle(Str, RecordService, Param)

          end
      end
  end.

handle(Str, #service_info{name=Name,activate = Activate, query = Query, cancel = Cancel, use = Use, description = Description},
    #param_info{activate = ActivateParam, query = QueryParam, cancel = CancelParam, use = UseParam}) ->
  case Str of
    Activate ->
      {list_to_atom(Name), list_to_atom(ActivateParam), []};
    Query ->
      {list_to_atom(Name), list_to_atom(QueryParam), []};
    Cancel ->
      {list_to_atom(Name), list_to_atom(CancelParam), []};
    Use ->
      {list_to_atom(Name), list_to_atom(UseParam), []};
    _ ->
      case Description of
        undefined ->
          invalid;
        _ ->
          case re:run(Str, nomalize(Activate, Description), [{capture, Description, list}]) of
            {match, ReturnParam} ->
              {list_to_atom(Name), list_to_atom(ActivateParam), ReturnParam};
            _ ->
              invalid
          end
      end
  end.

nomalize(Activate, Description) ->
  nomalize(Activate, Description, []).

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

test1() ->
  test_check_key(),
  test_load_config().

test_check_key() ->
  ?assertEqual("1", check_key("*#1#")),
  ?assertEqual("1", check_key("a1#")),
  ?assertEqual("1", check_key("a1aaaa")),
  ?assertEqual("123", check_key("*#@123*####")),
  ?assertEqual("123", check_key("*123*1#1##")),
  ?assertEqual(invalid, check_key("123*####")),
  ?assertEqual(invalid, check_key("****123")),
  ?assertEqual(invalid, check_key("123")).

test_load_config() ->
  {Element, _} = xmerl_scan:file("config.xml", [{space, normalize}]),
  [Clean] = xmerl_lib:remove_whitespace([Element]),
  A = xmerl_lib:simplify_element(Clean),
  Rules = A#rule.value,
  [Services] = [X#services.value || X<-Rules, is_record(X, services)],
  [Params] = [X#params.value || X<-Rules, is_record(X, params)],

%%  ?assertEqual([{service, [{key,"43"}],
%%                          [{name,[],["cw"]},
%%                          {activate,[],["*43#"]},
%%                          {query,[],["*#43#"]},
%%                          {cancel,[],["#43#"]},
%%                          {param,[],["ac_qr_deac"]}]},
%%                {service, [{key,"21"}],
%%                          [{name,[],["cfu"]},
%%                          {activate,[],["*21*TN#"]},
%%                          {query,[],["*#21#"]},
%%                          {cancel,[],["#21#"]},
%%                          {description,[],["TN"]},
%%                          {param,[],["ac_qr_deac"]}]},
%%                {service, [{key,"67"}],
%%                          [{name,[],["cfb"]},
%%                          {activate,[],["*67*TN#"]},
%%                          {query,[],["*#67#"]},
%%                          {cancel,[],["#67#"]},
%%                          {description,[],["TN"]},
%%                          {param,[],["ac_qr_deac"]}]},
%%                {service, [{key,"61"}],
%%                          [{name,[],["cfnr"]},
%%                          {activate,[],["*61*TN#"]},
%%                          {query,[],["*#61#"]},
%%                          {cancel,[],["#61#"]},
%%                          {description,[],["TN"]},
%%                          {param,[],["ac_qr_deac"]}]},
%%                {service, [{key,"62"}],
%%                          [{name,[],["cfnrc"]},
%%                          {activate,[],["*62*TN#"]},
%%                          {query,[],["*#62#"]},
%%                          {cancel,[],["#62#"]},
%%                          {description,[],["TN"]},
%%                          {param,[],["ac_qr_deac"]}]},
%%                {service, [{key,"63"}],
%%                          [{name,[],["cfnl"]},
%%                          {activate,[],["*63*TN#"]},
%%                          {query,[],["*#63#"]},
%%                          {cancel,[],["#63#"]},
%%                          {description,[],["TN"]},
%%                          {param,[],["ac_qr_deac"]}]},
%%                {service, [{key,"95"}],
%%                          [{name,[],["conf3pty"]},
%%                          {activate,[],["*95*TN#"]},
%%                          {query,[],["*#95#"]},
%%                          {cancel,[],["#95#"]},
%%                          {param,[],["ac_qr_deac"]}]}],
%%    Services),
  MapsServices = lists:foldl(fun(X, MapTemp) -> maps:put(creat_key(X#service.key),creat_service_info_record(X#service.info), MapTemp)
                    end, #{} ,Services),
%%  ?assertEqual(#{ "21" => {service_info,"cfu","*21*TN#","*#21#","#21#",undefined,["TN"],"ac_qr_deac"},
%%                  "43" => {service_info,"cw","*43#","*#43#","#43#",undefined,undefined,"ac_qr_deac"},
%%                  "61" => {service_info,"cfnr","*61*TN#","*#61#","#61#",undefined,["TN"],"ac_qr_deac"},
%%                  "62" => {service_info,"cfnrc","*62*TN#","*#62#","#62#",undefined,["TN"],"ac_qr_deac"},
%%                  "63" => {service_info,"cfnl","*63*TN#","*#63#","#63#",undefined,["TN"],"ac_qr_deac"},
%%                  "67" => {service_info,"cfb","*67*TN#","*#67#","#67#",undefined,["TN"],"ac_qr_deac"},
%%                  "95" => {service_info,"conf3pty","*95*TN#","*#95#","#95#",undefined,undefined,"ac_qr_deac"}},
%%    MapsServices),
  MapsParams = lists:foldl(fun(X, MapTemp) -> maps:put(creat_key(X#param.key),creat_param_info_record(X#param.info), MapTemp)
                           end, #{} ,Params),
%%  ?assertEqual(#{"ac_qr_deac" => {param_info,"active","query","deactive",undefined}},
%%    MapsParams),

  ?assertEqual({service_info,"cw","*43#","*#43#","#43#",undefined,undefined,"ac_qr_deac"},
    get_service(check_key("*43#"), MapsServices)),

  ?assertEqual(invalid,
    get_service(check_key("*33#"), MapsServices)),

  ?assertEqual({cw, query, []}, handle_ussd("*#43#", MapsServices, MapsParams)),
  ?assertEqual({cw, deactive, []}, handle_ussd("#43#", MapsServices, MapsParams)),
  ?assertEqual({cw, active, []}, handle_ussd("*43#", MapsServices, MapsParams)),


  ?assertEqual("[*]11[*](?<TN>.*)", nomalize("*11*TN",["TN"])),
  ?assertEqual("[*]11[*](?<TN>.*)[#]", nomalize("*11*TN#",["TN"])),
  ?assertEqual("[*]11[*](?<TN>.*)[*](?<MN>.*)", nomalize("*11*TN*MN",["TN", "MN"])),

  ?assertEqual({cfu, active, ["010102"]}, handle_ussd("*21*010102#", MapsServices, MapsParams)),
  ?assertEqual({cfu, active, ["*010102"]}, handle_ussd("*21**010102#", MapsServices, MapsParams)),
  ?assertEqual({cfu, active, ["*010102#"]}, handle_ussd("*21**010102##", MapsServices, MapsParams)),
  ?assertEqual(invalid, handle_ussd("*33**010102##", MapsServices, MapsParams)),
  ?assertEqual({test, a, ["*010102", "33#"]}, handle_ussd("*99**010102*33##", MapsServices, MapsParams)).