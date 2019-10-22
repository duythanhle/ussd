%%%-------------------------------------------------------------------
%%% @author thanhld19
%%% @copyright (C) 2019, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 15. Oct 2019 2:24 PM
%%%-------------------------------------------------------------------
-author("thanhld19").
% CW - Call Waiting
% Activate  *43#
% Query     *#43#
% Cancel    #43#

% CFU - Call Forward Unconditional
% Activate  *21*TN# with TN: Forwarded Telephone Number
% Query     *#21#
% Cancel    #21#

% CFB - Call Forward Busy
% Activate  *67*TN# with TN: Forwarded Telephone Number
% Query     *#67#
% Cancel    #67#

% CFNR - Call Forward No-Reply
% Activate  *61*TN# with TN: Forwarded Telephone Number
% Query     *#61#
% Cancel    #61#

% CFNRc - Call Forward on Not Reachable
% Activate  *62*TN# with TN: Forwarded Telephone Number
% Query     *#62#
% Cancel    #62#

% CFNL - Call Forward on Not Logged In
% Activate  *63*TN# with TN: Forwarded Telephone Number
% Query     *#63#
% Cancel    #63#

% 3PTY - 3 Party Call
% Activate  *95#
% Query     *#95#
% Cancel    #95#

% CONF - Conference Call
% Activate  *95#
% Query     *#95#
% Cancel    #95#
-define(UssdData, #{
  "43"=>cw,
  "21"=>cfu,
  "67"=>cfn,
  "61"=>cfnr,
  "62"=>cfnrc,
  "63"=>cfnl,
  "95"=>conf_3pty}).
-define(UssdDataWithTelephone, #{
  "21"=>cfu,
  "67"=>cfn,
  "61"=>cfnr,
  "62"=>cfnrc,
  "63"=>cfnl}).
-define(UssdDataWithoutTelephone, #{
  "43"=>cw,
  "95"=>conf_3pty}).

-record(service_info,{
  name,
  activate,
  query,
  cancel,
  use,
  description,
  param
}).
-record(param_info,{
  activate,
  query,
  cancel,
  use
}).

-record(rule, {
  default = [],
  value = []
}).

-record(services, {
  default = [],
  value = []
}).

-record(service, {
  key,
  info
}).

-record(key, {
  value
}).


-record(params, {
  default = [],
  value = []
}).
-record(param, {
  key,
  info
}).

