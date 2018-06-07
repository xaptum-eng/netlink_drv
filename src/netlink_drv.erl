%%-------------------------------------------------------------------------------------------
%% 
%% XAPTUM CONFIDENTIAL
%% __________________
%% 
%%  2017(C) Xaptum, Inc.
%%  All Rights Reserved.Patents Pending.
%% 
%% NOTICE:  All information contained herein is, and remains
%% the property of Xaptum, Inc.  The intellectual and technical concepts contained
%% herein are proprietary to Xaptum, Inc and may be covered by U.S. and Foreign Patents,
%% patents in process, and are protected by trade secret or copyright law.
%% Dissemination of this information or reproduction of this material
%% is strictly forbidden unless prior written permission is obtained
%% from Xaptum, Inc.
%%
%% @author Venkatakumar Srinivasan
%%
%%-------------------------------------------------------------------------------------------
-module(netlink_drv).

-behaviour(application).

%% API exports
-export([
	 start/2,
	 stop/1
	]).

-export([
	 priv_dir/0
]).

%%====================================================================
%% API functions
%%====================================================================
start(_StartType, _StartArgs) ->
    netlink_drv_sup:start_link().

stop(_State) ->
    ok.

priv_dir() ->
    case code:priv_dir(?MODULE) of
	{error, bad_name} ->
	    "./priv";
	PrivDir -> filename:absname(PrivDir)
    end.

%%====================================================================
%% Internal functions
%%====================================================================
