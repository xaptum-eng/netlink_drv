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
-module(netlink_drv_sup).


-behaviour(supervisor).

-define(SUPERVISOR, ?MODULE).

%% api
-export([start_link/0]).

%% supervisor callbacks
-export([init/1]).

%%=================================
%% API
%%=================================
start_link() ->
    supervisor:start_link({local, ?SUPERVISOR}, ?MODULE, []).

%%=================================
%% callbacks
%%=================================
init([]) ->
    RestartStrategy = {one_for_one, 4, 3600},
    NetlinkEventManager = #{ id => netlink_event,
                   start => {netlink_event, start_link, []},
		   restart => permanent,
		   shutdown => 2000,
		   type => worker,
		   modules => [netlink_event]},
    NetlinkListener = #{ id => netlink_listener,
                   start => {netlink_listener, start_link, []},
		   restart => permanent,
		   shutdown => 2000,
		   type => worker,
		   modules => [netlink_listener]},
    Children = [NetlinkEventManager, NetlinkListener],
    {ok, {RestartStrategy, Children}}.
