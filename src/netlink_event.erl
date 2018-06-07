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
-module(netlink_event).

-export([
	 start_link/0,
	 add_handler/2,
	 delete_handler/2,
	 notify/1
]).

-define(EVENT_MANAGER, ?MODULE).

%%====================================================================
%% API functions
%%====================================================================
start_link() ->
    gen_event:start_link({local, ?EVENT_MANAGER}).

add_handler(Handler, Args) ->
    gen_event:add_handler(?EVENT_MANAGER, Handler, Args).

delete_handler(Handler, Args) ->
    gen_event:delete_handler(?EVENT_MANAGER, Handler, Args).

notify(Event) ->
    gen_event:notify(?EVENT_MANAGER, Event).
%%====================================================================
%% Internal functions
%%====================================================================
