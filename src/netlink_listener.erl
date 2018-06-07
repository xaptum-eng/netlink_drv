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
-module(netlink_listener).

-behaviour(gen_server).

%% export API
-export([start_link/0,
	 stop/0
	]).

%% export gen_server callbacks
-export([init/1,
	 handle_call/3,
	 handle_cast/2,
	 handle_info/2,
	 terminate/2,
	 code_change/3]).

-record(state, {port}).

-define(SERVER_NAME, ?MODULE).

-define(NLDRV, "nldrv").
-define(POLL_INTERVAL, 250).

%%====================================
%% API
%%====================================
start_link() ->
    gen_server:start_link({local, ?SERVER_NAME}, ?MODULE, [], []).

stop() ->
    gen_server:cast(?SERVER_NAME, stop).

%%====================================
%% callbacks
%%====================================
init([]) ->
    self() ! start_netlink_monitor,
    {ok, #state{}}.

terminate(_Reason, #state{port = Port} = _State) ->
    catch erlang:port_close(Port),
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

handle_info(start_netlink_monitor, State) ->
    PrivDir = netlink_drv:priv_dir(),
    case erl_ddll:load(PrivDir, ?NLDRV) of
	ok -> ok;
	Other -> exit(Other)
    end,
    Port = erlang:open_port({spawn, ?NLDRV}, [binary]),

    %% Start poll
    poll_netlink(),

    {noreply, State#state{port = Port}};

handle_info({Port, {data, Data}}, #state{port = Port} = State) ->
    Term = erlang:binary_to_term(Data),
    ok = handle_port_data(Term),
    {noreply, State};

handle_info({'EXIT', Port, _Reason}, #state{port = Port} = State) ->
    self() ! start_netlink_monitor,
    {noreply, State#state{port = undefined}};

handle_info({Port, {exit_status, _Status}}, #state{port = Port} = State) ->
    self() ! start_netlink_monitor,
    {noreply, State#state{port = undefined}};

handle_info(poll_netlink, #state{port = Port} = State) ->
    ok = send_port_command(Port),
    poll_netlink(),
    {noreply, State};

handle_info(_Msg, State) ->
    {noreply, State}.


handle_cast(stop, State) ->
    {stop, normal, State};
handle_cast(_Msg, State) ->
    {noreply, State}.

handle_call(_Msg, _From, State) ->
    Reply = ok,
    {reply, Reply, State}.

%%====================================
%% internal functions
%%====================================
handle_port_data({error, _Reason}) ->
    ok;

handle_port_data(Data) ->
    netlink_event:notify(Data),
    ok.

send_port_command(undefined) ->
    ok;
send_port_command(Port) ->
    Port ! {self(), {command, term_to_binary(1)}},
    ok.

poll_netlink() ->
    erlang:send_after(?POLL_INTERVAL, self(), poll_netlink).
