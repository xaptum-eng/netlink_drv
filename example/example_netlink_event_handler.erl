-module(example_netlink_event_handler).

-behaviour(gen_event).
 
-export([
	 init/1, 
	 handle_event/2, 
	 handle_call/2, 
	 handle_info/2, 
	 code_change/3,
	 terminate/2
	]).

-export([
	 add_handler/0, 
	 delete_handler/0
	 ]).

%%==================================================
%% API
%%==================================================
%% Add self as handler
add_handler() ->
    netlink_event:add_handler(?MODULE, []).

delete_handler() ->
    netlink_event:delete_handler(?MODULE, []).


%%==================================================
%% gen_event callbacks
%%==================================================
init([]) ->
    {ok, []}.
 
handle_event(NetlinkEvent, State) ->
    process_netlink_event(NetlinkEvent),
    {ok, State}.
 
handle_call(_, State) ->
    {ok, ok, State}.
 
handle_info(_, State) ->
    {ok, State}.
 
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.
 
terminate(_Reason, _State) ->
    ok.

%%==================================================
%% internal function
%%==================================================
process_netlink_event({deladdr, Interface}) ->
    io:format("Interface ~p: address was removed~n", [Interface]),
    io:format("Detected ip address change. Reconnecting....~n"),
    ok;

process_netlink_event({newaddr, Interface, IpAddress}) ->
    io:format("Interface ~p: new address was assigned: ~p~n", [Interface, IpAddress]),
    io:format("Detected ip address change. Reconnecting....~n"),
    ok;

process_netlink_event({newlink, Interface, down, not_running}) ->
    io:format("New network interface ~p, state: DOWN NOT RUNNING~n", [Interface]),
    ok;

process_netlink_event({newlink,Interface,up,not_running}) ->
    io:format("New network interface ~p, state: UP NOT RUNNING~n", [Interface]),
    ok;

process_netlink_event({newlink,Interface,up,running}) ->
    io:format("New network interface ~p, state: UP RUNNING~n", [Interface]),
    ok;
process_netlink_event(rtchange) ->
    io:format("Routing table information changed~n"),
    ok;

process_netlink_event(Other) ->
    io:format("Unknown event ~p~n", [Other]),
    ok.

