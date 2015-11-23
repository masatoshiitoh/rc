-module(rc_vnode).
-behaviour(riak_core_vnode).
-include("rc.hrl").

-export([start_vnode/1,
         init/1,
         terminate/2,
         handle_command/3,
         is_empty/1,
         delete/1,
         handle_handoff_command/3,
         handoff_starting/2,
         handoff_cancelled/1,
         handoff_finished/2,
         handle_handoff_data/2,
         encode_handoff_item/2,
         handle_coverage/4,
         handle_exit/3]).

-ignore_xref([
             start_vnode/1
             ]).

-record(state, {partition, pids}).

%% API
start_vnode(I) ->
    riak_core_vnode_master:get_vnode_pid(I, ?MODULE).

init([Partition]) ->
    {ok, #state { partition=Partition, pids=dict:new() }}.

%% Sample command: respond to a ping
handle_command(ping, _Sender, State) ->
    ?PRINT({ping, State}),
    {reply, {pong, State#state.partition}, State};

%% Name is new comer.
handle_command({addnew, Name, Code}, _Sender, State) ->
	{ok, Pid} = rc_persona:start_link(Code),
	NewPids = dict:store(Name, Pid, State#state.pids),
    NewState = State#state{pids = NewPids},
    ?PRINT({addnew, Name, Code}),
    {reply, {{addnew, Pid}, State#state.partition}, NewState};

%% Find pid associated to Name
handle_command({button, Name, Button}, _Sender, State) ->
	Pid = dict:fetch(Name, State#state.pids),
	Result = rc_persona:button(Pid, Button),
    {reply, {{button, Pid}, Result, State#state.partition}, State};

%% Find pid associated to Name
handle_command({get_state, Name}, _Sender, State) ->
	Pid = dict:fetch(Name, State#state.pids),
    {reply, {{get_state, Pid}, State#state.partition}, State};

%% Find pid associated to Name
handle_command({lookup, Name}, _Sender, State) ->
	Pid = dict:fetch(Name, State#state.pids),
    {reply, {{lookup, Pid}, State#state.partition}, State};

handle_command(Message, _Sender, State) ->
    ?PRINT({unhandled_command, Message}),
    {noreply, State}.

handle_handoff_command(_Message, _Sender, State) ->
    ?PRINT({handoff_command, _Message}),
    {noreply, State}.

handoff_starting(_TargetNode, State) ->
    ?PRINT({handoff_starting, _TargetNode}),
    {true, State}.

handoff_cancelled(State) ->
    ?PRINT({handoff_cancelled, none}),
    {ok, State}.

handoff_finished(_TargetNode, State) ->
    ?PRINT({handoff_finished, _TargetNode}),
    {ok, State}.

handle_handoff_data(_Data, State) ->
    ?PRINT({handoff_data, _Data}),
    {reply, ok, State}.

encode_handoff_item(_ObjectName, _ObjectValue) ->
    <<>>.

is_empty(State) ->
    {true, State}.

delete(State) ->
    {ok, State}.

handle_coverage(_Req, _KeySpaces, _Sender, State) ->
    {stop, not_implemented, State}.

handle_exit(_Pid, _Reason, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.
