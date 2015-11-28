-module(rc).
-include("rc.hrl").
-include_lib("riak_core/include/riak_core_vnode.hrl").

-export([
	new_instance/1,
	register_script/2,
	action/2,
	get_current/0,

	ping/0,
	addnew/2,
	button/2,
	get_state/1,
	lookup/1
	]).

-ignore_xref([
	new_instance/1,
	register_script/2,
	action/2,
	get_current/0,

	ping/0,
	addnew/2,
	button/2,
	get_state/1,
	lookup/1
	]).

%% Public API

%% @doc Pings a random vnode to make sure communication is functional

%% add new instance.
%% returns name
new_instance(Type) -> "NAME_OF_CHAR".

%% add or update lua script with ai type name.
%% returns ok or ng
%% use luerl:load
register_script(Type, Script) ->ng.

%% send event to instance (event is processed by Lua script. rc doesn't care event content.)
%% returns ok or ng
action(Name, Action) ->ng.

%% receive new event from outflow tank.
%% returns list of instances
get_current() ->[].

%% receive new event from outflow tank.
%% returns ok or ng
start_watch(Dest) ->ng.
stop_watch(Dest) ->ng.


%% prepare blackboard
start_blackboard(BBName) -> ng.
clear_blackboard(BBName) -> ng.
get_blackboard(BBName) -> ng.

ping() ->
    DocIdx = riak_core_util:chash_key({<<"ping">>, term_to_binary(now())}),
    PrefList = riak_core_apl:get_primary_apl(DocIdx, 1, rc),
    [{IndexNode, _Type}] = PrefList,
    riak_core_vnode_master:sync_spawn_command(IndexNode, ping, rc_vnode_master).

addnew(Name, Code) ->
    DocIdx = riak_core_util:chash_key({<<"character">>, list_to_binary(Name)}),
    PrefList = riak_core_apl:get_apl(DocIdx, ?N, rc),
	riak_core_vnode_master:command(PrefList, {addnew, Name, Code}, rc_vnode_master).

speak_to(Name, Sender, Message) ->
    DocIdx = riak_core_util:chash_key({<<"character">>, list_to_binary(Name)}),
    PrefList = riak_core_apl:get_apl(DocIdx, ?N, rc),
	riak_core_vnode_master:command(PrefList, {speak_to, Name, Sender, Message}, rc_vnode_master).

attack_to(Name, Sender, Param) ->
    DocIdx = riak_core_util:chash_key({<<"character">>, list_to_binary(Name)}),
    PrefList = riak_core_apl:get_apl(DocIdx, ?N, rc),
	riak_core_vnode_master:command(PrefList, {attack_to, Name, Sender, Param}, rc_vnode_master).

button(Name, Button) ->
    DocIdx = riak_core_util:chash_key({<<"character">>, list_to_binary(Name)}),
    PrefList = riak_core_apl:get_primary_apl(DocIdx, 1, rc),
    [{IndexNode, _Type}] = PrefList,
	riak_core_vnode_master:sync_command(IndexNode, {button, Name, Button}, rc_vnode_master).

get_state(Name) ->
    DocIdx = riak_core_util:chash_key({<<"character">>, list_to_binary(Name)}),
    PrefList = riak_core_apl:get_primary_apl(DocIdx, 1, rc),
    [{IndexNode, _Type}] = PrefList,
	riak_core_vnode_master:sync_command(IndexNode, {get_state, Name}, rc_vnode_master).

lookup(Name) ->
    DocIdx = riak_core_util:chash_key({<<"character">>, list_to_binary(Name)}),
    PrefList = riak_core_apl:get_primary_apl(DocIdx, 1, rc),
    [{IndexNode, _Type}] = PrefList,
	riak_core_vnode_master:sync_command(IndexNode, {lookup, Name}, rc_vnode_master).
