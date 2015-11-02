-module(rc).
-include("rc.hrl").
-include_lib("riak_core/include/riak_core_vnode.hrl").

-export([
	ping/0,
	addnew/2,
	button/2,
	get_state/1,
	lookup/1
        ]).

-ignore_xref([
	ping/0,
	addnew/2,
	button/2,
	get_state/1,
	lookup/1
	]).

%% Public API

%% @doc Pings a random vnode to make sure communication is functional

ping() ->
    DocIdx = riak_core_util:chash_key({<<"ping">>, term_to_binary(now())}),
    PrefList = riak_core_apl:get_primary_apl(DocIdx, 1, rc),
    [{IndexNode, _Type}] = PrefList,
    riak_core_vnode_master:sync_spawn_command(IndexNode, ping, rc_vnode_master).

addnew(Name, Code) ->
    DocIdx = riak_core_util:chash_key({<<"character">>, list_to_binary(Name)}),
    PrefList = riak_core_apl:get_apl(DocIdx, ?N, rc),
	riak_core_vnode_master:command(PrefList, {addnew, Name, Code}, rc_vnode_master).

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
