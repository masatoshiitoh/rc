-module(rc).
-include("rc.hrl").
-include_lib("riak_core/include/riak_core_vnode.hrl").

-export([
	get_n/0,
	ping/0,
	lookup/1,
	addnew/1
        ]).

-ignore_xref([
	get_n/0,
	ping/0,
	lookup/1,
	addnew/1
	]).

%% Public API

%% @doc Pings a random vnode to make sure communication is functional
get_n() ->
    riak_core_apl:active_owners(rc).

ping() ->
    DocIdx = riak_core_util:chash_key({<<"ping">>, term_to_binary(now())}),
    PrefList = riak_core_apl:get_primary_apl(DocIdx, 1, rc),
    [{IndexNode, _Type}] = PrefList,
    riak_core_vnode_master:sync_spawn_command(IndexNode, ping, rc_vnode_master).

addnew(Name) ->
    DocIdx = riak_core_util:chash_key({<<"character">>, list_to_binary(Name)}),
    PrefList = riak_core_apl:get_apl(DocIdx, ?N, rc),
	riak_core_vnode_master:command(PrefList, {addnew, Name}, rc_vnode_master).

lookup(Name) ->
    DocIdx = riak_core_util:chash_key({<<"character">>, list_to_binary(Name)}),
    PrefList = riak_core_apl:get_apl(DocIdx, ?N, rc),
	riak_core_vnode_master:command(PrefList, {lookup, Name}, rc_vnode_master).
