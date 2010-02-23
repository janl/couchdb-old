#!/usr/bin/env escript
%% -*- erlang -*-

% Licensed under the Apache License, Version 2.0 (the "License"); you may not
% use this file except in compliance with the License.  You may obtain a copy of
% the License at
%
%   http://www.apache.org/licenses/LICENSE-2.0
%
% Unless required by applicable law or agreed to in writing, software
% distributed under the License is distributed on an "AS IS" BASIS, WITHOUT
% WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.  See the
% License for the specific language governing permissions and limitations under
% the License.

%% XXX: Figure out how to -include("couch_db.hrl")
-record(doc, {id= <<"">>, revs={0, []}, body={[]},
            attachments=[], deleted=false, meta=[]}).

-record(http_db, {
    url,
    auth = [],
    resource = "",
    headers = [
        {"User-Agent", "CouchDB/"++couch_server:get_version()},
        {"Accept", "application/json"},
        {"Accept-Encoding", "gzip"}
    ],
    qs = [],
    method = get,
    body = nil,
    options = [
        {response_format,binary},
        {inactivity_timeout, 30000}
    ],
    retries = 10,
    pause = 1,
    conn = nil
}).

-record(user_ctx,
    {
    name=null,
    roles=[],
    handler
}).

-define(LATEST_DISK_VERSION, 5).

-record(db_header,
    {disk_version = ?LATEST_DISK_VERSION,
     update_seq = 0,
     unused = 0,
     fulldocinfo_by_id_btree_state = nil,
     docinfo_by_seq_btree_state = nil,
     local_docs_btree_state = nil,
     purge_seq = 0,
     purged_docs = nil,
     security_ptr = nil,
     revs_limit = 1000
}).

-record(db,
    {main_pid = nil,
    update_pid = nil,
    compactor_pid = nil,
    instance_start_time, % number of microsecs since jan 1 1970 as a binary string
    fd,
    fd_ref_counter,
    header = #db_header{},
    committed_update_seq,
    fulldocinfo_by_id_btree,
    docinfo_by_seq_btree,
    local_docs_btree,
    update_seq,
    name,
    filepath,
    validate_doc_funs = [],
    security = [],
    security_ptr = nil,
    user_ctx = #user_ctx{},
    waiting_delayed_commit = nil,
    revs_limit = 1000,
    fsync_options = []
}).

config_files() ->
    lists:map(fun test_util:build_file/1, [
        "etc/couchdb/default_dev.ini",
        "etc/couchdb/local_dev.ini"
    ]).

main(_) ->
    test_util:init_code_path(),
    
    etap:plan(1),
    case (catch test()) of
        ok ->
            etap:end_tests();
        Other ->
            etap:diag(io_lib:format("Test died abnormally: ~p", [Other])),
            etap:bail(Other)
    end,
    ok.

test() ->
    couch_server_sup:start_link(config_files()),
    ibrowse:start(),
    crypto:start(),

    % couch_server:delete(<<"etap-test-db">>, []),
    % {ok, Db1} = couch_db:create(<<"etap-test-db">>, []),
    % test_all(local),
    % couch_db:close(Db1),
    % couch_server:delete(<<"etap-test-db">>, []),

    test_all(remote),
    couch_server:delete(<<"etap-test-db">>, []),

    ok.

test_all(Type) ->
    % db_wrap(fun() -> test_unchanged_db(Type) end),
    % db_wrap(fun() -> test_simple_change(Type) end),
    % db_wrap(fun() -> test_since_parameter(Type) end),
    db_wrap(fun() -> test_continuous_parameter(Type) end),
    db_wrap(fun() -> 
        test_continuous_parameter_with_filter(remote),
        % timer:sleep(1000)
        test_conflicts(Type)
    end),
    % db_wrap(fun() -> test_conflicts(Type) end),
    % db_wrap(fun() -> test_deleted_conflicts(Type) end),
    % db_wrap(fun() -> test_chunk_reassembly(remote) end),
    ok.

db_wrap(F) ->
    couch_server:delete(<<"etap-test-db">>, []),
    {ok, Db} = couch_db:create(<<"etap-test-db">>, []),
    F(),
    couch_db:close(Db),
    couch_server:delete(<<"etap-test-db">>, []).

test_unchanged_db(Type) ->
    {ok, Pid} = start_changes_feed(Type, 0, false),
    etap:is(
        couch_rep_changes_feed:next(Pid),
        complete,
        io_lib:format(
            "(~p) changes feed for unchanged DB is automatically complete",
            [Type])
    ).

test_simple_change(Type) ->
    Expect = generate_change(),
    {ok, Pid} = start_changes_feed(Type, 0, false),
    etap:is(
        {couch_rep_changes_feed:next(Pid), couch_rep_changes_feed:next(Pid)},
        {[Expect], complete},
        io_lib:format("(~p) change one document, get one row", [Type])
    ).

test_since_parameter(Type) ->
    {ok, Pid} = start_changes_feed(Type, get_update_seq(), false), 
    etap:is(
        couch_rep_changes_feed:next(Pid),
        complete,
        io_lib:format(
            "(~p) since query-string parameter allows us to skip changes",
            [Type])
    ).

test_continuous_parameter(Type) ->
    {ok, Pid} = start_changes_feed(Type, get_update_seq(), true),

    % make the changes_feed request before the next update
    Self = self(),
    spawn(fun() -> 
        Change = couch_rep_changes_feed:next(Pid), 
        Self ! {actual, Change}
    end),

    Expect = generate_change(),
    etap:is(
        receive {actual, Actual} -> Actual end,
        [Expect],
        io_lib:format(
            "(~p) feed=continuous query-string parameter picks up new changes",
            [Type])
    ),

    ok = couch_rep_changes_feed:stop(Pid).

test_continuous_parameter_with_filter(Type) when Type =:= remote ->
    generate_change(<<"_design/filter">>, {[
        {<<"filters">>, {[
            {<<"test">>,<<"function(doc) {return doc.type && doc.type == \"show\";}">>}
            % {<<"test">>,<<"function(doc, req) {return req && req.query && req.query.type && doc[req.query.type] && doc[req.query.type] == \"show\";}">>}
        ]}}
    ]}),
    {ok, Pid} = start_changes_feed(Type, get_update_seq(), true, "filter/test"),

    % make the changes_feed request before the next update
    Self = self(),
    spawn(fun() -> 
        Change = couch_rep_changes_feed:next(Pid), 
        Self ! {actual, Change}
    end),

    % Hide = generate_change(<<"hide">>, {[{<<"type">>, <<"hide">>}]}),
    Show = generate_change(<<"show">>, {[{<<"type">>, <<"show">>}]}),
    etap:is(
        receive {actual, Actual} -> Actual end,
        [Show],
        io_lib:format(
            "(~p) feed=continuous query-string parameter picks up filtered changes",
            [Type])
    ),
    ok = couch_rep_changes_feed:stop(Pid).

test_conflicts(Type) ->
    Since = get_update_seq(),
    Expect = generate_conflict(),
    {ok, Pid} = start_changes_feed(Type, Since, false),
    etap:is(
        {couch_rep_changes_feed:next(Pid), couch_rep_changes_feed:next(Pid)},
        {[Expect], complete},
        io_lib:format("(~p) conflict revisions show up in feed", [Type])
    ).

test_deleted_conflicts(Type) ->
    Since = get_update_seq(),
    {ExpectProps} = generate_conflict(),

    %% delete the conflict revision
    Id = proplists:get_value(<<"id">>, ExpectProps),
    [Win, {[{<<"rev">>, Lose}]}] = proplists:get_value(<<"changes">>, ExpectProps),
    Doc = couch_doc:from_json_obj({[
        {<<"_id">>, Id},
        {<<"_rev">>, couch_doc:rev_to_str(Lose)},
        {<<"_deleted">>, true}
    ]}),
    Db = get_db(),
    {ok, Rev} = couch_db:update_doc(Db, Doc, [full_commit]),
    couch_db:close(Db),

    Expect = {[
        {<<"seq">>, get_update_seq()},
        {<<"id">>, Id},
        {<<"changes">>, [Win, {[{<<"rev">>, Rev}]}]}
    ]},
    
    {ok, Pid} = start_changes_feed(Type, Since, false),
    etap:is(
        {couch_rep_changes_feed:next(Pid), couch_rep_changes_feed:next(Pid)},
        {[Expect], complete},
        io_lib:format("(~p) deleted conflict revisions show up in feed", [Type])
    ).

test_chunk_reassembly(Type) ->
    Since = get_update_seq(),
    Expect = [generate_change() || _I <- lists:seq(1,30)],
    {ok, Pid} = start_changes_feed(Type, Since, false),
    etap:is(
        get_all_changes(Pid, []),
        Expect,
        io_lib:format("(~p) reassembles chunks split across TCP frames",
            [Type])
    ).

get_all_changes(Pid, Acc) ->
    case couch_rep_changes_feed:next(Pid) of
    complete ->
        lists:flatten(lists:reverse(Acc));
    Else ->
        get_all_changes(Pid, [Else|Acc])
    end.

generate_change() ->
    generate_change(couch_uuids:random()).

generate_change(Id) ->
    generate_change(Id, {[]}).

generate_change(Id, EJson) ->
    Doc = couch_doc:from_json_obj(EJson),
    Db = get_db(),
    {ok, Rev} = couch_db:update_doc(Db, Doc#doc{id = Id}, [full_commit]),
    couch_db:close(Db),
    {[
        {<<"seq">>, get_update_seq()},
        {<<"id">>, Id},
        {<<"changes">>, [{[{<<"rev">>, Rev}]}]}
    ]}.

generate_conflict() ->
    Id = couch_uuids:random(),
    Db = get_db(),
    Doc1 = (couch_doc:from_json_obj({[<<"foo">>, <<"baa">>]}))#doc{id = Id},
    Doc2 = (couch_doc:from_json_obj({[<<"foo">>, <<"baz">>]}))#doc{id = Id},
    {ok, Rev1} = couch_db:update_doc(Db, Doc1, [full_commit]),
    {ok, Rev2} = couch_db:update_doc(Db, Doc2, [full_commit, all_or_nothing]),
    
    %% relies on undocumented CouchDB conflict winner algo and revision sorting!
    RevList = [{[{<<"rev">>, R}]} || R
        <- lists:sort(fun(A,B) -> B<A end, [Rev1,Rev2])],
    {[
        {<<"seq">>, get_update_seq()},
        {<<"id">>, Id},
        {<<"changes">>, RevList}
    ]}.
    
get_db() ->
    {ok, Db} = couch_db:open(<<"etap-test-db">>, []),
    % needs admin role for writing _design docs
    Db#db{user_ctx=#user_ctx{roles=[<<"_admin">>]}}.

get_dbname(local) ->
    "etap-test-db";
get_dbname(remote) ->
    "http://127.0.0.1:5984/etap-test-db/".

get_update_seq() ->
    Db = get_db(),
    Seq = couch_db:get_update_seq(Db),
    couch_db:close(Db),
    Seq.

start_changes_feed(local, Since, Continuous) ->
    Props = [{<<"continuous">>, Continuous}],
    couch_rep_changes_feed:start_link(self(), get_db(), Since, Props);
start_changes_feed(remote, Since, Continuous) ->
    Props = [{<<"continuous">>, Continuous}],
    Db = #http_db{url = get_dbname(remote)},
    couch_rep_changes_feed:start_link(self(), Db, Since, Props).

start_changes_feed(local, Since, Continuous, Filter) ->
    Props = [{<<"continuous">>, Continuous},{<<"filter">>, Filter},{<<"type">>,<<"type">>}],
    couch_rep_changes_feed:start_link(self(), get_db(), Since, Props);
start_changes_feed(remote, Since, Continuous, Filter) ->
    Props = [{<<"continuous">>, Continuous},{<<"filter">>, Filter},{<<"type">>,<<"type">>}],
    Db = #http_db{url = get_dbname(remote)},
    couch_rep_changes_feed:start_link(self(), Db, Since, Props).

