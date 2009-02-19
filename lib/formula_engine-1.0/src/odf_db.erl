%%% @doc Implementation of OpenFormula spec's Database datatype.
%%% @author HV <hasan@hypernumbers.com>

%%% === Conceptual Level ===
%%%
%%% Database is a set of Fields plus a set of Records.
%%% The set of fields must contain at least one element.
%%% The set of records may be empty.
%%% A record contains data items for every field.
%%%
%%% Criteria is a set of Criteriasets.
%%% Criteriaset is a list of Requirements, all of which must be satisfied
%%% for a record to match a criteriaset.
%%%
%%% It's enough to match one criteriaset from criteria for a record to be
%%% selected from a database.
%%%
%%% Requirements are specified by users with Conditions.
%%% A condition may be: number or boolean, comparator, or a text value.

%%% === Implementation Level ===
%%%
%%% Records are represented by lists of 2-tuples, where the first element
%%% is a hash of a field and the second element is the data item.
%%%
%%% A map of fields to hashes is kept it the database structure.
%%%
%%% Criteriasets are represented by funs, which take a record and return
%%% true if all requirements are satisfied.
%%% Each requirement is a fun which takes a record and checks that the data
%%% for some field in the record satisfies the condition.

%%% TODO: Error/sanity checking.
%%% TODO: EUnit tests.
%%% TODO: Computed criteria, nicely.

-module(odf_db).

-export([db_from_range/1, criteria_from_range/1, select/2, db_field/2]).
-include("handy_macros.hrl").

-record(odf_db, {
          fm   = [], % field map: original -> internal
          recs = []  % records
         }).

-define(hash(X),
        X). % for tests
        %%erlang:md5(X)). % for production.

%%% @doc Create a database from a range.

db_from_range({range, [Fields|DataRows]}) ->
    FieldsHash = map(fun(X) -> ?hash(X) end, Fields),
    #odf_db{fm   = zip(Fields, FieldsHash),
            recs = create_records(FieldsHash, DataRows)}.

%%% @doc Create criteria from a range.

criteria_from_range({range, [Fields|Csds]}) ->
    FsHash = map(fun(X) -> ?hash(X) end, Fields),
    compile_criteriaset_descriptions(FsHash, Csds, []).

%%% @doc Return database that contains only those records that satisfy
%%%      specified criteria.

select(#odf_db{fm = Fm, recs = Recs}, Criteriaset) ->
    #odf_db{fm = Fm,
            recs = select(Recs, Criteriaset, [])}.
select([], _Criteriaset, Acc) ->
    Acc;
select([Record|T], Criteriaset, Acc) ->
    case any_criteria_matches(Criteriaset, Record) of
        true  -> select(T, Criteriaset, [Record|Acc]);
        false -> select(T, Criteriaset, Acc)
    end.

%%% @doc Returns data for a field from all records in a database in a list.

db_field(Field, #odf_db{fm = Fm, recs = Recs}) ->
    case keysearch(?hash(Field), 2, Fm) of
        {value, {Field, InternalField}} ->
            map(fun(Rec) -> record_field(InternalField, Rec) end, Recs);
        false ->
            no_such_field %% TODO: hmmmm......
    end.

%%% private ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

%%% @doc Return list of records given a list of fields and a list
%%%      of lists with data.

create_records(Fields, DataRows) ->
    create_records(Fields, DataRows, []).
create_records(_Fields, [], Acc) ->
    reverse(Acc);
create_records(Fields, [Data|T], Acc) ->
    create_records(Fields, T, [zip(Fields, Data)|Acc]).

%%% @doc Return a list of criteriasets. Each criteriaset is a fun that
%%%      takes a db record and returns true if it matches, or false if
%%%      it doesn't.

compile_criteriaset_descriptions(_FsHash, [], Acc) ->
    Acc;
compile_criteriaset_descriptions(FsHash, [Csd|T], Acc) ->
    Reqs = compile_to_requirements(FsHash, Csd),
    Criteriaset = fun(Record) ->
                          all(fun(Req) -> Req(Record) end, Reqs)
                  end,
    compile_criteriaset_descriptions(FsHash, T, [Criteriaset|Acc]).

%%% @doc Compiles a criteriaset description to a criteriaset.

compile_to_requirements(FsHash, Csd) ->
    map(fun compile_to_requirement/1, zip(FsHash, Csd)).

%%% @doc Returns a fun that given a db record will return true if field F
%%%      in the record satisfies the requirement.

compile_to_requirement({_Field, blank}) ->
    fun(_Record) -> true end;
compile_to_requirement({Field, ReqDesc}) ->
    Selector = odf_comparator:create(ReqDesc),
    fun(Record) -> Selector(record_field(Field, Record)) end.

any_criteria_matches(Criteriaset, Record) ->
    any(fun(Criteria) -> matches(Criteria, Record) end, Criteriaset).

%%% @doc Check if a record matches some criteria.

matches(Criteria, Record) ->
    Criteria(Record).

%%% @doc Return data for some field from a record.

record_field(Field, Record) ->
    case keysearch(Field, 1, Record) of
        {value, {_Tag, Value}} -> Value;
        false                   -> not_found % TODO: hmm... ?ERR_DIV?
    end.
