-module(vodka_tests).
-export([set_field/3]).

-compile({parse_transform, vodka}).
-compile(export_all).


-record(rec, {f1 = 1, f2 = 2}).


set_field(K, V, A) ->
    A#rec{K = V}.

set_fields(K1, V1, K2, V2, A) ->
    A#rec{K1 = V1, K2 = V2}.

update_field(K1, V1, V2, A) ->
    A#rec{K1 = V1, f2 = V2}.

-include_lib("eunit/include/eunit.hrl").

-ifdef(TEST).

set_field_test_() ->
    [?_assertEqual(set_field(f1, 3, #rec{}), #rec{f1 = 3})
    ,?_assertError({record_field_not_found,f666}, set_field(f666, 3, #rec{}))].

set_fields_test_() ->
    [?_assertEqual(set_fields(f1, 3, f2, 4, #rec{}), #rec{f1 = 3, f2 = 4})].

update_fields_test_() ->
    [?_assertEqual(update_field(f1, 3, 4, #rec{}), #rec{f1 = 3, f2 = 4})].

-endif.
