-module(vodka_tests).
%-export([set_field/3]).

-compile({parse_transform, vodka}).
-compile(export_all).


-record(rec, {f1 = 1, f2 = 2}).
-record(rec2, {f2 = b, f3 = c}).


set_field(K, V, A) ->
    A#rec{K = V}.
    %% case K of
    %% f1 -> A#rec{f1 = V};
    %% f2 -> A#rec{f2 = V}
    %% end.

set_fields(K1, V1, K2, V2, A) ->
    A#rec{K1 = V1, K2 = V2}.

update_field(K1, V1, V2, A) ->
    A#rec{K1 = V1, f2 = V2}.

%match_field(K1, V1, V2, A) ->
%    case A of
%        A=#rec{K1 = V1} -> true;
%        _ -> false
%    end.


get_field(K, A) ->
    #rec{K = V} = A = A,
    V.
    %% case K of
    %% f1 -> #rec{f1 = V} = A;
    %% f2 -> #rec{f2 = V} = A
    %% end.

get_multi_field(K1, K2, K3, A) ->
    #rec{K1 = V} = #rec{K2 = V} = A#rec{K3 = updated},
    V.

get_nested_field(K1, A) ->
   [#rec{K1 = V}] = A,
   V.

get_arg_field(#rec{K1 = V}, K1) ->
   V.

get_match_field(K1, A) ->
   [#rec{K1 = V}] = A,
   V.

all_matches_alive(#rec{f1 = F1}) ->
    F1;
all_matches_alive(F1) ->
    F1.



-include_lib("eunit/include/eunit.hrl").

-ifdef(TEST).

set_field_test_() ->
    [?_assertEqual(set_field(f1, 3, #rec{}), #rec{f1 = 3})
%   ,?_assertError({record_field_not_found,f666}, set_field(f666, 3, #rec{}))
    ].

set_fields_test_() ->
    [?_assertEqual(set_fields(f1, 3, f2, 4, #rec{}), #rec{f1 = 3, f2 = 4})].

update_fields_test_() ->
    [?_assertEqual(update_field(f1, 3, 4, #rec{}), #rec{f1 = 3, f2 = 4})].

get_field_test_() ->
    [?_assertEqual(get_field(f1, #rec{}), 1)
    ,?_assertEqual(get_field(f2, #rec{}), 2)
    ,?_assertEqual(get_field(f2, #rec{f2=3}), 3)].

all_matches_alive_test_() ->
    [?_assertEqual(all_matches_alive(#rec{}), 1)
    ,?_assertEqual(all_matches_alive(22), 22)].

-endif.
