-module(vodka).
-export([parse_transform/2]).



parse_transform(Forms, _Options) ->
    %% Extract record 
    Records = extract_record_definitions(Forms),
    %% RecordName2FieldName
    RN2DFN = orddict:from_list(record_definitions_to_pl(Records)),
    F = getter_and_setter_rec_expr_trans(RN2DFN),
    X = preorder_map_group(F, Forms),
%   io:format(user, "Before:\t~p\n\nAfter:\t~p\n", [Forms, X]),
    X.


getter_and_setter_rec_expr_trans(RN2DFN) ->
    fun(Node) ->
        case node_type(Node) of
        match_expr ->
            Exprs = flatten_expressions(Node),
            IsRecExpr = any_list(fun is_record_expr/1, Exprs),
            case IsRecExpr of
            true ->
%               io:format(user, "Match: ~p.~n", [Node]),
                %% Replace each `Key' variable with a value of its
                %% field.
                %% Map(FieldNameVar) -> ValidFieldNames
                VarFieldMap = map_variable_to_field_name(RN2DFN, Exprs),
                rewrite_fields(Node, VarFieldMap);
            false -> Node
            end;

        record_expr ->
            VarFieldMap = map_variable_to_field_name_for_single_record(RN2DFN,
                                                                       Node),
            rewrite_fields(Node, VarFieldMap);

        function ->
            Clauses = erl_syntax:function_clauses(Node),
            FunName = erl_syntax:function_name(Node),
            Clauses2 = lists:flatmap(transform_clause(RN2DFN), Clauses),
            Fn = erl_syntax:revert(copy_pos(Node,
                                            erl_syntax:function(FunName, 
                                                                Clauses2))),
            [io:format(user, "Fun before:~n~s~n"
                             "Fun after:~n~s~n", 
                      [erl_pp:function(Node), erl_pp:function(Fn)]) 
                       || Fn =/= Node],
            Fn;

        _NodeType ->
            Node
        end
        end.

            
transform_clause(RN2DFN) ->
    fun(Node) ->
        Patterns = erl_syntax:clause_patterns(Node),
        IsRecExpr = any_list(fun is_record_expr/1, Patterns),
        case IsRecExpr of
        true ->
            VarFieldMap = map_variable_to_field_name(RN2DFN, Patterns),
            case VarFieldMap of
            [_|_] ->
                {Vars, VarValues} = lists:unzip(VarFieldMap),
                VarValuesCases = multiply_lists(VarValues),
                io:format(user, "~nMap: ~p~n", [VarFieldMap]),
                [begin
                    AtomASTs = atoms(VarVals),
                    delete_redifined_fields(
                        replace_variables_with_values(Node, Vars, AtomASTs))
                 end
                 || VarVals <- VarValuesCases];
            [] -> [Node]
            end;
        false -> [Node]
        end
    end.


rewrite_fields(Node, VarFieldMap) ->
    case VarFieldMap of
    [_|_] ->
%       io:format(user, "Var fields: ~p.~n", [VarFieldMap]),
        {Vars, VarValues} = lists:unzip(VarFieldMap),
        VarValuesCases = multiply_lists(VarValues),
        %% Create case expression.
        CaseExprArg = erl_syntax:revert(tuple_or_single(vars(Vars))),
%       io:format(user, "Case expr arg: ~p.~n", [CaseExprArg]),
        CaseExprClauses = 
            [getter_and_setter_case_clause(Node, Vars, Values)
             || Values <- VarValuesCases],
        CaseExpr = erl_syntax:case_expr(CaseExprArg, 
                                        CaseExprClauses),
        CaseExpr2 = erl_syntax:revert(copy_pos(Node, CaseExpr)),
        io:format(user, "Form before:~n~s~n"
                        "Form after:~n~s~n", 
                  [erl_pp:expr(Node), erl_pp:expr(CaseExpr2)]),
        CaseExpr2;
    [] ->
%       io:format(user, "Skip the record_expr with known fields.~n", []),
        Node
    end.


getter_and_setter_case_clause(Node, Vars, Values) ->
%   io:format(user, "Case clase: ~p -> ~p~n", [Vars, Values]),
    AtomASTs = [erl_syntax:revert(erl_syntax:atom(Val)) || Val <- Values],
    Body = delete_redifined_fields(
        replace_variables_with_values(Node, Vars, AtomASTs)),
    erl_syntax:revert(
        erl_syntax:clause([tuple_or_single(atoms(Values))], none, [Body])).


delete_redifined_fields(Node) ->
    F = fun delete_redifined_field_trans/1,
    preorder_map(F, Node).


delete_redifined_field_trans(Node) ->
    KeyMaker = fun(Field) ->
        erl_syntax:atom_value(erl_syntax:record_field_name(Field))
        end,
    case node_type(Node) of
        record_expr ->
            Fields = erl_syntax:record_expr_fields(Node),
            FieldGroups = group_with(KeyMaker, Fields),
            %% The last update value of the field is seted.
            %% Default behaviour for erlc is to throw this error:
            %% `field f1 already defined in record rec'.
            FixedFields = [lists:last(FieldGroup)
                           || {_RecFieldName, FieldGroup} <- FieldGroups],
            erl_syntax:record_expr(erl_syntax:record_expr_argument(Node),
                                   erl_syntax:record_expr_type(Node),
                                   FixedFields);
        _OtherType -> 
            Node
    end.

    

map_variable_to_field_name(RN2DFN, Exprs) ->
    Recs = filter_list(fun is_record_expr/1, Exprs),
    Pairs = lists:flatmap(get_variable_field_names(RN2DFN), Recs),
    SortedPairs = lists:usort(Pairs),
    merge_field_values_for_same_var_names(SortedPairs).

map_variable_to_field_name_for_single_record(RN2DFN, Node) ->
    Pairs = (get_variable_field_names(RN2DFN))(Node),
    lists:usort(Pairs).



merge_field_values_for_same_var_names([{Var, Vals1}, {Var, Vals2}|T]) ->
    case ordsets:intersection(Vals1, Vals2) of
        [] -> 
            %% Skip is an empty pair.
            error_logger:error_msg("Will never match: ~p~n", [Var]), 
            merge_field_values_for_same_var_names(T);
        Vals3 ->
            H = {Var, Vals3},
            merge_field_values_for_same_var_names([H|T])
    end;
merge_field_values_for_same_var_names([H|T]) ->
    [H|merge_field_values_for_same_var_names(T)];
merge_field_values_for_same_var_names([]) ->
    [].
    


get_variable_field_names(RN2DFN) ->
    fun(Node) ->
    case is_record_expr(Node) of
        true ->
            Type      = erl_syntax:record_expr_type(Node),
            Fields    = erl_syntax:record_expr_fields(Node),
            case lists:partition(fun is_valid_record_expr_field/1, Fields) of
                {_, []} -> 
                    [];
                {_ValidFields, InvalidFields} ->
%                   io:format(user, "~nInvalid fields: ~p.~n", [InvalidFields]),
                    FieldNames = ordsets:from_list(atom_vals(field_names(RN2DFN,
                                                                        Type))),
                    [{field_key_as_atom(Field), FieldNames}
                     || Field <- InvalidFields]
            end;
        false -> []
    end
    end.

field_key_as_atom(Field) ->
    erl_syntax:variable_name(erl_syntax:record_field_name(Field)).


is_record_expr(Node) ->
    node_type(Node) =:= record_expr.



is_valid_record_expr_field(Field) ->
    Name = erl_syntax:record_field_name(Field),
    node_type(Name) =/= variable.


field_names(RN2DFN, Type) -> 
    RecNameAtom = erl_syntax:atom_value(Type),
    proplists:get_value(RecNameAtom, RN2DFN).
    
    

%% -----------------------------------------------------------------------
%% Helpers
%% -----------------------------------------------------------------------

extract_record_definitions(Forms) ->
    [erl_syntax:attribute_arguments(F) 
       || F <- Forms, node_type(F) =:= attribute, 
          erl_syntax:atom_value(erl_syntax:attribute_name(F)) =:= record].



-spec record_definitions_to_pl(Recs) -> RecPL when
    Recs      :: [erl_syntax:syntaxTree()],
    RecPL     :: [{RecName, Fields}],
    Fields    :: [{FieldName, erl_syntax:syntaxTree()}],
    RecName   :: atom(),
    FieldName :: atom().

record_definitions_to_pl(Recs) ->
    [to_element(RecName, RecFields) || [RecName, RecFields] <- Recs].


-spec to_element(RecName, RecFields) -> Elem when
    RecFields   :: [Field],
    Field       :: erl_syntax:syntaxTree(),
    RecName     :: erl_syntax:syntaxTree(),
    Elem        :: {RecNameAtom, [RecNameAtom]},
    RecNameAtom :: atom().
    
to_element(RecName, RecFields) ->
    FieldNames  = [erl_syntax:record_field_name(Field) 
                   || Field <- erl_syntax:tuple_elements(RecFields)],
    RecNameAtom = erl_syntax:atom_value(RecName),
    {RecNameAtom, FieldNames}.


preorder_map(F, Form) ->
    Tree = F(Form),
    Tree2 = 
        case erl_syntax:subtrees(Tree) of
        [] ->
            Tree;
        List ->
            Groups = [preorder_map_group(F, Group) || Group <- List],
            erl_syntax:update_tree(Tree, Groups)
        end,
    erl_syntax:revert(Tree2).


preorder_map_group(F, Group) ->
    [preorder_map(F, Subtree) || Subtree <- Group].



preorder_foldl(F, Acc, Form) ->
    {Tree, Acc2} = F(Form, Acc),
    {Tree3, Acc4} = 
        case erl_syntax:subtrees(Tree) of
        [] ->
            {Tree, Acc2};
        List ->
            {Groups, Acc3} = lists:foldl(preorder_foldl_group(F), Acc2, List),
            Tree2 = erl_syntax:update_tree(Tree, Groups),
            {Tree2, Acc3}
        end,
    {erl_syntax:revert(Tree3), Acc4}.


preorder_foldl_group(F) ->
    FF = fun(Node, Acc) -> preorder_foldl(F, Acc, Node) end,
    fun(Group, Acc) ->
        lists:foldl(FF, Acc, Group)
    end.


filter(F, Form) ->
    case F(Form) of
        true -> [Form];
        false -> []
    end 
    ++
    case erl_syntax:subtrees(Form) of
    [] ->
        [];
    List ->
        lists:flatmap(fun(Group) -> 
                lists:flatmap(fun(X) -> filter(F, X) end, Group)
            end, List)
    end.


filter_list(F, Forms) ->
    lists:flatmap(fun(Form) -> filter(F, Form) end, Forms).


%% @doc Like `list:any/2', but for `syntaxTree'.
any(F, Form) when not is_list(Form) ->
    F(Form) orelse any_list_list(F, erl_syntax:subtrees(Form)).

%% @doc Execute any for a list.
any_list(F, L) when is_list(L) ->
    lists:any(fun(X) -> any(F, X) end, L).

any_list_list(F, L) when is_list(L) ->
    lists:any(fun(X) -> any_list(F, X) end, L).


copy_pos(From, To) ->
    Pos = erl_syntax:get_pos(From),
    erl_syntax:set_pos(To, Pos).


%% @doc Return a position of the element `X' in the list `L'.
-spec elem_pos(X, L) -> non_neg_integer() | undefined
    when X :: term(), L :: [X].

elem_pos(X, L) ->
    elem_pos(L, X, 1).


elem_pos(X, [X|T], P) ->
    P;
elem_pos(X, [H|T], P) ->
    elem_pos(X, T, P+1);
elem_pos(_X, [], _P) ->
    undefined.





%% @doc Form a list of explessions from a Tree.
%%
%% Example:
%% Tree: E1 = E2 = E3 = E4;
%% List: [E1, E2, E3, E4].
flatten_expressions(Node) ->
    %% "Pattern = Body"
    match_expr = node_type(Node),
    Pattern = erl_syntax:match_expr_pattern(Node),
    Body    = erl_syntax:match_expr_body(Node),
    flatten_expressions(Pattern, Body).


flatten_expressions(Pattern, Body) ->
    case node_type(Body) of
        match_expr ->
            [Pattern|flatten_expressions(Body)];
        _ ->
            [Pattern, Body]
    end.


node_type(Node) ->
    erl_syntax:type(Node).


atom_vals(Atoms) ->
    [erl_syntax:atom_value(X) || X <- Atoms].

atoms(Atoms) ->
    [erl_syntax:atom(X) || X <- Atoms].

vars(Vars) ->
    [erl_syntax:variable(X) || X <- Vars].


%% [[1,2,3],[4,5,6],[7,8,9]] =>
%% 1 4 7
%% 1 4 8
%% 1 4 9
%% 1 5 7
%% ...
multiply_lists([]) ->                         
    [];                                       
multiply_lists([H]) ->                        
    [[X] || X <- H];                          
multiply_lists([H|T]) ->                      
    [[X|Y] || X <- H, Y <- multiply_lists(T)].



replace_variables_with_values(Node, [VarName|Vars], [Val|Values]) ->
    Node2 = with_same_variable(fun(_) -> Val end, VarName, Node),
    replace_variables_with_values(Node2, Vars, Values);
replace_variables_with_values(Node, [], []) ->
    Node.


is_variable(VarName) ->
    fun(Node) ->
            case node_type(Node) of
                variable -> erl_syntax:variable_name(Node) =:= VarName;
                _OtherType -> false
            end
        end.

with_same_variable(F, VarName, Tree) ->
    case node_type(Tree) of
        fun_expr ->
            Clauses = erl_syntax:fun_expr_clauses(Tree),
            IsRedefine = 
            [any_list(is_variable(VarName), erl_syntax:clause_patterns(Clause))
             || Clause <- Clauses],
            if 
                %% It has other meaning now.
                IsRedefine -> Tree;
                true -> with_same_variable_subtrees(F, VarName, Tree)
            end;
        variable ->
            case (is_variable(VarName))(Tree) of
                %% Skip an other variable.
                false -> Tree;
                true -> F(Tree)
            end;
        _OtherType ->
            with_same_variable_subtrees(F, VarName, Tree)
    end.

with_same_variable_subtrees(F, VarName, Tree) ->
    case erl_syntax:subtrees(Tree) of
    [] ->
        Tree;
    List ->
        Groups = [[with_same_variable(F, VarName, SubTree) 
                   || SubTree <- Group]
                  || Group <- List],
        erl_syntax:revert(erl_syntax:update_tree(Tree, Groups))
    end.







%% @doc Looks like `GROUP BY KeyMaker(List)' in SQL.
%% It is from `test/xapian_proper_tests.erl'.
-spec group_with(fun(), list()) -> list({term(),list()}).

group_with(_keymaker, []) ->
    [];

group_with(KeyMaker, List) ->
    %% Map
    Mapped = [{KeyMaker(X), X} || X <- List],
    [SortedH|SortedT] = lists:keysort(1, Mapped),

    %% Reduce
    group_reduce(SortedT, [SortedH], []).


%% @doc Return `[{Key, [Value1, Value2, ...]}]'.
%% @end
%%
%% Still the same group:
group_reduce([{Key, _}=H|T],  [{Key, _}|_] = SubAcc,  Acc) ->
    group_reduce(T,  [H|SubAcc],  Acc);

%% Add the new group:
group_reduce([H|T],  SubAcc,  Acc) ->
    NewAcc = add_sub_acc(SubAcc, Acc),
    group_reduce(T,  [H],  NewAcc);

%% End of the list
group_reduce([],  SubAcc,  Acc) ->
    NewAcc = add_sub_acc(SubAcc, Acc),
    lists:reverse(NewAcc).


add_sub_acc([{Key, _Val}|_] = SubAcc, Acc) when is_list(Acc) ->
    Elem = {Key, lists:reverse(delete_keys(SubAcc))},
    [Elem | Acc].


delete_keys(List) ->
    [Val || {_Key, Val} <- List].


tuple_or_single([X]) ->
    X;
tuple_or_single(Xs) ->
    erl_syntax:tuple(Xs).
