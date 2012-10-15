-module(vodka).
-export([parse_transform/2]).



parse_transform(Forms, _Options) ->
    %% Extract record 
    Records = extract_record_definitions(Forms),
    %% RecordName2FieldName
    RN2DFN = orddict:from_list(record_definitions_to_pl(Records)),
    F = getter_rec_expr_trans(RN2DFN),
    X = preorder_map_group(F, Forms),
    io:format(user, "Before:\t~p\n\nAfter:\t~p\n", [Forms, X]),
    X.


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


getter_rec_expr_trans(RN2DFN) ->
    fun(Node) ->
        case node_type(Node) of
        match_expr ->
            Exprs = flatten_expressions(Node),
            IsCreateExpr = lists:any(fun is_record_expr/1, Exprs),
            case IsCreateExpr of
            true ->
%               io:format(user, "Match: ~p.~n", [Node]),
                %% Replace each `Key' variable with a value of its
                %% field.
                %% Map(FieldNameVar) -> ValidFieldNames
                VarFieldMap = map_variable_to_field_name(RN2DFN, Exprs),
                case VarFieldMap of
                [_|_] ->
                    io:format(user, "Var fields: ~p.~n", [VarFieldMap]),
                    {Vars, VarValues} = lists:unzip(VarFieldMap),
                    VarValuesCases = multiply_lists(VarValues),
                    %% Create case expression.
                    CaseExprArg = erl_syntax:revert(erl_syntax:tuple(vars(Vars))),
                    io:format(user, "Case expr arg: ~p.~n", [CaseExprArg]),
                    CaseExprClauses = 
                        [getter_case_clause(Node, Vars, Values)
                         || Values <- VarValuesCases],
                    CaseExpr = erl_syntax:case_expr(CaseExprArg, 
                                                    CaseExprClauses),
                    CaseExpr2 = erl_syntax:revert(copy_pos(Node, CaseExpr)),
                    io:format(user, "Form before:~n~s~n"
                                    "Form after:~n~s~n", 
                              [erl_pp:expr(Node), erl_pp:expr(CaseExpr2)]),
                    CaseExpr2;
                [] ->
                    io:format(user, "Skip the record_expr with known fields.~n", []),
                    Node
                end;
            false -> Node
            end;
        _NodeType ->
            Node
        end
        end.


getter_case_clause(Node, Vars, Values) ->
    io:format(user, "Case clase: ~p -> ~p~n", [Vars, Values]),
    AtomASTs = [erl_syntax:revert(erl_syntax:atom(Val)) || Val <- Values],
    F = replace_variables_with_values(Vars, AtomASTs),
    Body = preorder_map(F, Node),
    erl_syntax:revert(
        erl_syntax:clause([erl_syntax:tuple(atoms(Values))], none, [Body])).


map_variable_to_field_name(RN2DFN, Exprs) ->
    Pairs = lists:flatmap(get_variable_field_names(RN2DFN), Exprs),
    SortedPairs = lists:usort(Pairs),
    merge_field_values_for_same_var_names(SortedPairs).



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
                    io:format(user, "~nInvalid fields: ~p.~n", [InvalidFields]),
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
    
    


copy_pos(From, To) ->
    Pos = erl_syntax:get_pos(From),
    erl_syntax:set_pos(To, Pos).





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



replace_variables_with_values(Vars, Values) ->
    VarValDict = orddict:from_list(lists:zip(Vars, Values)),
    io:format(user, "~nVar -> Val dictionary: ~p~n", [VarValDict]),
    fun(Node) ->
        case node_type(Node) of
            variable ->
                Name = erl_syntax:variable_name(Node),
                case orddict:find(Name, VarValDict) of
                    {ok, Val} ->
                        io:format(user, "~nOld val: ~p~n"
                                        "New val: ~p~n", [Val, Node]),
                        erl_syntax:revert(copy_pos(Node, Val));
                    error ->
                        Node
                end;
            _OtherType ->
                Node
        end
    end.
