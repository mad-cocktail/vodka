-module(vodka).
-export([parse_transform/2]).



parse_transform(Forms, _Options) ->
    %% Extract record 
    Records = extract_record_definitions(Forms),
    %% RecordName2FieldName
    RN2DFN = orddict:from_list(record_definitions_to_pl(Records)),
    F = setter_rec_expr_trans(RN2DFN),
    X = [postorder(F, Tree) || Tree <- Forms],
%   io:format(user, "Before:\t~p\n\nAfter:\t~p\n", [Forms, X]),
    X.


extract_record_definitions(Forms) ->
    [erl_syntax:attribute_arguments(F) 
       || F <- Forms, erl_syntax:type(F) =:= attribute, 
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


postorder(F, Form) ->
    NewTree =
        case erl_syntax:subtrees(Form) of
        [] ->
            Form;
        List ->
            Groups = [handle_group(F, Group) || Group <- List],
            Tree2 = erl_syntax:update_tree(Form, Groups),
            Form2 = erl_syntax:revert(Tree2),
            Form2
        end,
    F(NewTree).


setter_rec_expr_trans(RN2DFN) ->
    fun(Node) ->
        case erl_syntax:type(Node) of
        record_expr ->
            Argument  = erl_syntax:record_expr_argument(Node),
            Type      = erl_syntax:record_expr_type(Node),
            Fields    = erl_syntax:record_expr_fields(Node),
            case lists:partition(fun is_valid_record_expr_field/1, Fields) of
                {_, []} -> 
                    Node;
                {ValidFields, InvalidFields} ->
                    lists:foldr(set_unknown_field_name(RN2DFN, Type), 
                        erl_syntax:record_expr(Argument, Type, ValidFields),
                        InvalidFields)
            end;
        _NodeType ->
            Node
        end
        end.


handle_group(F, Group) ->
    [postorder(F, Subtree) || Subtree <- Group].


is_valid_record_expr_field(Field) ->
    Name = erl_syntax:record_field_name(Field),
    erl_syntax:type(Name) =/= variable.

set_unknown_field_name(RN2DFN, Type) -> 
    RecNameAtom = erl_syntax:atom_value(Type),
    fun(Field, Argument) ->
        VarName    = erl_syntax:record_field_name(Field),
        Value      = erl_syntax:record_field_value(Field),
        FieldNames = proplists:get_value(RecNameAtom, RN2DFN),
        Clauses    = [begin
                        NewField = copy_pos(Field, 
                                            erl_syntax:record_field(FieldName, 
                                                                    Value)),
                        Body = [erl_syntax:record_expr(Argument, Type, [NewField])],
                        erl_syntax:clause([FieldName], none, Body)
                      end
                      || FieldName <- FieldNames],
        Otherwise  = erl_syntax:clause([erl_syntax:underscore()],
                                       none,
                                       [record_field_not_exist(VarName)]),
        Clauses2  = [copy_pos(Field, X) || X <- Clauses ++ [Otherwise]],
        erl_syntax:revert(copy_pos(Field, erl_syntax:case_expr(VarName, Clauses2)))
    end.


copy_pos(From, To) ->
    Pos = erl_syntax:get_pos(From),
    erl_syntax:set_pos(To, Pos).


record_field_not_exist(VarName) ->
    NotFound    = erl_syntax:atom(record_field_not_found),
    Error       = copy_pos(VarName, erl_syntax:tuple([NotFound, VarName])),
    throw_error(Error).

throw_error(Error) ->
    Module      = erl_syntax:atom(erlang),
    Function    = erl_syntax:atom(error),
    copy_pos(Error, erl_syntax:application(Module, Function, [Error])).

