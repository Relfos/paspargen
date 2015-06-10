{how to support ambiguities...
let multiple tokens reside at same place, as long as they have the same value/length!

 ? BinaryOperator
 
WhileStatement, ForStatement, RepeatStatement,


Possible improvement -> for lookead parsing (eg:binary expression), generate shorted parseBinaryExpr() method

Possible improvement -> turn simple regex (eg: "true") into string matchers 
}

Program paspargen;

{$APPTYPE CONSOLE}

Uses TERRA_Utils, TERRA_String, TERRA_Stream, TERRA_MemoryStream, TERRA_FileStream, TERRA_Error,
  TERRA_Lexer, TERRA_Parser
  ;

(*

Block = 'begin' Statement 'end' ';';
Declarations = Consts, Vars, Types, Labels;

Statement = StatementList, WhileStatement, ForStatement, RepeatStatement, Assignment;    

    WhileStatement = 'while' Condition 'do' Statement;
    RepeatStatement = 'repeat' Statement 'until' Condition ';';
    Assignment = Variable ':=' Expression ';';

    Comment = BeginComment -> { } -> EndComment;
    Define = '{$IFDEF' Identifier '}'  -> { } -> '{$ENDIF}';

*)
{

http://en.wikipedia.org/wiki/LALR_parser
http://wiki.freepascal.org/Regexpr

http://stackoverflow.com/questions/9814528/recursive-descent-parser-implementation

http://www.tutorialspoint.com/compiler_design/compiler_design_bottom_up_parser.htm


possible cases

Abstract class with children or Logical rule (meaning no class is generated)
Goal = Program, Unit;

Value (RegExpr) -> Those will be parsed and returned as a string
IntegerNumber = "[0-9]+";


AST NODE
Program = 'program' Identifier ';' [UsesDecl] [Declarations] Block '.';
generates
  ProgramNode = Class(GoalNode)
    Protected
      _Identifier:IdentifierNode;
      _UsesDecl:_UsesDeclNode;
      _Declarations:DeclarationsNode;
      etc
  End;


parser algoritm
1 - start with goal
2 - goal tries matching any of his rules
3 - found rule tries to parse any of its rules
4 - etc etc repeat recursively

LEXER algoritm

possibilties:array of string -> contains all regexpr and normal tokens, sorted by length/frequency
1 - get next char
2 - test match for each possibility_value
3 - those that failed get discarded
 (just send them to the end of the list)
4 - check if any match is complete, if yes, generate a new token
5 - if all matches failed, generate error
6 - else, return to step 1

Note: this has to respect precedence, and should find the longest token!!!


  AST := Parser.ParseGoal();

  Parser.ParseGoal(); Begin  Result := Parser.ParseProgram();  End;

  Parser.ParseProgram();
  Begin
    ExpectToken('program');
    Self.Identifier := ExpectNode(Self.ParseIdentifier());
    ExpectToken(';');
    Self.UsesDecl := OptionalNode(Self.ParseUsesDecl());
    Self.Declarations := OptionalNode(Self.ParseDeclarations());
    Selfe.Block := ExpectNode(Self.ParseBlock());
    ExpectToken('.');

    Result := ProgramNode.Create(Identifier, UsesDecl, Declarations, Block);
  End;

}


Type
  GrammarRuleKind = (rule_Unknown, rule_abstract, rule_compound, rule_set, rule_token, rule_RegExpr);

  GrammarEntryKind = (grammar_Token, grammar_RegExpr, grammar_Rule);

  Grammar = Class;
  GrammarToken = Class;

  GrammarEntry = Record
    Kind:GrammarEntryKind;
    Value:TERRAString;
    Annotation:TERRAString;
    OptionalID:Integer;
    Null:Boolean;
    ListID:Integer;
    Token:GrammarToken;
    Discard:Boolean;
    CustomType:TERRAString;

    GenType:TERRAString;
    IsLookAhead:Boolean;
    IsImportant:Boolean;
  End;

  GrammarRuleLine = Class
    EntryList:Array Of GrammarEntry;
    EntryCount:Integer;

    Procedure AddEntry(Const S:TERRAString; Kind:GrammarEntryKind; OptionalID:Integer);

    Function GetKind():GrammarRuleKind;
  End;

  GrammarRule = Class
    Public
      Name:TERRAString;

      Lines:Array Of GrammarRuleLine;
      LineCount:Integer;

      CurrentLine:GrammarRuleLine;

      Procedure NewLine();

      Function GetKind():GrammarRuleKind;
  End;

  {GrammarSet = Class
    Owner:Grammar;
    Name:TERRAString;

    Values:Array Of TERRAString;
    ValueCount:Integer;

    Constructor Create(Const Name: TERRAString; Owner:Grammar);
    Procedure AddValue(Const S, Annotation:TERRAString);
  End;}

  GrammarNode = Class;

  GrammarField = Record
    Name:TERRAString;
    Entry:GrammarEntry;
    Node:GrammarNode;
  End;

  GrammarToken = Class
    ID:Cardinal;
    Name:TERRAString;
    Value:TERRAString;
    RegExpr:Boolean;
    Discard:Boolean;

    Procedure GenerateRegExp(Dest:Stream);
    Procedure GenerateStringMatch(Dest:Stream);
  End;


  GrammarNode = Class
    Index:Integer;
    
    Owner:Grammar;
    Name:TERRAString;
    Parent:GrammarNode;

    Declared:Boolean;
    ArrayDeclared:Boolean;

    LookAheadIndex:Integer;
    ImportantIndex:Integer;

    Fields:Array Of GrammarField;
    FieldCount:Integer;

    Children:Array Of GrammarNode;
    ChildrenCount:Integer;
    ExpectedChildrenCount:Integer;
    ExpectedChildren:Array Of TERRAString;

    Constructor Create(Index:Integer; Const Name: TERRAString; Owner: Grammar);
    Procedure AddField(Const Entry:GrammarEntry);

    Function UseNode(Node:GrammarNode):Boolean;

    Function GenNextToken(Const OptValue:TERRAString; Const Token:GrammarToken):TERRAString;

    Function GenArgs():TERRAString;
    Function GenParseNode(Const Store, NodeKind, IsOpt:TERRAString):TERRAString;

    Procedure GenLookAhead(From:GrammarNode; Dest:Stream; Tabs:TERRAString);

    Procedure GenCompoundNode(Dest:Stream; AllowOpt:Boolean; Const IsOpt:TERRAString);
    Procedure GenAbstractNode(Dest:Stream; AllowOpt:Boolean; Const IsOpt:TERRAString);
  End;

  Grammar = Class
    Protected
      Name:TERRAString;

      Rules:Array Of GrammarRule;
      RuleCount:Integer;

      Tokens:Array Of GrammarToken;
      TokenCount:Integer;
      LastTokenID:Cardinal;
      TokenNames:Array Of TERRAString;

      Nodes:Array Of GrammarNode;
      NodeCount:Integer;

{      Sets:Array Of GrammarSet;
      SetCount:Integer;}

      Function GetParent(Const Value:TERRAString):GrammarNode;
      Function GetNode(Const Value:TERRAString):GrammarNode;
      Function HasNode(Const Value:TERRAString):Boolean;
      Function GetToken(Value, Name:TERRAString; ID:Integer; RegExpr, Discard:Boolean):GrammarToken;

{      Function GetSet(Const Value:TERRAString):GrammarSet;
      Function FindSet(Const Value:TERRAString):GrammarSet;}

      Function OrderNode(Index:Integer):Boolean;

      Function AllocToken(Const Name:TERRAString):Cardinal;

    Public
      Constructor Create(S:TERRAString);

      Function ProcessRules():Boolean;
      
      Procedure GenerateNodesInterface(Dest:Stream);
      Procedure GenerateNodesImplementation(Dest:Stream);

      Procedure GenerateTokenInterface(Dest:Stream);
      Procedure GenerateTokenImplementation(Dest:Stream);

      Procedure GenerateLexerInterface(Dest:Stream);
      Procedure GenerateLexerImplementation(Dest:Stream);

      Procedure GenerateParserInterface(Dest:Stream);
      Procedure GenerateParserImplementation(Dest:Stream);

      Procedure GenerateVisitorInterface(Dest:Stream);
      Procedure GenerateVisitorImplementation(Dest:Stream);

{      Procedure GeneratePreProcessorInterface(Dest:Stream);
      Procedure GeneratePreProcessorImplementation(Dest:Stream);}

      Procedure Generate(Dest:Stream);
  End;

Var
  Code, PrevCode:TERRAString;
  PrevLine:Integer;
  LineNumber:Integer;
  LastChar:TERRAChar;

Function NextToken():TERRAString;
Var
  It:StringIterator;
  Temp:TERRAString;
  C, Inside:TERRAChar;
  Ignore, SkipChar:Boolean;
Begin
  PrevCode := Code;
  PrevLine := LineNumber;

  Inside := 0;
  Ignore := False;
  C := 0;

  StringCreateIterator(Code, It);
  Result := '';
  SkipChar := False;
  While It.HasNext() Do
  Begin
    C := It.GetNext();

    If (Ignore) Then
    Begin
      //If (C=Ord('}')) Then        Ignore := False;

      Continue;
    End;

    If C = NewLineChar Then
      Inc(LineNumber);

    If (C=Ord('/')) And (LastChar=C) Then
    Begin
      While (It.HasNext()) And (C<>NewLineChar) Do
      Begin
        C := It.GetNext();
      End;
      Inc(LineNumber);
    End;

    If (C=Ord('\')) And (LastChar <> Ord('\')) Then
    Begin
      C := It.GetNext();

      Case C Of
      Ord('n'): C := NewLineChar;
      Ord('t'): C := 9;
      End;

      LastChar := C;
      StringAppendChar(Result, C);
      Continue;
    End;

    LastChar := C;

    If (C<=13) Then
      Continue;

    If (Inside>0) And (Not SkipChar) Then
    Begin
      If (C = Inside) Then
      Begin
        It.Split(Temp, Code);
        StringAppendChar(Result, C);
        Break;
      End Else
        StringAppendChar(Result, C);

      Continue;
    End;

    If ((C=Ord('"')) Or (C=Ord(''''))) And (Not SkipChar) Then
    Begin
      Inside := C;
      StringAppendChar(Result, C);
      Continue;
    End;

    {If (C=Ord('(')) And (Not SkipChar) Then
    Begin
      Inside := Ord(')');
      StringAppendChar(Result, C);
      Continue;
    End;}

    SkipChar := False;

    {If (C=Ord('{')) Then
    Begin
      Ignore := True;
      Continue;
    End;}

    If (CharLower(C)>=Ord('a')) And (CharLower(C)<=Ord('z'))  Then
      StringAppendChar(Result, C)
    Else
    If (CharLower(C)>=Ord('0')) And (CharLower(C)<=Ord('9'))  Then
      StringAppendChar(Result, C)
    Else
    If (C=Ord('_')) Or (C=Ord('"')) Then
      StringAppendChar(Result, C)
    Else
    If (Result<>'') Or (C>32) Then
    Begin
      If (Result = '') Then
      Begin
        It.Split(Result, Code);
        Result := StringFromChar(C);
      End Else
      Begin
        It.Split(Temp, Code);
        If C > 32 Then
          StringPrependChar(Code, C);
      End;

      Break;
    End;
  End;

  Result := StringTrim(Result);
  WriteLn(Result);

  If Result = '' Then
    RaiseError('unexpected end of file!');
End;

Procedure RevertToken();
Begin
  Code := PrevCode;
  LineNumber := PrevLine;
End;

Procedure Expect(Const S:TERRAstring);
Var
  Token:TERRAString;
Begin
  Token := NextToken();
  If Not StringEquals(S, Token) Then
  Begin
    RaiseError('Sintax error: expected '+S+' at line '+IntToString(LineNumber));
  End;
End;

Function ExpectID():TERRAString;
Var
  It:StringIterator;
  C:TERRAChar;
Begin
  Result := NextToken();

  StringCreateIterator(Result, It);
  While It.HasNext() Do
  Begin
    C := It.GetNext();
    If (C<Ord('A')) Then
    Begin
      RaiseError('Sintax error: expected identifier at line '+IntToString(LineNumber));
    End;
  End;
End;

Function ExpectType():TERRAString;
Begin
  Result := ExpectID();
End;

{ GrammarSet }
{Constructor GrammarSet.Create(const Name: TERRAString; Owner: Grammar);
Begin
  Self.Owner := Owner;
  self.Name := Name;
  Self.ValueCount := 0;
End;

Procedure GrammarSet.AddValue(const S, Annotation: TERRAString);
Begin
  Inc(ValueCount);
  SetLength(Values, ValueCount);

  If Annotation<>'' Then
    Values[Pred(ValueCount)] := Annotation
  Else
    Values[Pred(ValueCount)] := S;
End;}

/////////////////////////////////////////////////////////////////////////////////

{ GrammarToken }
Procedure GrammarToken.GenerateRegExp(Dest: Stream);
Type
  RegExState = Record
    Condition:TERRAString;
    Opt:Integer;
  End;

Var
  It, It2:StringIterator;
  Temp:StringIteratorState;
  C, PC:TERRAChar;
  S, S2, Prev, Rest:TERRAString;
  Negate:Boolean;

  I, CurrentState, FinalState:Integer;
  States:Array Of RegExState;
  StateCount:Integer;
Begin
  StringCreateIterator(Self.Value, It);

  States := Nil;
  StateCount := 0;

  Negate := False;
  While It.HasNext Do
  Begin
    C := It.GetNext();
    S := '';

    If (C=Ord('[')) Then
    Begin
      If It.PeekNext() = Ord('^') Then
      Begin
        Negate := True;
        It.GetNext();
      End Else
        Negate := False;

      S2 := '';
      While It.HasNext() Do
      Begin
        C := It.GetNext();
        If C= Ord(']') Then
          Break;

        StringAppendChar(S2, C);
      End;

      StringCreateIterator(S2, It2);
      S := '';
      Prev := '';
      While It2.HasNext() Do
      Begin
        PC := C;
        C := It2.GetNext();

        If (C = Ord('-')) Then
        Begin
          S := Prev;
          C := It2.GetNext();

          If (C=0) Then
            C := Ord('-');

          If S<>'' Then
            S := S + ' Or ';
          S := S + '((C >= ' + CardinalToString(PC)+') And (C <= ' + CardinalToString(C)+'))';

          Continue;
        End;

        Prev := S;
        If S<>'' Then
          S := S + ' Or ';
        S := S + '(C = ' + CardinalToString(C)+')';


      End;

      If Negate Then
        S := 'Not ('+S+')';

      S := 'If ' + S + ' Then';
      //_a-zA-Z
    End Else
    Begin
      S := S + '(C = ' + CardinalToString(C)+')';
      S := 'If ' + S + ' Then';
    End;

    Inc(StateCount);
    SetLength(States, StateCount);

    States[Pred(StateCount)].Condition := S;

    It.SaveState(Temp);
    C := It.GetNext();
    If (C=Ord('*')) Then
    Begin
      States[Pred(StateCount)].Opt := -1;
    End Else
    If (C=Ord('?')) Then
    Begin
      States[Pred(StateCount)].Opt := 1;
    End Else
    If (C=Ord('+')) Then
    Begin
      States[Pred(StateCount)].Opt := 2;
    End Else
    Begin
      States[Pred(StateCount)].Opt := 0;
      It.RestoreState(Temp);
    End;

    Negate := False;
  End;

  CurrentState := 0;
  FinalState := Pred(StateCount);
  For I:=0 To Pred(StateCount) Do
  If (States[I].Opt=0) Or (States[I].Opt=2) Then
  Begin
    FinalState := I;
  End;

  Dest.WriteLine(#9+'Case _State Of');
  For I:=0 To Pred(StateCount) Do
  Begin
    Dest.WriteLine(#9#9+IntToString(CurrentState)+':');

    {If (Opt<0) And (Not It.HasNext()) Then // optimization for * in last position
    Begin
      Dest.WriteLine(#9#9#9+'Begin');
      Dest.WriteLine(#9#9#9#9+'Result := True;');
      Dest.WriteLine(#9#9#9+'End;');
      Break;
    End;}

    Dest.WriteLine(#9#9#9+States[I].Condition);
    Dest.WriteLine(#9#9#9+'Begin');
    If I = FinalState Then
      Dest.WriteLine(#9#9#9+'_Complete := True;');
    Dest.WriteLine(#9#9#9#9+'Result := True;');

    If (States[I].Opt>=0) Then
      Dest.WriteLine(#9#9#9#9+'Inc(_State);');

    Dest.WriteLine(#9#9#9+'End Else');

    If (States[I].Opt = 2) Then // + -> 1 or more reps
    Begin
      Dest.WriteLine(#9#9#9+'Begin');
      Dest.WriteLine(#9#9#9#9+'Result := False;');
      Dest.WriteLine(#9#9#9+'End;');

      Inc(CurrentState);
      Dest.WriteLine(#9#9+IntToString(CurrentState)+':');
      Dest.WriteLine(#9#9#9+S);
      Dest.WriteLine(#9#9#9+'Begin');
      If I = FinalState Then
        Dest.WriteLine(#9#9#9+'_Complete := True;');
      Dest.WriteLine(#9#9#9#9+'Result := True;');
      Dest.WriteLine(#9#9#9+'End Else');
    End;

    If (States[I].Opt<>0) Then
    Begin
      Dest.WriteLine(#9#9#9+'Begin');
      Dest.WriteLine(#9#9#9#9+'Inc(_State);');
      Dest.WriteLine(#9#9#9#9+'Result := Self.Match(C);');
      Dest.WriteLine(#9#9#9+'End;');
    End Else
      Dest.WriteLine(#9#9#9#9+'Result := False;');

    Dest.WriteLine();

    Inc(CurrentState);
  End;

  Dest.WriteLine(#9#9+'Else');
//  Dest.WriteLine(#9#9+'Begin');
//  Dest.WriteLine(#9#9#9+'_Complete := (_State >= '+IntToString(Pred(State))+');');
  Dest.WriteLine(#9#9#9+'Result := False;');
//  Dest.WriteLine(#9#9+'End;');
  Dest.WriteLine(#9+'End;');
End;

Procedure GrammarToken.GenerateStringMatch(Dest: Stream);
Var
  It:StringIterator;
  C:TERRAChar;
  S:TERRAString;
Begin
  If (StringLength(Self.Value)=1) Then
  Begin
    C := StringFirstChar(Self.Value);
    S := '  // '+Self.Value;
    Dest.WriteLine(#9+'If (C = '+CardinalToString(C)+') Then'+S);
    Dest.WriteLine(#9#9+'Result := tokenAccept');
    Dest.WriteLine(#9+'Else');
    Dest.WriteLine(#9#9+'Result := tokenReject;');
  End;

  {Dest.WriteLine(#9+'Case _Index Of');
  StringCreateIterator(Self.Value, It);
  While It.HasNext Do
  Begin
    C := It.GetNext();

    Dest.WriteLine(#9+IntToString(It.Position)+':');
    Dest.WriteLine(#9#9+'If (C = '+CardinalToString(C)+') Then');
    Dest.WriteLine(#9#9#9+'Result := tokenIncomplete');
    Dest.WriteLine(#9#9+'Else');
    Dest.WriteLine(#9#9#9+'Result := tokenReject;');
    Dest.WriteLine();
  End;
  Dest.WriteLine(#9+'Else');
  Dest.WriteLine(#9#9+'Result := tokenReject;');
  Dest.WriteLine(#9+'End;');}
End;

{ GrammarNode }
Constructor GrammarNode.Create(Index:Integer; Const Name: TERRAString; Owner: Grammar);
Begin
  Self.Index := Index;
  Self.Name := Name;
  Self.Owner := Owner;
  Self.Parent := Nil;
  Self.LookAheadIndex := -1;
  Self.ImportantIndex := -1;
end;

Procedure GrammarNode.AddField(Const Entry:GrammarEntry);
Var
  Node:GrammarNode;
Begin
  If Entry.Kind = grammar_Rule Then
  Begin
    Node := Self.Owner.GetNode(Entry.Value);
  End Else
    Node := Nil;

  Inc(FieldCount);
  SetLength(Fields, FieldCount);
  Fields[Pred(FieldCount)].Node := Node;
  Fields[Pred(FieldCount)].Entry := Entry;

  If Entry.Annotation <>'' Then
    Fields[Pred(FieldCount)].Name := Entry.Annotation
  Else
    Fields[Pred(FieldCount)].Name := Entry.Value;      
End;

Function GrammarNode.UseNode(Node: GrammarNode): Boolean;
Var
  I:Integer;
Begin
  Result := True;

  If (Parent = Node) Then
    Exit;

  For I:=0 To Pred(FieldCount) do
  If (Fields[I].Node = Node) Then
    Exit;

  Result := False;
End;

Procedure GrammarNode.GenLookAhead(From:GrammarNode; Dest: Stream; Tabs:TERRAString);
Var
  I:Integer;
  Field:GrammarField;
  LookAhead:TERRAString;
Begin
  LookAhead := Self.Fields[Self.LookAheadIndex].Name;

  Dest.WriteLine(Tabs+'LookAheadToken := Self.CurrentToken;');
  Dest.WriteLine(Tabs+ Self.GenParseNode(LookAhead, LookAhead, 'True'));
  Dest.WriteLine(Tabs+'If '+ LookAhead+' = Nil Then');
  Dest.WriteLine(Tabs+'Begin');
  Dest.WriteLine(Tabs+#9+'Self.CurrentToken := LookAheadToken;');
  Dest.WriteLine(Tabs+#9+'Exit;');
  Dest.WriteLine(Tabs+'End;');

  For I:=Succ(LookAheadIndex) To Pred(From.ChildrenCount) Do
  Begin
    Field := Self.Fields[I];

    Dest.WriteLine(Tabs+ Self.GenParseNode(Field.Name, Field.Node.Name, 'True'));
    Dest.WriteLine(Tabs+'If Assigned('+ Field.Name+') Then');
    Dest.WriteLine(Tabs+'Begin');
    Dest.WriteLine(#9+Tabs+'Result := '+Self.Name+'Node.Create(Result, '+LookAhead + ', '+Field.Name+');');
    Dest.WriteLine(#9+Tabs+'Exit;');
    Dest.WriteLine(Tabs+'End;');
    Break;
  End;
End;

Function GrammarNode.GenNextToken(const OptValue: TERRAString; Const Token:GrammarToken): TERRAString;
Begin
  Result := 'If (Not NextToken().ExpectToken(HandlerNode, '+OptValue+', '+CardinalToString(Token.ID)+')) Then';
End;


Function GrammarNode.GenParseNode(const Store, NodeKind, IsOpt: TERRAString): TERRAString;
Var
  Handler:TERRAString;
Begin
  If Self.Index = 0 Then
    Handler := Self.Name+'Node'
  Else
    Handler := 'HandlerNode';
    
  Result := Store + ' := Self.Parse'+NodeKind+'('+Handler+', '+IsOpt+');';
End;

Procedure GrammarNode.GenAbstractNode(Dest: Stream; AllowOpt:Boolean; Const IsOpt:TERRAString);
Var
  OptValue, NodeName, S, S2:TERRAString;
  J, K:Integer;
  Temp, LookAhead:GrammarNode;
Begin
  Dest.WriteLine('Var');
  Dest.WriteLine(#9+'TempToken:LexerToken;');

  For J:=0 To Pred(Self.ChildrenCount) Do
  Begin
    If (Self.ExpectedChildrenCount>0) Then
      NodeName := Self.ExpectedChildren[J]
    Else
      NodeName := Self.Children[J].Name;

    Temp := Self.Owner.GetNode(NodeName);
    If Temp.LookAheadIndex>=0 Then
    Begin
      Dest.WriteLine(#9+'LookAheadToken:LexerToken;');

      For K:=Temp.LookAheadIndex To Pred(Temp.FieldCount) Do
        Dest.WriteLine(#9+Temp.Fields[K].Name+':'+Temp.Fields[K].Entry.Value+'Node;');

      Break;
    End;
  End;

  Dest.WriteLine('Begin');

  If (Self.ChildrenCount<Self.ExpectedChildrenCount) Then
  Begin
    K := Self.ExpectedChildrenCount - Self.ChildrenCount;
    S := '';
    S2 := '';

    For J:=0 To Pred(Self.ExpectedChildrenCount) Do
    If Not Self.Owner.HasNode(Self.ExpectedChildren[J])  Then
    Begin
      If S<>'' Then
        S := S + ', ';
      S := S + Self.ExpectedChildren[J];
    End Else
    Begin
      Temp := Self.Owner.GetNode(Self.ExpectedChildren[J]);
      If Temp.Parent <> Self  Then
      Begin
        If S2<>'' Then
          S2 := S2 + ', ';
        S2 := S2 + Self.ExpectedChildren[J];
      End;
    End;

    If S2<>'' Then
      RaiseError('Invalid node: '+Self.Name+', '+IntToString(K)+' subnodes have multiple parents ('+S2+')')
    Else
    If S<>'' Then
      RaiseError('Incomplete node: '+Self.Name+', '+IntToString(K)+' subnodes are missing declarations ('+S+')');
  End Else
  If (Self.ChildrenCount<=0) Then
  Begin
    RaiseError('Undefined terminal node: '+Self.Name);
  End;

  LookAhead := Nil;

  S := '';
  For J:=0 To Pred(Self.ChildrenCount) Do
  Begin
    If (J<Pred(Self.ChildrenCount)) Then
      OptValue := 'True'
    Else
      OptValue := IsOpt;

    If (Self.ExpectedChildrenCount>0) Then
      NodeName := Self.ExpectedChildren[J]
    Else
      NodeName := Self.Children[J].Name;

    Temp := Self.Owner.GetNode(NodeName);
    If Temp.LookAheadIndex>=0 Then
    Begin
      LookAhead := Temp;
      Continue;
    End;

    Dest.WriteLine(#9+'TempToken := Self.CurrentToken;');
    Dest.WriteLine(#9+Self.GenParseNode('Result', NodeName, OptValue));

    If J<Pred(Self.ChildrenCount) Then
    Begin
      Dest.WriteLine(#9+'If Assigned(Result) Then');
      Dest.WriteLine(#9'Begin');

      If Assigned(LookAhead) Then
      Begin
        LookAhead.GenLookAhead(Self, Dest, #9#9);
      End Else
        Dest.WriteLine(#9#9+'Exit;');

        Dest.WriteLine(#9'End;');
        Dest.WriteLine(#9+'Self.CurrentToken := TempToken;');
      End;

      If (J<Pred(Self.ChildrenCount)) Then
        Dest.WriteLine();

      If S<>'' Then
        S := S + '/';
      S := S + NodeName;
    End;

    If AllowOpt Then
      Dest.WriteLine(#9'If (Result = Nil) And (Not IsOpt) Then')
    Else
      Dest.WriteLine(#9'If (Result = Nil) Then');

    Dest.WriteLine(#9#9'ParsingExceptedError('+Self.Name+'Node, '''+{S+}''');');

    If Assigned(LookAhead) Then
    Begin
      LookAhead.GenLookAhead(Self, Dest, #9);
    End;

    If Self.ChildrenCount<=0 Then
      Dest.WriteLine(#9'Result := Nil;');
End;

Procedure GrammarNode.GenCompoundNode(Dest: Stream; AllowOpt:Boolean; Const IsOpt:TERRAString);
Var
  J, K:Integer;
  S, CustomType, Conversion, OptValue, NodeKind, NodeName:TERRAString;
  HasLabels, FoundToken:Boolean;
Begin
  HasLabels := False;

{  If Self.Name = 'TypeAlias' Then
    IntToString(2);}

  For J:=0 To Pred(Self.FieldCount) Do
  Begin
    If (Self.Fields[J].Entry.Kind <>  grammar_Token) Or (Self.Fields[J].Entry.GenType<>'') Then
    Begin
      Dest.WriteLine('Var');
      Break;
    End;
  End;

  HasLabels := False;

  For J:=0 To Pred(Self.FieldCount) Do
  Begin
    If (Self.Fields[J].Entry.OptionalID>0) Or (Self.Fields[J].Entry.ListID>0) Then
      HasLabels := True;

    NodeName := Self.Fields[J].Name;

    Case Self.Fields[J].Entry.Kind Of 
        grammar_Token:
        If (Self.Fields[J].Entry.GenType<>'') Then
        Begin
          NodeName := Self.Fields[J].Entry.Annotation;
          NodeKind := Self.Fields[J].Entry.GenType;
        End Else
          Continue;

        grammar_Rule:
          Begin
            NodeKind := Self.Fields[J].Node.Name+'Node';

            If Self.Fields[J].Entry.ListID>0 Then
              Dest.WriteLine(#9+Self.Fields[J].Node.Name+'s :'+NodeKind+'Array;');
          End;

        grammar_RegExpr:
          Begin
            If Self.Fields[J].Entry.CustomType<>'' Then
              NodeKind := Self.Fields[J].Entry.CustomType
            Else
              NodeKind := 'TERRAString';
          End;
    End;

    Dest.WriteLine(#9+NodeName+' :'+NodeKind+';');
  End;

  If HasLabels Then
  Begin
    Dest.WriteLine(#9+'TempToken:LexerToken;');

    For J:=0 To Pred(Self.FieldCount) Do
    If (Self.Fields[J].Entry.ListID>0 )Then
    Begin
      Dest.WriteLine(#9+'ListFinished:Boolean;');
      Break;
    End;

        {S := '';
        For J:=0 To Pred(Self.FieldCount) Do
        If (Self.Fields[J].Entry.OptionalID>0 ) And (Not Self.Fields[J].Entry.List )Then
        Begin
          If Self.Fields[J].Entry.Kind =  grammar_Token Then
            Continue;

          If S<>'' Then
            S := S + ',';
          S := S + 'EndOpt'+IntToString(Self.Fields[J].Entry.OptionalID);
        End;
        If S<>'' Then
          Dest.WriteLine('Label '+S+';');}
  End;

  Dest.WriteLine('Begin');
    //Dest.WriteLine('WriteLn(''Parsing '+Self.Name+' '');');

  FoundToken := False;
  Dest.WriteLine(#9'Result := Nil;');

  S := '';
  For J:=0 To Pred(Self.FieldCount) Do
  Begin
    Case Self.Fields[J].Entry.Kind Of
        grammar_Rule: NodeKind := 'Node';
        grammar_Token: NodeKind := 'Token';
        grammar_RegExpr: NodeKind := 'Token';
    End;

    If (J>0) And (Self.Fields[Pred(J)].Entry.OptionalID>0) And (Self.Fields[Pred(J)].Entry.OptionalID<>Self.Fields[J].Entry.OptionalID)
    And (Self.Fields[Pred(J)].Entry.ListID<=0) Then
    Begin
      Dest.WriteLine(#9+'Until True;');
      Dest.WriteLine();
        //Dest.WriteLine(#9+'EndOpt'+IntToString(Self.Fields[Pred(J)].Entry.OptionalID)+':');
    End;

    If (Self.Fields[J].Entry.ListID>0) And ((J=0) Or (Self.Fields[Pred(J)].Entry.ListID<=0)) Then
    Begin
      Dest.WriteLine(#9+'TempToken := Self.CurrentToken;');
      Dest.WriteLine(#9+'ListFinished := False;');
      Dest.WriteLine(#9+'Repeat');
    End Else
    If (J>0) And (Self.Fields[J].Entry.OptionalID>0) And (Self.Fields[Pred(J)].Entry.OptionalID<>Self.Fields[J].Entry.OptionalID) Then
    Begin
      Dest.WriteLine(#9+'TempToken := Self.CurrentToken;');
      Dest.WriteLine(#9+'Repeat');
    End;

    If (Self.Fields[J].Entry.Kind = grammar_Token) Then
    Begin
      If (Self.Fields[J].Entry.ListID>0) Or (Self.Fields[J].Entry.OptionalID>0) Then
        OptValue := 'True'
      Else
      If (FoundToken) Then
        OptValue := 'False'
      Else
        OptValue := 'IsOpt';

      If (Self.Fields[J].Entry.GenType<>'') Then
      Begin
        Dest.WriteLine(#9+Self.Fields[J].Entry.Annotation+' := False;');
      End;

      If Self.Fields[J].Entry.ListID>0 Then
      Begin

        Dest.WriteLine(#9+'TempToken := Self.CurrentToken;');
        Dest.WriteLine(#9+Self.GenNextToken(OptValue, Self.Fields[J].Entry.Token));
        Dest.WriteLine(#9+'Begin');
        Dest.WriteLine(#9#9+'Self.CurrentToken := TempToken;');
        Dest.WriteLine(#9#9+'Break;');
        Dest.WriteLine(#9+'End;');
      End Else
      Begin
        Dest.WriteLine(#9+Self.GenNextToken(OptValue, Self.Fields[J].Entry.Token));
        If (Self.Fields[J].Entry.OptionalID>0) Then
        Begin
          Dest.WriteLine(#9#9+'Begin');
          For K:=Succ(J) To Pred(Self.FieldCount) Do
          If Assigned(Self.Fields[K].Node) Then
            Dest.WriteLine(#9#9#9+Self.Fields[K].Name +' := Nil;');
          Dest.WriteLine(#9#9#9+'Self.CurrentToken := TempToken;');
          Dest.WriteLine(#9#9#9+'Break;');
          Dest.WriteLine(#9#9+'End;');
        End Else
          Dest.WriteLine(#9#9#9+'Exit;');
      End;

      If (Self.Fields[J].Entry.GenType<>'') Then
      Begin
        Dest.WriteLine(#9+Self.Fields[J].Entry.Annotation+' := True;');
      End;

      Dest.WriteLine();

      If (Self.Fields[J].Entry.ListID>0) Then
      Begin
        If (J>=Pred(Self.FieldCount)) Or (Self.Fields[Succ(J)].Entry.ListID<=0) Then
          Dest.WriteLine(#9+'Until (ListFinished);');
      End;
    End Else
    If (Self.Fields[J].Entry.Kind = grammar_Rule) Then
    Begin
        {If Self.Fields[J].Node = Nil Then
          IntToString(2);}

      If (Self.Fields[J].Entry.OptionalID>0) Then
        OptValue := 'True'
      Else
      If (Self.Fields[J].Entry.ListID>0) Then
        OptValue := 'True'
      Else
      If FoundToken Then
        OptValue := 'False'
      Else
        OptValue := IsOpt;

      If Self.Fields[J].Entry.ListID>0 Then
      Begin
        Dest.WriteLine(#9+Self.GenParseNode(Self.Fields[J].Name, Self.Fields[J].Node.Name, OptValue));
        Dest.WriteLine(#9+'If Assigned('+Self.Fields[J].Name+') Then');
        Dest.WriteLine(#9+'Begin');
        Dest.WriteLine(#9#9+'SetLength('+Self.Fields[J].Name+'s, Succ(Length('+Self.Fields[J].Name+'s)));');
        Dest.WriteLine(#9#9+Self.Fields[J].Name+'s[Pred(Length('+Self.Fields[J].Name+'s))] := '+Self.Fields[J].Name+';');
        Dest.WriteLine(#9#9+'TempToken := Self.CurrentToken;');
        Dest.WriteLine(#9+'End Else');
        Dest.WriteLine(#9+'Begin');
        Dest.WriteLine(#9#9+'ListFinished := True;');
        Dest.WriteLine(#9#9+'Self.CurrentToken := TempToken;');
        Dest.WriteLine(#9+'End;');


        If (J>=Pred(Self.FieldCount)) Or (Self.Fields[Succ(J)].Entry.ListID<=0) Then
          Dest.WriteLine(#9+'Until (ListFinished);');
      End Else
      Begin
        If Self.Fields[J].Entry.Kind = grammar_Rule Then
          NodeName := Self.Fields[J].Node.Name
        Else
          NodeName := Self.Fields[J].Name;

        If (Self.Fields[J].Entry.Null) Then
        Begin
          Dest.WriteLine(#9+'If (Parse'+NodeName+'('+OptValue+')= Nil) Then');
        End Else
        Begin
          Dest.WriteLine(#9+Self.GenParseNode(Self.Fields[J].Name, NodeName, OptValue));
          Dest.WriteLine(#9+'If '+Self.Fields[J].Name+'= Nil Then');
        End;

        If Self.Fields[J].Entry.OptionalID>0 Then
        Begin
          Dest.WriteLine(#9+'Begin');
          Dest.WriteLine(#9#9+'Self.CurrentToken := TempToken;');
          //Dest.WriteLine(#9#9+'Goto EndOpt'+IntToString(Self.Fields[J].Entry.OptionalID)+';');
          Dest.WriteLine(#9#9+'Break;');
          Dest.WriteLine(#9+'End;');
        End Else
          Dest.WriteLine(#9#9+'Exit;');
      End;

      Dest.WriteLine();
    End Else
    If (Self.Fields[J].Entry.Kind = grammar_RegExpr) Then
    Begin
        {If Self.Fields[J].Node = Nil Then
            IntToString(2);}

      Dest.WriteLine(#9+ Self.GenNextToken('IsOpt', Self.Fields[J].Entry.Token));
      Dest.WriteLine(#9#9+'Exit;');

      CustomType := Self.Fields[J].Entry.CustomType;
      If (StringEquals(CustomType, 'Double')) Or (StringEquals(CustomType, 'Single'))Then
        Conversion := 'StringToFloat'
      Else
      If StringEquals(CustomType, 'Boolean') Then
        Conversion := 'StringToBool'
      Else
      If StringEquals(CustomType, 'Cardinal') Then
        Conversion := 'StringToCardinal'
      Else
      If StringEquals(CustomType, 'Integer') Then
        Conversion := 'StringToInt'
      Else
        Conversion := '';

      If (Conversion<>'') Then
        Dest.WriteLine(#9+Self.Fields[J].Name+' := '+Conversion+'(TokenValue());')
      Else
        Dest.WriteLine(#9+Self.Fields[J].Name+' := TokenValue();');
    End Else
      Continue;

    If (Not FoundToken) And (
      ((J>=Self.ImportantIndex) And (Self.ImportantIndex>=0))
      Or ((Self.ImportantIndex<0) And (Self.Fields[J].Entry.Kind = grammar_Token))) Then
    Begin
      FoundToken := True;

      If (J<Pred(Self.FieldCount)) Then
        Dest.WriteLine(#9+'HandlerNode := '+ Self.Name +'Node;');
    End;

    If (Self.Fields[J].Entry.Kind = grammar_Token) And (Self.Fields[J].Entry.GenType='') Then
      Continue;


    If (Not Self.Fields[J].Entry.Null) Then
    Begin
      If S<>'' Then
        S := S + ', ';

      If (Self.Fields[J].Entry.GenType<>'') Then
        S := S + Self.Fields[J].Entry.Annotation
      Else
      Begin
        S := S + Self.Fields[J].Name;
        If Self.Fields[J].Entry.ListID>0 Then
          S := S + 's';
      End;
    End;

  End;

  If (Self.Fields[Pred(Self.FieldCount)].Entry.OptionalID>0)
  And (Self.Fields[Pred(Self.FieldCount)].Entry.ListID<=0) Then
  Begin
    Dest.WriteLine(#9+'Until True;');
    Dest.WriteLine();
      //Dest.WriteLine(#9+'EndOpt'+IntToString(Self.Fields[Pred(Self.FieldCount)].Entry.OptionalID)+':');
  End;

  Dest.WriteLine(#9+'Result := '+Self.Name+'Node.Create('+S+');');
End;

Function GrammarNode.GenArgs: TERRAString;
Begin
  Result := 'HandlerNode:ASTNodeClass; IsOpt:Boolean';
End;

{ GrammarRuleLine }
Procedure GrammarRuleLine.AddEntry(const S: TERRAString; Kind: GrammarEntryKind; OptionalID:Integer);
Begin
  Inc(EntryCount);
  SetLength(EntryList, EntryCount);

  EntryList[Pred(EntryCount)].Kind := Kind;
  EntryList[Pred(EntryCount)].Value := S;
  EntryList[Pred(EntryCount)].OptionalID := OptionalID;
  EntryList[Pred(EntryCount)].Annotation := '';
End;


Function GrammarRuleLine.GetKind():GrammarRuleKind;
Var
  I:Integer;
Begin
  Result := rule_Unknown;
  If Self.EntryCount<=0 Then
    Exit;

  If (Self.EntryCount>1) Then
  Begin
    Result := rule_compound;
    Exit;
  End;

  Case self.EntryList[0].Kind Of
  grammar_Rule: Result := rule_abstract;
  grammar_RegExpr: Result := rule_RegExpr;
  grammar_Token: Result := rule_Token;
  End;
End;

{ GrammarRule }
Function GrammarRule.GetKind():GrammarRuleKind;
Var
  I:Integer;
Begin
  Result := rule_Unknown;

  If Self.LineCount<=0 Then
    Exit;

{  If (Self.Preprocessor) Then
  Begin
    Result := rule_Preprocessor;
    Exit;
  End;}

  If (Self.LineCount = 1) Then
  Begin
    Result := (self.Lines[0].GetKind());
    Exit;
  End;

  StringToInt(Name, False);

  For I:=1 To Pred(Self.LineCount) Do
  If (Lines[I].GetKind() <> Lines[0].GetKind()) Then
  Begin
    Exit;
  End;

  If (Lines[0].GetKind() = rule_RegExpr) Then
    Result := rule_RegExpr
  Else
  If (Lines[0].GetKind() = rule_token) Then
    Result := rule_set
  Else
    Result := rule_abstract;
End;

Procedure GrammarRule.NewLine;
Begin
  Inc(LineCount);
  SetLength(Lines, LineCount);
  Self.CurrentLine := GrammarRuleLine.Create();
  Lines[Pred(LineCount)] := Self.CurrentLine;
End;

{ Grammar }
Constructor Grammar.Create(S:String);
Var
  Src:Stream;
Begin
  Src := MemoryStream.Create(S);
  Code := '';
  While Not Src.EOF Do
  Begin
    Src.ReadLine(S);
    Code := Code + S;
    StringAppendChar(Code, NewLineChar);
  End;
  ReleaseObject(Src);

  LineNumber := 1;
  RuleCount := 0;
  Try
    Expect('grammar');
    Self.Name := ExpectID();
    Expect(';');

    Expect('rules');
    While ProcessRules() Do
    Begin
    End;

  Except
    Halt;
  End;
End;

Function Grammar.AllocToken(Const Name:TERRAString): Cardinal;
Begin
  Result := LastTokenID;
  Inc(LastTokenID);
  SetLength(TokenNames, LastTokenID);
  TokenNames[Result] := Name;
End;

Function Grammar.GetToken(Value, Name: TERRAString; ID:Integer; RegExpr, Discard:Boolean):GrammarToken;
Var
  I:Integer;
Begin
  If (Value<>'') And (Not RegExpr) Then
  For I:=0 To Pred(TokenCount) Do
  If (StringEquals(Tokens[I].Value, Value)) Then
  Begin
    Result := Tokens[I];
    Exit;
  End;

  Inc(TokenCount);

  If Name = '' Then
    Name := IntToString(TokenCount);

  If ID<0 Then
    ID := Self.AllocToken(Value);

  If (RegExpr) Then
  Begin
    If (Not StringContains('[', Value)) And (Not StringContains(']', Value)) Then
      RegExpr := False;
  End;

  SetLength(Tokens, TokenCount);
  Result := GrammarToken.Create();
  Result.ID := ID;
  Result.Value := Value;
  Result.Name := Name;
  Result.RegExpr := RegExpr;
  Result.Discard := Discard;
  Tokens[Pred(TokenCount)] := Result;
End;

Function Grammar.ProcessRules():Boolean;
Var
  I ,N, TempLine:Integer;
  S, S2:TERRAString;
  Rule:GrammarRule;
  OptionalID, LastOptionalID:Integer;
  CurrentListID:Integer;
Begin
  S := NextToken();
  If (StringEquals(S, 'end')) Then
  Begin
    Expect('.');
    Result := False;
    Exit;
  End Else
    RevertToken();

  Inc(RuleCount);
  SetLength(Rules, RuleCount);

  Rule := GrammarRule.Create();

  Rules[Pred(RuleCount)] := Rule;

  Rule.LineCount := 0;
  S := ExpectID();

  For I:=0 To Pred(RuleCount) Do
  If StringEquals(Rules[I].Name, S) Then
  Begin
    RaiseError('Duplicated rule: '+S);
    Exit;
  End;

  Rule.Name := S;
  Rule.CurrentLine := Nil;

  Expect('=');

  OptionalID := 0;
  LastOptionalID := 0;
  CurrentListID := 0;
  TempLine := LineNumber;

  Repeat
    S := NextToken();

    If (TempLine<>LineNumber) Then
      RaiseError('Expected ; at line '+ IntToString(TempLine));

    If S = ';' Then
      Break;

    If S = '@' Then
    Begin
      S := ExpectID();
      Rule.CurrentLine.EntryList[Pred(Rule.CurrentLine.EntryCount)].Annotation := S;
      Continue;
    End;

    If S = '%' Then
    Begin
      Rule.CurrentLine.EntryList[Pred(Rule.CurrentLine.EntryCount)].Null := True;
      Continue;
    End;

    If S = ',' Then
    Begin
      Rule.NewLine();
      Continue;
    End;

    If S = '=' Then
    Begin
      Expect('>');
      S := ExpectID();

      If StringEquals(S, 'null') Then
      Begin
        For I:=0 To Pred(Rule.CurrentLine.EntryCount) Do
          Rule.CurrentLine.EntryList[I].Discard := True;
      End Else
      Begin
        For I:=0 To Pred(Rule.CurrentLine.EntryCount) Do
          Rule.CurrentLine.EntryList[I].CustomType := S;
      End;

      Expect(';');
      Break;
    End;

(*    If S = '-' Then
    Begin
      Expect('>');
      Expect('{');
      Expect('}');
      Expect(';');
      Rule.Preprocessor := True;
      Break;
    End;*)

    If S = '[' Then
    Begin
      Inc(LastOptionalID);
      OptionalID := LastOptionalID;
      Continue;
    End;

    If S = ']' Then
    Begin
      OptionalID := 0;
      Continue;
    End;

    If (Rule.CurrentLine = Nil)  Then
    Begin
      Rule.NewLine();
      OptionalID := 0;
    End;

    If (StringBeginsWithChar(Ord(':'), S,  False)) Then
    Begin
      S := NextToken();
      Rule.CurrentLine.EntryList[Pred(Rule.CurrentLine.EntryCount)].Annotation := S;
      Rule.CurrentLine.EntryList[Pred(Rule.CurrentLine.EntryCount)].GenType := 'Boolean';
    End Else
    If (StringBeginsWithChar(Ord('?'), S,  False)) Then
    Begin
      Rule.CurrentLine.EntryList[Pred(Rule.CurrentLine.EntryCount)].IsLookAhead := True;
    End Else
    If (StringBeginsWithChar(Ord('!'), S,  False)) Then
    Begin
      Rule.CurrentLine.EntryList[Pred(Rule.CurrentLine.EntryCount)].IsImportant := True;
    End Else
    If (StringBeginsWithChar(Ord('"'), S,  False)) Then
    Begin
      S := StringCopy(S, 2, StringLength(S)- 2);
      Rule.CurrentLine.AddEntry(S, grammar_RegExpr, OptionalID);
    End Else
    If (StringBeginsWithChar(Ord('('), S,  False)) Then
    Begin
      Inc(CurrentListID);
      S := NextToken();
      Rule.CurrentLine.AddEntry(S, grammar_Rule, OptionalID);
      Rule.CurrentLine.EntryList[Pred(Rule.CurrentLine.EntryCount)].ListID := CurrentListID;

      S := NextToken();
      If (S <> ')') Then
      Begin
        S := StringCopy(S, 2, StringLength(S)- 2);
        //Rule.CurrentLine.EntryList[Pred(Rule.CurrentLine.EntryCount)].ListSep := S;
        Rule.CurrentLine.AddEntry(S, grammar_Token, OptionalID);
        Rule.CurrentLine.EntryList[Pred(Rule.CurrentLine.EntryCount)].ListID := CurrentListID;
        Expect(')');
      End;

    End Else
    If (StringBeginsWithChar(Ord(''''), S,  False)) Then
    Begin
      S := StringCopy(S, 2, StringLength(S)- 2);
      Rule.CurrentLine.AddEntry(S, grammar_Token, OptionalID);
    End Else
    Begin
      Rule.CurrentLine.AddEntry(S, grammar_Rule, OptionalID);
    End;

  Until False;

  Result := True;
End;

Procedure Grammar.GenerateTokenInterface(Dest:Stream);
Var
  I,J, K:Integer;
  TokenName, S, S2, S3:TERRAString;
  TokenID:Integer;
Begin
  TokenCount := 0;
  Self.GetToken('', 'Empty', -1, False, False); // null token

  For I:=0 To Pred(RuleCount) Do
  Begin
    If (Rules[I].GetKind = rule_RegExpr) Then
      TokenID := Self.AllocToken(Rules[I].Name)
    Else
      TokenID := -1;

    For J:=0 To Pred(Rules[I].LineCount) Do
    Begin
      For K:=0 To Pred(Rules[I].Lines[J].EntryCount) Do
      Begin
        TokenName := Rules[I].Lines[J].EntryList[K].Annotation;
        If TokenName = '' Then
        Begin
          TokenName := Rules[I].Name;
          If (Rules[I].LineCount>1) Then
            TokenName := TokenName + IntToString(Succ(J));
        End;

        If (Rules[I].Lines[J].EntryList[K].Kind = grammar_Token) Then
        Begin
          Rules[I].Lines[J].EntryList[K].Token := Self.GetToken(Rules[I].Lines[J].EntryList[K].Value, TokenName, -1, False, Rules[I].Lines[J].EntryList[K].Discard);
        End Else
        If (Rules[I].Lines[J].EntryList[K].Kind = grammar_RegExpr) Then
        Begin
          Rules[I].Lines[J].EntryList[K].Token := Self.GetToken(Rules[I].Lines[J].EntryList[K].Value, TokenName, TokenID, True, Rules[I].Lines[J].EntryList[K].Discard);
        End;
      End;
    End;
  End;

{  For I:=0 To Pred(RuleCount) Do
    If (Rules[I].Lines[0].EntryList[0].Kind = grammar_RegExpr) Then
      Begin
        Rules[I].Lines[0].EntryList[0].ID := Self.GetTokenID(Rules[I].Name);
      End;}

  For I:=1 To Pred(TokenCount) Do
  If Tokens[I].RegExpr Then
  Begin
    S2 := 'RegexTokenMatcher';

    S := Tokens[I].Name+'RegexMatcher = Class('+S2+')';

    S3 := Tokens[I].Value;
    StringReplaceText(StringFromChar(NewLineChar), '\n', S3);
    StringReplaceText(StringFromChar(9), '\t', S3);

    S := S + ' // matcher for ' + S3;
    Dest.WriteLine(#9+S);

    S := 'Function Match(Const C:TERRAChar):Boolean; Override;';
    Dest.WriteLine(#9#9+S);

    Dest.WriteLine(#9+'End;');
    Dest.WriteLine();
  End;

  Dest.WriteLine();
End;



Function Grammar.HasNode(const Value: TERRAString):Boolean;
Var
  I, J:Integer;
Begin
  For I:=0 To Pred(NodeCount) Do
  If (StringEquals(Nodes[I].Name, Value)) Then
  Begin
    Result := True;
    Exit;
  End;

  Result := False;
End;

Function Grammar.GetNode(const Value: TERRAString): GrammarNode;
Var
  I, J:Integer;
Begin
  For I:=0 To Pred(NodeCount) Do
  If (StringEquals(Nodes[I].Name, Value)) Then
  Begin
    Result := Nodes[I];
    Exit;
  End;

  Inc(NodeCount);
  SetLength(Nodes, NodeCount);
  Result := GrammarNode.Create(Pred(NodeCount), Value, Self);
  Nodes[Pred(NodeCount)] := Result;
End;

Function Grammar.GetParent(const Value: TERRAString): GrammarNode;
Var
  I, J, K:Integer;
Begin
  For I:=0 To Pred(RuleCount) Do
  If (Rules[I].GetKind() = rule_abstract) Then
  Begin
    For J:=0 To Pred(Rules[I].LineCount) Do
      For K:=0 To Pred(Rules[I].Lines[J].EntryCount) Do
      If (StringEquals(Rules[I].Lines[J].EntryList[K].Value, Value)) Then
      Begin
        Result := GetNode(Rules[I].Name);
        Exit;
      End;
  End;

  Result := Nil;
End;

{Function Grammar.FindSet(const Value: TERRAString): GrammarSet;
Var
  I:Integer;
Begin
  For I:=0 To Pred(SetCount) Do
  If (StringEquals(Sets[I].Name, Value)) Then
  Begin
    Result := Sets[I];
    Exit;
  End;

  Result :=  Nil;
End;

Function Grammar.GetSet(const Value: TERRAString): GrammarSet;
Var
  I:Integer;
Begin
  Result := FindSet(Value);
  If Assigned(Result) Then
    Exit;

  Inc(SetCount);
  SetLength(Sets, SetCount);
  Result := GrammarSet.Create(Value, Self);
  Sets[Pred(SetCount)] := Result;
End;}

Function Grammar.OrderNode(Index: Integer):Boolean;
Var
  I, Min:Integer;
  Temp:GrammarNode;
Begin
  Min := -1;

  For I:=0 To Pred(Index) Do
  If (Nodes[I].UseNode(Nodes[Index])) Then
  Begin
    Min := I;
    Break;
  End;

  If Min<0 Then
  Begin
    Result := False;
    Exit;
  End;


  Temp := Nodes[Index];

//  WriteLn('Moving ',Temp.Name + ' to pos ',Min);

  For I:=Index DownTo Succ(Min) Do
    Nodes[I] := Nodes[Pred(I)];

  Nodes[Min] := Temp;

  Result := True;
End;

Procedure Grammar.GenerateNodesInterface(Dest:Stream);
Var
  I,J, K:Integer;
  S, S2, S3, Prefix, Parent:TERRAString;
  Node, PreNode, Other:GrammarNode;
  HasRegExpr:Boolean;
//  MySet:GrammarSet;
Begin
  NodeCount := 0;

  For I:=0 To Pred(RuleCount) Do
  Begin
{    If Rules[I].Name='BinaryExpression' Then
      IntToString(2);}

    Case Rules[I].GetKind() Of
    rule_abstract:
      Begin
        Node := GetNode(Rules[I].Name);
        Node.ExpectedChildrenCount := Rules[I].LineCount;

        SetLength(Node.ExpectedChildren, Node.ExpectedChildrenCount);
        K:=0;
        For J:=0 To Pred(Rules[I].LineCount) Do
        Begin
          Node.ExpectedChildren[J] := Rules[I].Lines[J].EntryList[K].Value;
        End;
      End;

    rule_RegExpr:
      Begin
        Node := GetNode(Rules[I].Name);
        J:=0;
        For K:=0 To Pred(Rules[I].Lines[J].EntryCount) Do
        Begin
          If Rules[I].Lines[J].EntryList[K].Annotation = '' Then
            Rules[I].Lines[J].EntryList[K].Annotation := 'Value';
          Node.AddField(Rules[I].Lines[J].EntryList[K]);
        End;
      End;

    rule_compound:
      Begin
        Node := GetNode(Rules[I].Name);
        J:=0;

        For K:=0 To Pred(Rules[I].Lines[J].EntryCount) Do
        Begin
          Node.AddField(Rules[I].Lines[J].EntryList[K]);

          If Rules[I].Lines[J].EntryList[K].IsLookAhead Then
            Node.LookAheadIndex := K;

          If Rules[I].Lines[J].EntryList[K].IsImportant Then
            Node.ImportantIndex := K;
        End;
      End;

    rule_Set:
      Begin
        PreNode := GetNode(Rules[I].Name);
        For J:=0 To Pred(Rules[I].LineCount) Do
        Begin
          S := Rules[I].Name + Rules[I].Lines[J].EntryList[0].Annotation;
          Node := GetNode(S);
          Node.Parent := PreNode;
          Node.AddField(Rules[I].Lines[J].EntryList[0]);
          //MySet.AddValue(Rules[I].Lines[J].EntryList[0].Value, Rules[I].Lines[J].EntryList[0].Annotation);}
        End;

       { MySet := GetSet(Rules[I].Name);
        For J:=0 To Pred(Rules[I].LineCount) Do
          MySet.AddValue(Rules[I].Lines[J].EntryList[0].Value, Rules[I].Lines[J].EntryList[0].Annotation);}
      End;

    End;
  End;
  
  For I:=0 To Pred(NodeCount) Do
  Begin
    If (Nodes[I].Parent = Nodes[I]) Then
      Nodes[I].Parent := Nil;

    If (Nodes[I].Parent = Nil) Then
      Nodes[I].Parent := GetParent(Nodes[I].Name);
  End;

  I:=0;
  While I<NodeCount Do
  If OrderNode(I) Then
    I := 0
  Else
    Inc(I);

{  For I:=0 To Pred(SetCount) Do
  Begin
    Dest.WriteLine(#9+Sets[I].Name+'Type = (');

    For J:=0 To Pred(Sets[I].ValueCount) Do
    Begin
      S := Sets[I].Name+Sets[I].Values[J];
      If J<Pred(Sets[I].ValueCount) Then
        S := S + ',';
      Dest.WriteLine(#9#9+S);
    End;

    Dest.WriteLine(#9+');');
    Dest.WriteLine();
  End;}

  For I:=0 To Pred(NodeCount) Do
  Begin
    If Assigned(Nodes[I].Parent) Then
      Parent := Nodes[I].Parent.Name+'Node'
    Else
      Parent := 'ASTNode';

    If (Nodes[I].FieldCount>0) Then
    Begin
      For J:=0 To Pred(Nodes[I].FieldCount) Do
      If (Nodes[I].Fields[J].Entry.Kind = grammar_Rule) And (Nodes[I].Fields[J].Entry.ListID>0) Then
      Begin
        S := Nodes[I].Fields[J].Entry.Value+'Node';

        Other := Self.GetNode(Nodes[I].Fields[J].Entry.Value);
        If (Assigned(Other)) And (Not Other.Declared) Then
        Begin
          Dest.WriteLine(#9+S+' = Class;');
        End;

        If (Assigned(Other)) And (Not Other.ArrayDeclared) Then
        Begin
          Dest.WriteLine(#9+S+'Array = Array Of '+S+';');
          Other.ArrayDeclared := True;
        End;

        Dest.WriteLine();
      End;
    End;

    Dest.WriteLine(#9+Nodes[I].Name+'Node = Class('+Parent+')');

    HasRegExpr := False;

    Nodes[I].Declared := True;

    If (Nodes[I].FieldCount>0) Then
    Begin
      Dest.WriteLine(#9#9+'Public');
      S3 := '';
      For J:=0 To Pred(Nodes[I].FieldCount) Do
      If Not Nodes[I].Fields[J].Entry.Null Then
      Begin
        S2 := Nodes[I].Fields[J].Name;

        If Nodes[I].Fields[J].Entry.Kind = grammar_Rule Then
        Begin
          S := Nodes[I].Fields[J].Entry.Value+'Node';

          If Nodes[I].Fields[J].Entry.ListID>0 Then
          Begin
            S2 := S2 +'s';
            S := S +'Array'//;'Array Of '+S;
          End;

          Prefix := '';
        End Else
        If Nodes[I].Fields[J].Entry.Kind = grammar_RegExpr Then
        Begin
          If Nodes[I].Fields[J].Entry.CustomType<>'' Then
            S := Nodes[I].Fields[J].Entry.CustomType
          Else
            S := 'TERRAString';

          Prefix := 'Const ';
          HasRegExpr := True;
        End Else
        If (Nodes[I].Fields[J].Entry.Kind = grammar_Token) And (Nodes[I].Fields[J].Entry.GenType<>'') Then
        Begin
          S2 := Nodes[I].Fields[J].Entry.Annotation;
          S := Nodes[I].Fields[J].Entry.GenType;
          Prefix := 'Const ';
        End Else
          Continue;

        Dest.WriteLine(#9#9#9+S2+': '+S+';');

        {If Nodes[I].Fields[J].Entry.List Then
        Begin
          Dest.WriteLine(#9#9#9+Nodes[I].Fields[J].Name+'Count:Integer;');
        End;}

        If S3<>'' Then
          S3 := S3 + '; ';

        S3 := S3 + Prefix + S2+':'+S;
      End;

{      If S3<>'' Then
        S3 := '; ' + S3;}

      Dest.WriteLine();
      Dest.WriteLine(#9#9#9+'Constructor Create('+S3+');');

      If HasRegExpr Then
      Begin
        Dest.WriteLine(#9#9#9+'Function GetValue():TERRAString; Override;');
      End;
    End;

    Dest.WriteLine(#9+'End;');
    Dest.WriteLine();
  End;


  Dest.WriteLine();
End;

procedure Grammar.GenerateNodesImplementation(Dest: Stream);
Var
  I,J:Integer;
  Conversion, CustomType, S, S2,S3,S4, S5:TERRAString;
begin
  For I:=0 To Pred(NodeCount) Do
  If (Nodes[I].FieldCount>0) Then
  Begin
    S := '';
    For J:=0 To Pred(Nodes[I].FieldCount) Do
    If (Not Nodes[I].Fields[J].Entry.Null) Then
    Begin
      S5 := Nodes[I].Fields[J].Name;
      S3 := '';

      Case Nodes[I].Fields[J].Entry.Kind Of
        grammar_Token:
        If (Nodes[I].Fields[J].Entry.GenType<>'') Then
        Begin
          S5 := Nodes[I].Fields[J].Entry.Annotation;
          S2 := Nodes[I].Fields[J].Entry.GenType;
          S3 := 'Const ';
        End Else
          Continue;

        grammar_Rule:
          Begin
            S2 := Nodes[I].Fields[J].Node.Name+  'Node';
            If Nodes[I].Fields[J].Entry.ListID>0 Then
            Begin
              S5 := S5 +'s';
              S2 := S2+'Array';
            End;
          End;

        grammar_RegExpr:
          Begin
            If Nodes[I].Fields[J].Entry.CustomType<>'' Then
              S2 := Nodes[I].Fields[J].Entry.CustomType
            Else
              S2 := 'TERRAString';

            S3 := 'Const ';
          End;
      End;

      If S<>'' Then
        S := S + '; ';

      S := S + S3 + S5+':'+S2;
    End;

{    If I>0 Then
    Begin
      If S<>'' Then
        S := '; '+S;
      S := 'Parent:ASTNode'+S;
    End;}

    Dest.WriteLine('Constructor '+Nodes[I].Name+'Node.Create('+S+');');
    For J:=0 To Pred(Nodes[I].FieldCount) Do
      If Nodes[I].Fields[J].Entry.ListID>0 Then
      Begin
        Dest.WriteLine('Var');
        Dest.WriteLine(#9'I:Integer;');
        Break;
      End;

    Dest.WriteLine('Begin');
    For J:=0 To Pred(Nodes[I].FieldCount) Do
    If (Nodes[I].Fields[J].Entry.Kind <> grammar_Token) And (Not Nodes[I].Fields[J].Entry.Null) Then
    Begin
      S5 := Nodes[I].Fields[J].Name;
      S4 := Nodes[I].Fields[J].Name;

      If Nodes[I].Fields[J].Entry.ListID>0 Then
      Begin
        S4 := S4 + 's';
        S5 := S5 + 's';
      End;

      Dest.WriteLine(#9+'Self.'+S5+' := '+S4+';');

      If Nodes[I].Fields[J].Entry.Kind<>Grammar_Rule Then
        Continue;

      Dest.WriteLine(#9+'If Assigned('+S5+') Then');
      If Nodes[I].Fields[J].Entry.ListID>0 Then
      Begin
        Dest.WriteLine(#9#9+'For I:=0 To Pred(Length('+S5+')) Do');
        Dest.WriteLine(#9#9#9+S5+'[I].SetParent(Self);');
      End Else
        Dest.WriteLine(#9#9+S5+'.SetParent(Self);');

    End;
    Dest.WriteLine('End;');
    Dest.WriteLine();

    For J:=0 To Pred(Nodes[I].FieldCount) Do
    If Nodes[I].Fields[J].Entry.Kind = grammar_regExpr Then
    Begin
      Dest.WriteLine('Function '+Nodes[I].Name+'Node.GetValue():TERRAString;');
      Dest.WriteLine('Begin');

      CustomType := Nodes[I].Fields[J].Entry.CustomType;
      If (StringEquals(CustomType, 'Double')) Or (StringEquals(CustomType, 'Single'))Then
        Conversion := 'FloatToString'
      Else
      If (StringEquals(CustomType, 'Boolean')) Then
        Conversion := 'BoolToString'
      Else
      If (StringEquals(CustomType, 'Cardinal')) Then
        Conversion := 'CardinalToString'
      Else
      If (StringEquals(CustomType, 'Integer')) Then
        Conversion := 'IntToString'
      Else
        Conversion := '';

      If Conversion<>'' Then
        Dest.WriteLine(#9+'Result := '+Conversion+'(Value);')
      Else
        Dest.WriteLine(#9+'Result := Value;');
      Dest.WriteLine('End;');

      Dest.WriteLine();
      Break;
    End;
  End;
end;

Procedure Grammar.GenerateParserInterface(Dest: Stream);
Var
  I, J:Integer;
  S2, S3:TERRAString;
Begin
  Dest.WriteLine(#9+Self.Name+'Parser = Class(Parser)');
  Dest.WriteLine(#9#9+'Protected');
  For I:=0 To Pred(NodeCount) Do
  Begin
    Nodes[I].ChildrenCount := 0;

    For J:=0 To Pred(NodeCount) Do
    If (Nodes[J].Parent = Nodes[I]) Then
    Begin
      Inc(Nodes[I].ChildrenCount);
      SetLength(Nodes[I].Children, Nodes[I].ChildrenCount);
      Nodes[I].Children[Pred(Nodes[I].ChildrenCount)] := Nodes[J];
    End;

    If Nodes[I].LookAheadIndex>=0 Then
      Continue;

    If I=0 Then
    Begin
      S2 := 'ASTNode; Override;';
    End Else
    Begin
      S2 := Nodes[I].Name+'Node;';
    End;

    If I>0 Then
      S3 := Nodes[I].GenArgs()
    Else
      S3 := '';

    Dest.WriteLine(#9#9#9+'Function Parse'+Nodes[I].Name+'('+S3+'):'+S2);
  End;

  Dest.WriteLine(#9#9#9+'Function GetTokenName(ID:Cardinal; Const Value:TERRAString):TERRAString; Override;');

  Dest.WriteLine(#9#9+'Public');
  //Dest.WriteLine(#9#9#9+'Constructor Create();');
  Dest.WriteLine(#9#9#9+'Function Parse(Source:Stream; IgnoreCase:Boolean):ASTNode; Override;');

  Dest.WriteLine(#9+'End;');

  Dest.WriteLine();
End;

Procedure Grammar.GenerateParserImplementation(Dest: Stream);
Var
  I, J, K:Integer;
  S, OptArg, IsOpt, NodeKind:TERRAString;
  HasLabels:Boolean;
Begin
  Dest.WriteLine();

{  Dest.WriteLine('Constructor '+Self.Name+'Parser.Create();');
  Dest.WriteLine('Begin');
    For I:=0 To Pred(RuleCount) Do
    If (Rules[I].Preprocessor) Then
    Begin
      Dest.WriteLine(#9+'AddPreProcessorMatcher('+Rules[I].Name+'PreProcessorMatcher.Create());');
    End;

  Dest.WriteLine('End;');
  Dest.WriteLine();}

  Dest.WriteLine('Function '+Self.Name+'Parser.Parse(Source:Stream; IgnoreCase:Boolean):ASTNode;');
  Dest.WriteLine('Begin');
  Dest.WriteLine(#9'_Lexer := '+Self.Name+'Lexer.Create();');
  Dest.WriteLine(#9'Result := Inherited Parse(Source, IgnoreCase);');
  Dest.WriteLine('End;');
  Dest.WriteLine();

  Dest.WriteLine('Function '+Self.Name+'Parser.GetTokenName(ID:Cardinal; Const Value:TERRAString):TERRAString;');
  Dest.WriteLine('Begin');
  Dest.WriteLine(#9'Case ID Of');
  For I:=1 To Pred(LastTokenID) Do
  Begin
    {If Tokens[I].RegExpr Then
      S := StringLower(Tokens[I].Name)
    Else
      S := Tokens[I].Value;}

    S := TokenNames[I];


    Dest.WriteLine(#9#9+CardinalToString(I)+': Result := '''+S+''';');
  End;
  Dest.WriteLine(#9'Else');
  Dest.WriteLine(#9#9'Result := CardinalToString(ID);');
  Dest.WriteLine(#9'End;');
  Dest.WriteLine(#9'Result := ''[''+Result+'']'';');
  Dest.WriteLine('End;');
  Dest.WriteLine();


  For I:=0 To Pred(NodeCount) Do
  Begin
    If I = 0 Then
      NodeKind := 'AST'
    Else
      NodeKind := Nodes[I].Name;


    If I=0 Then
    Begin
      OptArg := '';
      IsOpt := 'False';
    End Else
    Begin
      OptArg := Nodes[I].GenArgs();
      IsOpt := 'IsOpt';
    End;

{    If (I>0) Then
    Begin
      If S3<>'' Then
        S3 := '; ' +S3;
      S3 := 'Parent:ASTNode'+S3;
    End;}

    If Nodes[I].LookAheadIndex>=0 Then
      Continue;
    
    Dest.WriteLine('Function '+Self.Name+'Parser.Parse'+Nodes[I].Name+'('+OptArg+'):'+NodeKind+'Node;');

    If (Nodes[I].FieldCount>0) Then
      Nodes[I].GenCompoundNode(Dest, (I>0), IsOpt)
    Else
      Nodes[I].GenAbstractNode(Dest, (I>0), IsOpt);

    Dest.WriteLine('End;');
    Dest.WriteLine();
  End;
End;

Procedure Grammar.GenerateTokenImplementation(Dest: Stream);
Var
  I,J, K:Integer;
  S, S2:TERRAString;
Begin
  For I:=1 To Pred(TokenCount) Do
  If Tokens[I].RegExpr Then
  Begin
{    Dest.WriteLine('Constructor RegexMatcher'+Tokens[I].Name+'.Create(Token:Cardinal);');
    Dest.WriteLine('Begin');
    Dest.WriteLine(#9+' Self._Token := Token;');
    Dest.WriteLine('End;');
    Dest.WriteLine();}

    Dest.WriteLine('Function '+Tokens[I].Name+'RegexMatcher.Match(Const C:TERRAChar):Boolean;');
    Dest.WriteLine('Begin');

    If (Tokens[I].RegExpr) Then
      Tokens[I].GenerateRegExp(Dest)
    Else
      Tokens[I].GenerateStringMatch(Dest);

    Dest.WriteLine('End;');
    Dest.WriteLine();
  End;
End;

Procedure Grammar.GenerateLexerInterface(Dest: Stream);
Begin
  Dest.WriteLine(#9+Self.Name+'Lexer = Class(Lexer)');
  Dest.WriteLine(#9#9+'Public');
  Dest.WriteLine(#9#9#9+'Constructor Create();');
  Dest.WriteLine(#9+'End;');
  Dest.WriteLine();
End;

Procedure Grammar.GenerateLexerImplementation(Dest: Stream);
Var
  S2, Comment, TokenValue:TERRAString;
  I, Priority:Integer;
Begin
  Dest.WriteLine('Constructor '+Self.Name+'Lexer.Create();');
  Dest.WriteLine('Begin');
  For I:=1 To Pred(TokenCount) Do
  Begin
    If Tokens[I].Discard Then
      S2 := 'lexerDiscard'
    Else
      S2 := 'lexerAccept';

    TokenValue := CardinalToString(Tokens[I].ID);

    Comment := Tokens[I].Value;
    StringReplaceText(StringFromChar(NewLineChar), '\n', Comment);

    If Tokens[I].RegExpr Then
    Begin
      If (StringContains(']*', Tokens[I].Value)) Then
        Priority := 0
      Else
      If (StringContains(']+', Tokens[I].Value)) Then
        Priority := 1
      Else
      If (StringContains(']?', Tokens[I].Value)) Then
        Priority := 1
      Else
        Priority := 3;
      Dest.WriteLine(#9+'AddMatcher('+Tokens[I].Name+'RegexMatcher.Create('+TokenValue+', '+IntToString(Priority)+', '+S2+'));  // '+Comment);
    End Else
    If StringLength(Tokens[I].Value)>1 Then
      Dest.WriteLine(#9+'AddMatcher(StringTokenMatcher.Create('+TokenValue+', '''+Tokens[I].Value+''', '+S2+'));')
    Else
      Dest.WriteLine(#9+'AddMatcher(CharTokenMatcher.Create('+TokenValue+', '+CardinalToString(StringFirstChar(Tokens[I].Value))+', '+S2+'));  // '+Comment);
  End;
  Dest.WriteLine('End;');
  Dest.WriteLine();
End;

procedure Grammar.GenerateVisitorInterface(Dest: Stream);
Var
  I:Integer;
Begin
  Dest.WriteLine(#9+Self.Name+'Processor = Class(TERRAObject)');
  Dest.WriteLine(#9#9+'Public');

  For I:=0 To Pred(NodeCount) Do
  Begin
    Dest.WriteLine(#9#9#9+'Procedure Visit'+Nodes[I].Name+'(Node:'+Nodes[I].Name+'Node); Virtual;');
  End;

  Dest.WriteLine(#9+'End;');
  Dest.WriteLine();
End;

Procedure Grammar.GenerateVisitorImplementation(Dest: Stream);
Var
  I,J:Integer;
Begin
  For I:=0 To Pred(NodeCount) Do
  Begin
    Dest.WriteLine('Procedure '+Self.Name+'Processor.Visit'+Nodes[I].Name+'(Node:'+Nodes[I].Name+'Node);');

    For J:=0 To Pred(Nodes[I].FieldCount) Do
    If (Nodes[I].Fields[J].Entry.Kind = grammar_Rule) And (Nodes[I].Fields[J].Entry.ListID>0) And (Not Nodes[I].Fields[J].Entry.Null)  Then
    Begin
        Dest.WriteLine('Var');
        Dest.WriteLine(#9'I:Integer;');
    End;

    Dest.WriteLine('Begin');

    If (Nodes[I].FieldCount>0) Then
    Begin
      For J:=0 To Pred(Nodes[I].FieldCount) Do
      If (Nodes[I].Fields[J].Entry.Kind = grammar_Rule) And (Not Nodes[I].Fields[J].Entry.Null)  Then
      Begin
        If Nodes[I].Fields[J].Entry.ListID>0 Then
        Begin
          Dest.WriteLine(#9'For I:=0 To Pred(Length(Node.'+Nodes[I].Fields[J].Node.Name+'s)) Do');
          Dest.WriteLine(#9#9'Visit'+Nodes[I].Fields[J].Node.Name+'(Node.'+Nodes[I].Fields[J].Name+'s[I]);');
        End Else
          Dest.WriteLine(#9'Visit'+Nodes[I].Fields[J].Node.Name+'(Node.'+Nodes[I].Fields[J].Name+');');
      End;
    End Else
    Begin
      For J:=0 To Pred(Nodes[I].ChildrenCount) Do
      Begin
        Dest.WriteLine(#9'If (Node Is '+Nodes[I].Children[J].Name+'Node) Then');
        Dest.WriteLine(#9#9'Visit'+Nodes[I].Children[J].Name+'('+Nodes[I].Children[J].Name+'Node(Node))');
        Dest.WriteLine(#9'Else');
      End;
    End;
    Dest.WriteLine('End;');
    Dest.WriteLine();
  End;
End;



(*procedure Grammar.GeneratePreProcessorInterface(Dest: Stream);
Var
  I,J:Integer;
begin
  For I:=0 To Pred(RuleCount) Do
  If (Rules[I].Preprocessor) Then
  Begin
    Dest.WriteLine(#9+Rules[I].Name+'PreProcessorMatcher = Class(PreProcessorMatcher)');

    Dest.WriteLine(#9#9+'Function Match(Token:LexerToken):Boolean; Override;');
    Dest.WriteLine(#9#9+'Procedure Consume(Token:LexerToken); Override;');

    Dest.WriteLine(#9+'End;');
    Dest.WriteLine();
  End;
End;

procedure Grammar.GeneratePreProcessorImplementation(Dest: Stream);
Var
  I,J:Integer;
begin
  For I:=0 To Pred(RuleCount) Do
  If (Rules[I].Preprocessor) Then
  Begin
    Dest.WriteLine('Function '+Rules[I].Name+'PreProcessorMatcher.Match(Token:LexerToken):Boolean;');
    Dest.WriteLine('Begin');
    Dest.WriteLine('End;');
    Dest.WriteLine();

    Dest.WriteLine('Procedure '+Rules[I].Name+'PreProcessorMatcher.Consume(Token:LexerToken);');
    Dest.WriteLine('Begin');
    Dest.WriteLine('End;');
    Dest.WriteLine();
  End;
End;*)


Procedure Grammar.Generate(Dest: Stream);
Var
  S:TERRAString;
Begin
  Dest.WriteLine('Unit '+ Self.Name+';');
  Dest.WriteLine();
  Dest.WriteLine('{$I terra.inc}');
  Dest.WriteLine();
  Dest.WriteLine('Interface');
  Dest.WriteLine('Uses TERRA_String, TERRA_Utils, TERRA_Stream, TERRA_Lexer, TERRA_Parser;');
  Dest.WriteLine();

  Dest.WriteLine('Type');
  GenerateTokenInterface(Dest);
  GenerateNodesInterface(Dest);
//  GeneratePreProcessorInterface(Dest);
  GenerateLexerInterface(Dest);
  GenerateVisitorInterface(Dest);
  GenerateParserInterface(Dest);

  Dest.WriteLine('Implementation');
  Dest.WriteLine('Uses TERRA_Error;');
  Dest.WriteLine();

  GenerateTokenImplementation(Dest);
  GenerateNodesImplementation(Dest);
//  GeneratePreProcessorImplementation(Dest);
  GenerateLexerImplementation(Dest);
  GenerateVisitorImplementation(Dest);
  GenerateParserImplementation(Dest);

  Dest.WriteLine();
  Dest.WriteLine('End.');

End;



Var
  G:Grammar;
  S:TERRAString;
  Dest:Stream;


Begin
  {If ParamCount<1 Then
  Begin
    WriteLn('Sintax: ProgName <filename>');
    Halt(1);
  End;

  S := ParamStr(1);}

  S := 'delphi.grammar';
  //S := 'xml.grammar';
  
  G := Grammar.Create(S);


  Dest := FileStream.Create(G.Name+'.pas');
  Dest.Encoding := encodingASCII;
  Dest.EOL := EOL_Windows;
  //Dest := MemoryStream.Create(32);
  G.Generate(Dest);
  G.Destroy();


{  Dest.Seek(0);
  While Not Dest.EOF Do
  Begin
    Dest.ReadLine(S);
    WriteLn(S);
  End;}
  ReleaseObject(Dest);

  FloatToString(0.5e+23);

  WrIteLn('Finished!');
  ReadLn;
End.
