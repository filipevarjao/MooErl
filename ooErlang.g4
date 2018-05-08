grammar ooErlang;

forms : form+ EOF ;

form : (attribute | function | ruleClauses) '.' |
       (('class_attributes' | 'attributes') '.'  oo_attributes_1) |
       (('class_methods' | 'methods') '.' oo_methods);

oo_attributes_1 : (oo_attribute ( '.' |';' oo_attributes) | );
oo_attributes : oo_attribute (';' oo_attributes | '.');
oo_attribute : tokVar ( |'=' exprs);

oo_methods : (function '.' oo_methods | );

/// Tokens

tokAtom : TokAtom ;
TokAtom : [a-z@ß-öø-ÿ][0-9a-zA-Z_@]*
        | '\'' ( '\\' (~'\\'|'\\') | ~[\\''] )* '\'' ;

tokVar : TokVar ;
TokVar : [A-Z_À-ÖØ-Þ][0-9a-zA-Z_]* ;

tokFloat : TokFloat ;
TokFloat : '-'? [0-9]+ '.' [0-9]+  ([Ee] [+-]? [0-9]+)? ;

tokInteger : TokInteger ;
TokInteger : '-'? [0-9]+ ('#' [0-9a-zA-Z]+)? ;

tokChar : TokChar ;
TokChar : '$' ('\\'? ~[\r\n] | '\\' [0-9] [0-9] [0-9]) ;

tokString : TokString ;
TokString : '"' ( '\\' (~'\\'|'\\') | ~[\\"] )* '"' ;

// antlr4 would not accept spec as an Atom otherwise.
AttrName : '-' ('spec' | 'callback') ;

Comment : '%' ~[\r\n]* '\r'? '\n' -> skip ;

WS : [ \t\r\n]+ -> skip ;



attribute : '-' tokAtom          attrVal
          | '-' tokAtom     typedAttrVal
          | '-' tokAtom '(' typedAttrVal ')'
          | AttrName        typeSpec
          ;


/// Typing

typeSpec :     specFun typeSigs
         | '(' specFun typeSigs ')'
         ;

specFun :             tokAtom
        | tokAtom ':' tokAtom
// The following two are retained only for backwards compatibility;
// they are not part of the EEP syntax and should be removed.
        |             tokAtom '/' tokInteger '::'
        | tokAtom ':' tokAtom '/' tokInteger '::'
        ;

typedAttrVal : expr ','  typedRecordFields
             | expr '::' topType
             ;

typedRecordFields : '{' typedExprs '}' ;

typedExprs : typedExpr
           | typedExpr  ',' typedExprs
           | expr       ',' typedExprs
           | typedExpr  ','      exprs ;

typedExpr : expr '::' topType ;

typeSigs : typeSig (';' typeSig)* ;

typeSig : funType ('when' typeGuards)? ;

typeGuards : typeGuard (',' typeGuard)* ;

typeGuard : tokAtom '(' topTypes ')'
          | tokVar '::' topType ;

topTypes : topType (',' topType)* ;

topType : (tokVar '::')? topType100 ;

topType100 : type ('|' topType100)? ;

type : type '..'     type
     | type addOp    type
     | type multOp   type
     |      prefixOp type
     | '(' topType ')'
     | tokVar
     | tokAtom
     | tokAtom             '('          ')'
     | tokAtom             '(' topTypes ')'
     | tokAtom ':' tokAtom '('          ')'
     | tokAtom ':' tokAtom '(' topTypes ')'
     | '['                   ']'
     | '[' topType           ']'
     | '[' topType ',' '...' ']'
     | '#' '{'              '}'
     | '#' '{' mapPairTypes '}'
     | '{'          '}'
     | '{' topTypes '}'
     | '#' tokAtom '{'            '}'
     | '#' tokAtom '{' fieldTypes '}'
     | binaryType
     | tokInteger
     | 'fun' '('            ')'
     | 'fun' '(' funType100 ')' ;

funType100 : '(' '...' ')' '->' topType
           | funType ;

funType : '(' (topTypes)? ')' '->' topType ;

mapPairTypes : mapPairType (',' mapPairType)* ;

mapPairType : topType '=>' topType ;

fieldTypes : fieldType (',' fieldType)* ;

fieldType : tokAtom '::' topType ;

binaryType : '<<'                             '>>'
           | '<<' binBaseType                 '>>'
           | '<<'                 binUnitType '>>'
           | '<<' binBaseType ',' binUnitType '>>'
           ;

binBaseType : tokVar ':'            type ;

binUnitType : tokVar ':' tokVar '*' type ;


/// Exprs

attrVal :     expr
        | '(' expr           ')'
        |     expr ',' exprs
        | '(' expr ',' exprs ')' ;

function : functionClause (';' functionClause)* ;

functionClause : tokAtom clauseArgs clauseGuard clauseBody ;


clauseArgs : argumentList ;

clauseGuard : ('when' guard)? ;

clauseBody : '->' exprs ;


// Note: order of expr matters.

expr :      'catch'     expr
     | expr ('=' | '!') expr
     | expr 'orelse'    expr
     | expr 'andalso'   expr
     | expr compOp      expr
     | expr listOp      expr
     | expr addOp       expr
     | expr multOp      expr
     |      prefixOp    expr
     | map
     | functionCall
     | recordExpr
     | remoteExpr
     | expr (':'|'::') exprMax
     | expr '::' expr
     | exprMax
     ;

exprMax : tokVar
        | atomic
        | list
        | binary
        | listComprehension
        | binaryComprehension
        | tuple
      //  | struct
        | bracedExpr
        | beginEndExpr
        | ifExpr
        | caseExpr
        | receiveExpr
        | funExpr
        | tryExpr
        ;

functionCall : remoteExpr argumentList
             | exprMax    argumentList ;

remoteExpr : exprMax ':' exprMax ;

list : '['      ']'
     | '[' expr tail
     ;
tail :          ']'
     | '|' expr ']'
     | ',' expr tail
     ;

binary : '<<'             '>>'
       | '<<' binElements '>>' ;

binElements : binElement (',' binElement)* ;

binElement : bitExpr optBitSizeExpr optBitTypeList ;

bitExpr : prefixOp? exprMax ;

optBitSizeExpr : (':' bitSizeExpr)? ;

optBitTypeList : ('/' bitTypeList)? ;

bitTypeList : bitType ('-' bitType)* ;

bitType : tokAtom (':' tokInteger)? ;

bitSizeExpr : exprMax ;


listComprehension :   '['  expr   '||' lcExprs ']' ;

binaryComprehension : '<<' binary '||' lcExprs '>>' ;

lcExprs : lcExpr (',' lcExpr)* ;

lcExpr : expr
       | expr   '<-' expr
       | binary '<=' expr
       ;

tuple : '{' exprs? '}' ;

bracedExpr : '(' expr ')' ;

beginEndExpr : 'begin' exprs 'end' ;

/* struct : tokAtom tuple ; */

map :         '#' '{' mapFields? '}'
    | exprMax '#' '{' mapFields? '}'
    | map     '#' '{' mapFields? '}' ;

mapFields : mapField (',' mapField)* ;

mapField : expr '=>' expr
         | expr ':=' expr ;


/* N.B. Field names are returned as the complete object, even if they are
   always atoms for the moment, this might change in the future.           */

recordExpr : exprMax?   '#' tokAtom ('.' tokAtom | recordTuple)
           | recordExpr '#' tokAtom ('.' tokAtom | recordTuple)
           ;

recordTuple : '{' recordFields? '}' ;

recordFields : recordField (',' recordField)* ;

recordField : (tokVar | tokAtom) '=' expr ;


ifExpr : 'if' ifClauses 'end' ;

ifClauses : ifClause (';' ifClause)* ;

ifClause : guard clauseBody ;


caseExpr : 'case' expr 'of' crClauses 'end' ;

crClauses : crClause (';' crClause)* ;

crClause : expr clauseGuard clauseBody ;


receiveExpr : 'receive' crClauses                         'end'
            | 'receive'           'after' expr clauseBody 'end'
            | 'receive' crClauses 'after' expr clauseBody 'end'
            ;


funExpr : 'fun'               tokAtom   '/' tokInteger
        | 'fun' atomOrVar ':' atomOrVar '/' integerOrVar
        | 'fun' funClauses 'end'
        ;

atomOrVar : tokAtom | tokVar ;

integerOrVar : tokInteger | tokVar ;


funClauses : funClause (';' funClause)* ;

funClause : argumentList clauseGuard clauseBody ;


tryExpr : 'try' exprs ('of' crClauses)? tryCatch ;

tryCatch : 'catch' tryClauses               'end'
         | 'catch' tryClauses 'after' exprs 'end'
         |                    'after' exprs 'end' ;

tryClauses : tryClause (';' tryClause)* ;

tryClause : (atomOrVar ':')? expr clauseGuard clauseBody ;



argumentList : '(' exprs? ')' ;

exprs : expr (',' expr)* ;

guard : exprs (';' exprs)* ;

atomic : tokChar
       | tokInteger
       | tokFloat
       | tokAtom
       | (tokString)+
       ;

prefixOp : '+'
         | '-'
         | 'bnot'
         | 'not'
         ;

multOp : '/'
       | '*'
       | 'div'
       | 'rem'
       | 'band'
       | 'and'
       ;

addOp : '+'
      | '-'
      | 'bor'
      | 'bxor'
      | 'bsl'
      | 'bsr'
      | 'or'
      | 'xor'
      ;

listOp : '++'
       | '--'
       ;

compOp : '=='
       | '/='
       | '=<'
       | '<'
       | '>='
       | '>'
       | '=:='
       | '=/='
       ;


ruleClauses : ruleClause (';' ruleClause)* ;

ruleClause : tokAtom clauseArgs clauseGuard ruleBody ;

ruleBody : ':-' lcExprs ;

