// source: https://github.com/antlr/grammars-v4/blob/master/json/JSON.g4

grammar JSON;

error: UNEXPECTED_CHAR {
 throw new RuntimeException("UNEXPECTED_CHAR=" + $UNEXPECTED_CHAR.text);
};

json
   : value
   ;

obj
   : '{' pair (',' pair)* '}'
   | '{' '}'
   ;

pair
   : key=STRING ':' value
   ;

arr
   : '[' value (',' value)* ']'
   | '[' ']'
   ;

value
   : STRING #StringValue
   | NUMBER #NumberValue
   | obj #ObjectValue
   | arr #ArrayValue
   | 'true' #True
   | 'false' #False
   | 'null' #Null
   ;


STRING
   : '"' (ESC | SAFECODEPOINT)* '"'
   ;


fragment ESC
   : '\\' (["\\/bfnrt] | UNICODE)
   ;
fragment UNICODE
   : 'u' HEX HEX HEX HEX
   ;
fragment HEX
   : [0-9a-fA-F]
   ;
fragment SAFECODEPOINT
   : ~ ["\\\u0000-\u001F]
   ;


NUMBER
   : '-'? INT ('.' [0-9] +)? EXP?
   ;


fragment INT
   : '0' | [1-9] [0-9]*
   ;

fragment EXP
   : [Ee] [+\-]? INT
   ;

WS
   : [ \t\n\r] + -> skip
   ;

UNEXPECTED_CHAR
                : .
                ;