private package Kit.Parser.Tokens is

   type Token is
      (Tok_None, Tok_End_Of_File, Tok_Bad_Character,
       Tok_Identifier, Tok_Integer_Constant, Tok_String_Constant,

       Tok_Abstract, Tok_End, Tok_For, Tok_Is, Tok_Key, Tok_New, Tok_Package,
       Tok_Record, Tok_Type, Tok_Unique, Tok_Use, Tok_With,

       Tok_Colon, Tok_Semi, Tok_Left_Paren, Tok_Right_Paren, Tok_Comma,
       Tok_Dot, Tok_Dot_Dot, Tok_Arrow, Tok_Becomes, Tok_Apostrophe, Tok_Box);

end Kit.Parser.Tokens;
