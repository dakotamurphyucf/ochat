(* Reviewed against chatml_parser.mly and the checked program/task guides.
   Literal production IDs and action hashes are intentional: do not regenerate
   this file in CI. A grammar production is not a claim of executable syntax;
   structural, explicit-rejection and never-reduced branches are included. *)
let productions =
  [ ( "binding_pattern -> LIDENT"
    , "64f9eadbf08997396c12ca08cdcb6c2cb4e2e1e1151694d141d7344e6a615cd0"
    , "chatml.programs" )
  ; ( "binding_pattern -> TUPLE_OPEN binding_pattern COMMA binding_pattern \
       binding_pattern_tail TUPLE_CLOSE"
    , "4ccf655128c60df390db3251483571f09f8cf82b7a2fd61b5f6a1a85f9e4f800"
    , "chatml.programs" )
  ; ( "binding_pattern -> UNDERSCORE"
    , "9acb26325e5b659bdcb36b5d11166e6c26a4277d56479c466925565e8c861395"
    , "chatml.programs" )
  ; ( "binding_pattern_tail -> "
    , "c14678994f533c3f2c3d97521e122c3e03fbcf0e255923dd67dc47d1eb0c4a32"
    , "chatml.programs" )
  ; ( "binding_pattern_tail -> COMMA binding_pattern binding_pattern_tail"
    , "2ffabfbb3c3e5a359574ca03e332771765f8ae16b4b792dbe7e9d2ce500621b1"
    , "chatml.programs" )
  ; ( "expr -> TUPLE_OPEN expr_sequence COMMA expr_sequence tuple_expr_tail TUPLE_CLOSE"
    , "76e3ea1267361fffb81c8dc24b0e87a28476c6480fd6aca9ea9a7f0ce864a88e"
    , "chatml.programs" )
  ; ( "pattern -> TUPLE_OPEN pattern COMMA pattern tuple_pattern_tail TUPLE_CLOSE"
    , "f57c35d14e3acbd862ca1ff0c70dbb5f6baeca377ce7a55df89d43c8e5f26396"
    , "chatml.programs" )
  ; ( "tuple_expr_tail -> "
    , "d94bfd4678f4c6bb363f729193fcf4bfbe721691afb3351c04fc0a7138c99ebe"
    , "chatml.programs" )
  ; ( "tuple_expr_tail -> COMMA expr_sequence tuple_expr_tail"
    , "dcc97d1d50e16822735d662eec738ec520ae96a0a187a0b96acc3ab926204df7"
    , "chatml.programs" )
  ; ( "tuple_pattern_tail -> "
    , "723b55c09f1503ad43c074114a6aa1b1696f102ea72dcde7340191f8a7ff6e89"
    , "chatml.programs" )
  ; ( "tuple_pattern_tail -> COMMA pattern tuple_pattern_tail"
    , "42fa5d3e12350de7cd7ddfbc7d87ab7b36686662fdae5b62abbb6610974016b1"
    , "chatml.programs" )
  ; ( "type_product -> type_postfix"
    , "74998295798c3b056063670563cbbabe876e6528d343a6b6d798220b8925f68c"
    , "chatml.programs" )
  ; ( "type_product -> type_postfix STAR type_postfix type_product_tail"
    , "e15bafc598f7c2925284a48f0443c824db4cb530ff2659962ffadf1cc5a90f90"
    , "chatml.programs" )
  ; ( "type_product_tail -> "
    , "d607031832146a6a56ebe45b6ae22cdc1fcb873cf7a255f7fd44bcea4aa8b743"
    , "chatml.programs" )
  ; ( "type_product_tail -> STAR type_postfix type_product_tail"
    , "1fdc63b020f2a75959428d3a7017cc7e7761a2bc88fb70788846a5b5cf168968"
    , "chatml.programs" )
  ; ( "expr -> BANG expr"
    , "057318ed316e874dd1e1be39a5677e64e1456603b687ddfbc5acc3ffd8880ae3"
    , "chatml.programs" )
  ; ( "expr -> BANGEQ expr"
    , "e559f7b164f2014b1097cd44482e781f339b15109ae8fc7d49c145da939536f0"
    , "chatml.programs" )
  ; ( "expr -> BOOL"
    , "a1803c3af361e4eef77964a0bf79e8ebb43601d7370c318dab94daf2242702ef"
    , "chatml.programs" )
  ; ( "expr -> DOT expr"
    , "23b371bbbb7e6531216ac83aaaf252034648b9acad77f740eb3c1ab2dbce9bcf"
    , "chatml.programs" )
  ; ( "expr -> FLOAT"
    , "112f16ee078473ba388df7bc1bb5539f236dbbc8a36d82eb2a33b8e23b2f9efa"
    , "chatml.programs" )
  ; ( "expr -> FUN LPAREN RPAREN ARROW expr_sequence"
    , "2f48c8249622693bf29be7705b1781b4986a87db2cafbad564f485bdea3f3d6a"
    , "chatml.programs" )
  ; ( "expr -> FUN params ARROW expr_sequence"
    , "1fd6503a4e1d4dbb5c30653e4e3b73cc0b6d88d8105e7ef56f642da1bd89677f"
    , "chatml.programs" )
  ; ( "expr -> IF expr_sequence THEN expr_sequence ELSE expr_sequence"
    , "7abd64d00a72e5fddaafc99f1a9db0bdc273700a4386085804f295965fac29bb"
    , "chatml.programs" )
  ; ( "expr -> INT"
    , "0fcb13c1957023eee82bbdee4a02af9be6c546fdf35d4d69c96ad5096d364b6a"
    , "chatml.programs" )
  ; ( "expr -> LBRACE expr_sequence WITH field_decls RBRACE"
    , "0183de8c92bed0f199153372b9f8e09efbbf3a99fe90b05c1efccc172baeb3c9"
    , "chatml.programs" )
  ; ( "expr -> LBRACE field_list RBRACE"
    , "bcd4067fe726b87a11ad9858a9ca5ff0d6e5872e18ddc8f439c8f42cda8734bc"
    , "chatml.programs" )
  ; ( "expr -> LBRACKET expr_list RBRACKET"
    , "316c862d43bbb6e8748c30ccec0dae3acb6de28064af5ff5bcdf8b946dcd70c3"
    , "chatml.programs" )
  ; ( "expr -> LET binding_pattern COLON type_expr EQ expr_sequence IN expr_sequence"
    , "9ea4b8a0d4842da6902015e2b6db6783fc7ff7086636352c2acddfc304d4c5f8"
    , "chatml.programs" )
  ; ( "expr -> LET binding_pattern EQ expr_sequence IN expr_sequence"
    , "c7e72b23e0c2ab2989f8995bd924f48054106cd764e2cfad62e8aea4e0345284"
    , "chatml.programs" )
  ; ( "expr -> LET LIDENT LPAREN RPAREN EQ expr_sequence IN expr_sequence"
    , "3497ea0119c594ff5e4476297b03a277a49302b478498e9ce8f33cc11534fcea"
    , "chatml.programs" )
  ; ( "expr -> LET LIDENT params EQ expr_sequence IN expr_sequence"
    , "b68ae4b2751817d139c3ee3fc3d0c2d4436679d22fa16f28e9bd6dbbfecd98f1"
    , "chatml.programs" )
  ; ( "expr -> LET REC rec_bindings IN expr_sequence"
    , "c6db87b6e25297a8f41d136dd44de0f8d9f68ab0ccff428ca595802ab86c0069"
    , "chatml.programs" )
  ; ( "expr -> LETPLUS task_let_binder EQ expr_sequence IN expr_sequence"
    , "ed12fe95b7e8f5d81e98b09095d19e0de6078548b0065249b5f37c92da2c33de"
    , "chatml.task-effects" )
  ; ( "expr -> LETSTAR task_let_binder EQ expr_sequence IN expr_sequence"
    , "f46c67fe0c18db571a37ca74457208ef94348588277bf659f960cb2341c2e39d"
    , "chatml.task-effects" )
  ; ( "expr -> LPAREN RPAREN"
    , "07bf962ca3f7468fc60b94a29cfafe87e61975f856e8998cac8e3e57617e9505"
    , "chatml.programs" )
  ; ( "expr -> LPAREN expr_sequence RPAREN"
    , "95e313f490b0f00c43241fd20c690894f32ff31b63f96f8a60c40e9ff8fb8a2e"
    , "chatml.programs" )
  ; ( "expr -> MATCH expr_sequence WITH pattern_cases"
    , "083be16aff95a3f0cd92da580d9d7f34fc0592743b4e687f8821e9826a322055"
    , "chatml.programs" )
  ; ( "expr -> MINUS expr"
    , "4a58cf8072195a6ad46171b5b065af7d2c97dba180c06d31c0d7f3e7358e072f"
    , "chatml.programs" )
  ; ( "expr -> MINUSDOT expr"
    , "029c39b70522ef467181ac60e50fb34ca7dd3789a6c6ff9f012c32ca56af6c91"
    , "chatml.programs" )
  ; ( "expr -> REF expr"
    , "8475ed6ec9fbec9a40606f52759896557993b40900c28193fc4dc5cee2a9face"
    , "chatml.programs" )
  ; ( "expr -> STRING"
    , "6283862d341d55e9b124be69793b4d3079499bb728503a29a6a7fd1a388a86ad"
    , "chatml.programs" )
  ; ( "expr -> TICKIDENT"
    , "434e9670072cf399359516c22110af45d768bbc19e1a152dad9d92a843c1f5b5"
    , "chatml.programs" )
  ; ( "expr -> TICKIDENT LPAREN expr_list RPAREN"
    , "55ee2e8075bc01d8db85e1dd662288565438deef7f4d794982ffdf4ff81861aa"
    , "chatml.programs" )
  ; ( "expr -> WHILE expr_sequence DO expr_sequence DONE"
    , "07d0c74909b0d15fec5e9acf71b41d404a4592e66fa86d00a6a21ebc0135843f"
    , "chatml.programs" )
  ; ( "expr -> expr BANGEQ expr"
    , "1d04c987b6422daf6da3a8bc13e7ad9ea390004dbdb0b4f1965e843d12eb5b66"
    , "chatml.programs" )
  ; ( "expr -> expr COLONEQ expr"
    , "6d487c40bd4445e53fb948c10bd726a30ecc748241e853efe9c390682d166334"
    , "chatml.programs" )
  ; ( "expr -> expr DOT LIDENT"
    , "9f7cbd3cb1b49ee31429f538a6019393a84c7196ae2cb391885aee67619e01fd"
    , "chatml.programs" )
  ; ( "expr -> expr DOT LIDENT LPAREN expr_list RPAREN"
    , "60183c31d0873d88ee9b242dbab1e01cf863c81a1c4e2259fc1a10485656e8e3"
    , "chatml.programs" )
  ; ( "expr -> expr DOT UIDENT"
    , "f5ded0a1d3b521e1e332cda55215476723550c70feddf5c742c66c5c8db9447a"
    , "chatml.programs" )
  ; ( "expr -> expr EQEQ expr"
    , "0f0eebdcd223ab6b513eed23cf487a5689078b0f9366140248bcf940ca503883"
    , "chatml.programs" )
  ; ( "expr -> expr GT expr"
    , "fde82ecec7e2f510bb654aa3a458f39f838d3cea8d849e541b20edcd22ad9f79"
    , "chatml.programs" )
  ; ( "expr -> expr GTDOT expr"
    , "20c5369d5f8a759131777881f8d4e797ddfd03d014040f396d2bea740e351ee3"
    , "chatml.programs" )
  ; ( "expr -> expr GTEQ expr"
    , "4404dcdf30d1e90dc5ac5e56ffb2bf5493b8c059bda8c64c61b9af93d9117e82"
    , "chatml.programs" )
  ; ( "expr -> expr GTEQDOT expr"
    , "335ec7e5563d2759ee0ce617c15c5cc22c94b3488c273783d9ae96a0016c3541"
    , "chatml.programs" )
  ; ( "expr -> expr LBRACKET expr RBRACKET"
    , "6237c6a553cae11e39e5591086a8466b0069242b30f8dd56b0d4c9f97a9f7690"
    , "chatml.programs" )
  ; ( "expr -> expr LBRACKET expr RBRACKET LEFTARROW expr"
    , "22a61af6819facf6349151621eb2a7a60f8699800f08f01cee249f9013181101"
    , "chatml.programs" )
  ; ( "expr -> expr LPAREN expr_list RPAREN"
    , "21cafbd4df0ce642a6fd097c91ed5241026c1064cebeba684db4a9b530c3f056"
    , "chatml.programs" )
  ; ( "expr -> expr LT expr"
    , "b195a06bff7cbf64090680ef48c68259a3dad6d387976a9e3409c6bf9d409eaf"
    , "chatml.programs" )
  ; ( "expr -> expr LTDOT expr"
    , "160229245c1caa393c03535533120eb235165bf941ae92fda84fd2f2ffe88506"
    , "chatml.programs" )
  ; ( "expr -> expr LTEQ expr"
    , "9855aa022e2ece3667e68e19989d9486e7bc1b859152d3b63bc5fef3ac042206"
    , "chatml.programs" )
  ; ( "expr -> expr LTEQDOT expr"
    , "466e2630c0b02b18ca363a844100da810923ca853250d5043cc936411ce39973"
    , "chatml.programs" )
  ; ( "expr -> expr MINUS expr"
    , "072bd38c3859167699f9e8ce7636b06deb847cd3f354520d22600b266b21c9a9"
    , "chatml.programs" )
  ; ( "expr -> expr MINUSDOT expr"
    , "5c27f0cea481fec5775437ce014a7f3bb1a29d44c1f4e97b97b96f3d8b159b0e"
    , "chatml.programs" )
  ; ( "expr -> expr PLUS expr"
    , "76ad97d5a2ed16e114530a2d05c17c92850f491cece6951df8f7e35252694bd0"
    , "chatml.programs" )
  ; ( "expr -> expr PLUSDOT expr"
    , "5e028efd21c82697649fca6b006f597a5b3f18ca9dafff5260e32528f6062d6c"
    , "chatml.programs" )
  ; ( "expr -> expr PLUSPLUS expr"
    , "74f3b86283cff4c2e98a0d1440e13039a10628994f415c7a2c858d764db723aa"
    , "chatml.programs" )
  ; ( "expr -> expr SLASH expr"
    , "c8169c49d0ee09e47b0d22d15d9620a7e344cc45233bc61b66255a495cdaeed5"
    , "chatml.programs" )
  ; ( "expr -> expr SLASHDOT expr"
    , "4c0ad9df6c197c94dd7bf21f1ec831239943ce4b911a726edca815cd5144b1fe"
    , "chatml.programs" )
  ; ( "expr -> expr STAR expr"
    , "22c59bc2a67b7de48a1e9fd223531aec5f9c352c56627ea663d8bb13414a3174"
    , "chatml.programs" )
  ; ( "expr -> expr STARDOT expr"
    , "cb67f095b21702b09c51bfe5272237ac006c2c27ef39ff5afc40ebef204040fa"
    , "chatml.programs" )
  ; ( "expr -> ident"
    , "2088660d8b2e25cfeeac1f6d95c8cf356861e5629e8c02e83ebeffad1740b951"
    , "chatml.programs" )
  ; ( "expr_list -> "
    , "93bdb147494dabdf616e538c6c12cec0ad249b57fcd9fa03a01af33d5e025960"
    , "chatml.programs" )
  ; ( "expr_list -> expr_list_nonempty"
    , "b518ffdf57f5890c429e35ff22672fef41552b87741f332669426b35cf8cf604"
    , "chatml.programs" )
  ; ( "expr_list_nonempty -> expr_sequence"
    , "a9c6ba111529267c993bfe18782ebf28929eb76943f9a6b7843e54baec7cf5dd"
    , "chatml.programs" )
  ; ( "expr_list_nonempty -> expr_sequence COMMA expr_list_nonempty"
    , "243da55e097d79f3ee7fbd23f56cc9710a59a415b30ce7c074c5b6cd223ece2d"
    , "chatml.programs" )
  ; ( "expr_sequence -> expr"
    , "8315f3ff47050ac83b4b8277090b8c7df877c317c10fc8bc49bce6c4f6232bf7"
    , "chatml.programs" )
  ; ( "expr_sequence -> expr SEMI expr_sequence"
    , "1018bfe6134944bef31c8b86180b0915f7f93bfbe15ea750f857fba921b461a6"
    , "chatml.programs" )
  ; ( "field_decl -> LIDENT EQ expr"
    , "5a087bebd1d046833a8a27f7e2d5844bd24e21cafdc0c1980392ced4cdfd99c5"
    , "chatml.programs" )
  ; ( "field_decls -> field_decl"
    , "8e677e6916193aa33abf060860991b92c2783897c40c520e3ee24a45aed5ede3"
    , "chatml.programs" )
  ; ( "field_decls -> field_decl SEMI"
    , "f3e3d8306ebb52377e85417c08bfac125fa8366f07cf8df6084c858e72f4e6ad"
    , "chatml.programs" )
  ; ( "field_decls -> field_decl SEMI field_decls"
    , "cd540e28d6db69311c6b0a5b165642964bcaff165cd9a0671c2da87856e9b93f"
    , "chatml.programs" )
  ; ( "field_list -> "
    , "0df52b85b3f0db01f0fd0c34bdf150157f1ac65fa54b3a28de0bd1a7d5ef1af4"
    , "chatml.programs" )
  ; ( "field_list -> field_decls"
    , "6f21e4827dfd8f2430997a1b0a1f1f34bdd2436de6115cf6903422ec41906e5a"
    , "chatml.programs" )
  ; ( "ident -> LIDENT"
    , "b87e7f7068e405c22dc447b4b0cfc30f4c19748aba7b7f3fe50ac58ac253a1bd"
    , "chatml.programs" )
  ; ( "ident -> UIDENT"
    , "48bf68021e17d574ffab71c94a8008466353d84b94ae50e282f8fb3bcecce402"
    , "chatml.programs" )
  ; ( "loption(separated_nonempty_list(COMMA,type_expr)) -> "
    , "7cfb73df4d1abed0c691870dbbec4395abdd1b3b1fa728b4f16d380fd2ba7ad8"
    , "chatml.programs" )
  ; ( "loption(separated_nonempty_list(COMMA,type_expr)) -> \
       separated_nonempty_list(COMMA,type_expr)"
    , "52464b694af641988ae7e68f5d9ce08842e1a3a034f0540ec01353d5b349034c"
    , "chatml.programs" )
  ; ( "module_interface -> signature_items EOF"
    , "605a6b923efadf9a9b1f19d9f4ac3de859ed513c8eb235723703002ff21e56f7"
    , "chatml.programs" )
  ; ( "module_stmt -> LET binding_pattern COLON type_expr EQ expr_sequence"
    , "df0cce369fc66f500a89a48aed488ae17a469da97bc12c9c2e59af3604152d38"
    , "chatml.programs" )
  ; ( "module_stmt -> LET binding_pattern EQ expr_sequence"
    , "ae26a2eda10c585f449010f2df82581cfa99c2709ff2e16d80fed74cb6d782f3"
    , "chatml.programs" )
  ; ( "module_stmt -> LET LIDENT LPAREN RPAREN EQ expr_sequence"
    , "3c0e914d78d3464f817622aeaafeda6491df51a15e42e268e0e714426b48b99f"
    , "chatml.programs" )
  ; ( "module_stmt -> LET LIDENT params EQ expr_sequence"
    , "beff5c7daace66e1a491b156cd133f6a539a9d2f783894a860642b1eb4e07dd6"
    , "chatml.programs" )
  ; ( "module_stmt -> LET REC rec_bindings"
    , "23054fa2658bc521d7e6fbe7c43abfc3fb4eae2918bc6ddcbe0e51297e838faf"
    , "chatml.programs" )
  ; ( "module_stmt -> LET REC rec_bindings IN expr_sequence"
    , "aff9a294b32141958564e1dc981ca5700864ce86d7eddb2a0151ab7a3a23b6e6"
    , "chatml.programs" )
  ; ( "module_stmt -> MODULE UIDENT COLON SIG signature_items END EQ STRUCT module_stmts \
       END"
    , "0b0facb89369fb027dd3447e2eb7003b0b0c4abd85f069764b8df9f414a363d4"
    , "chatml.programs" )
  ; ( "module_stmt -> MODULE UIDENT EQ STRUCT module_stmts END"
    , "c1f6c3702e8b91b514016730ef5b0dd704f16d72220dac519d0f8edd69670e19"
    , "chatml.programs" )
  ; ( "module_stmt -> OPEN UIDENT"
    , "79241d108e711e47e3e4148d8bb2709999cbfbb9a0f2ac7f340c0787d3402803"
    , "chatml.programs" )
  ; ( "module_stmt -> TYPE type_parameters LIDENT EQ type_expr"
    , "70d7bc1d718b44df62c5b4cb5572743cd691369755de20dbab8db28a3906eaf6"
    , "chatml.programs" )
  ; ( "module_stmt -> expr"
    , "b5ad1d57b820bf02e7e1de21c4b10876178daf4e6965c34b43cbfe0399e8ad9f"
    , "chatml.programs" )
  ; ( "module_stmts -> "
    , "cbc9eac1d02c7d0af57c4325b1f4b12e78b090160e62d549ec274fd59086243d"
    , "chatml.programs" )
  ; ( "module_stmts -> module_stmts SEMISEMI"
    , "b09b3a0f39c7c3d7acb7b41859b752bec4cc0ca669dfd9ef107d11aea8c9fdad"
    , "chatml.programs" )
  ; ( "module_stmts -> module_stmts module_stmt"
    , "064b2c2f22ca7687a5685c130e61638243cc8630f056da206a2e29cd4c5afe12"
    , "chatml.programs" )
  ; ( "opt_row_tail -> "
    , "40ee50f8afc95e67bff236ec23dbebffc1b257fd9e1b55b7c1990af2b7e7d59d"
    , "chatml.programs" )
  ; ( "opt_row_tail -> SEMI UNDERSCORE"
    , "8c048a67d09af3a583a2bdbf9264321efc841302ab13fb85091b6087ee92e037"
    , "chatml.programs" )
  ; ( "params -> binding_pattern"
    , "ad1b7b1462bd17d04926233e70e50c5f63839e8cbd9f58f7154930f53d3ffe77"
    , "chatml.programs" )
  ; ( "params -> binding_pattern params"
    , "49027476b678a53baae578ec0187ab585c12f5d7ec97cecdd328587fd9a2e806"
    , "chatml.programs" )
  ; ( "pattern -> BOOL"
    , "8af56d393b003397a947524e5ce9c155d7143c41753d2b2d4d03a7cc2b6024a0"
    , "chatml.programs" )
  ; ( "pattern -> FLOAT"
    , "79efdee59183825bcdddea09b865a6466ce06ac5978a147a42141358a1552f30"
    , "chatml.programs" )
  ; ( "pattern -> INT"
    , "5018ef77ed620d22d7d348f4fb7f7af2ac700aa6304d6a4d93a78dac93a6b675"
    , "chatml.programs" )
  ; ( "pattern -> LBRACE pattern_field_list RBRACE"
    , "b6116a18be72d095b8019fb8621503084cdfba67e41b55d6a798bcc0496bba06"
    , "chatml.programs" )
  ; ( "pattern -> LIDENT"
    , "d864640dcbf47f7d3c8e110753bf0952fc6bb55627db5180305244299d60eaf4"
    , "chatml.programs" )
  ; ( "pattern -> LPAREN RPAREN"
    , "ca35b7d52bcf68e5ecf188923d8071dd000ad990ebc2610d84561ef528fa90f4"
    , "chatml.programs" )
  ; ( "pattern -> STRING"
    , "723773369aab9c6e4490019b57ad7ea7397bee167d106c44b1a7b6aeab31b648"
    , "chatml.programs" )
  ; ( "pattern -> TICKIDENT"
    , "a3c3caadaaafb9157d44b8f58edbfdc775a5c92a4bf886aef7ddade138996dda"
    , "chatml.programs" )
  ; ( "pattern -> TICKIDENT LPAREN pattern_list RPAREN"
    , "191d78aadd41cc27952b49bea5b71992250b4ede0fbfb0f6926a700fb984b7b7"
    , "chatml.programs" )
  ; ( "pattern -> UNDERSCORE"
    , "6466cddb4459b2f9e98ab3406e7407b043980972fba5cd71ca80f9dfbccc8667"
    , "chatml.programs" )
  ; ( "pattern_case -> BAR pattern ARROW expr_sequence"
    , "cda2163525b228720f8b8e2a779eddcd690733c5b6c77d313e8d8dbcfb913a99"
    , "chatml.programs" )
  ; ( "pattern_cases -> pattern_case"
    , "ec61cd558f6da3d479c34f9d9814849229335cc8b1977659074c0cacbfdce86b"
    , "chatml.programs" )
  ; ( "pattern_cases -> pattern_case pattern_cases"
    , "e7791b787d043fb0341ced502c4d294f90cf4b16c4a8fb016f0377298d04afc7"
    , "chatml.programs" )
  ; ( "pattern_field_decl -> LIDENT EQ pattern"
    , "32f86d0c24884a3f460a5cfcb3bcff3947c5a23f09efc0c98220818666c8e7b3"
    , "chatml.programs" )
  ; ( "pattern_field_decls -> pattern_field_decl SEMI"
    , "3bb779e3cc743f5e59fe1dddf8e9a503d4df01e682597381d864a80e90c28b13"
    , "chatml.programs" )
  ; ( "pattern_field_decls -> pattern_field_decl SEMI UNDERSCORE"
    , "1a614bb1920443481cf3085665022deb8c67f04d4615e1f822d1cc8fdfec3f38"
    , "chatml.programs" )
  ; ( "pattern_field_decls -> pattern_field_decl SEMI UNDERSCORE SEMI"
    , "b6e17a4e1bb773a262d9e1268b82cba26686cfd543074d28b6bf40d9b1f7a455"
    , "chatml.programs" )
  ; ( "pattern_field_decls -> pattern_field_decl SEMI pattern_field_decls"
    , "91a25c9c5a15e0c06c1d81cb6b0537e83444eee9b83d83fca6e7efd3479a5eba"
    , "chatml.programs" )
  ; ( "pattern_field_decls -> pattern_field_decl opt_row_tail"
    , "804d5561ede75ec9a35b18d022926e7c4c08a8f8072f1f2be79211e5b3a82968"
    , "chatml.programs" )
  ; ( "pattern_field_list -> "
    , "db580902ba8317022ed22c026026b1dc48e45f81c94009c8fb5893e648329eba"
    , "chatml.programs" )
  ; ( "pattern_field_list -> pattern_field_decls"
    , "7fdca9262ab2850aa05f4cb077d29477ecbe1a7f9db25d9ca634ce6cb270187c"
    , "chatml.programs" )
  ; ( "pattern_list -> pattern"
    , "0e27f13e364803d7aa9edf05463572f16b1d9e2ab3480675b934275526232ec9"
    , "chatml.programs" )
  ; ( "pattern_list -> pattern COMMA pattern_list"
    , "5c3cb4fb03734b2c0117f00020dfb52ee462fb9c069a9386cc709b69f515f9fa"
    , "chatml.programs" )
  ; ( "program -> program_stmts EOF"
    , "62e3ad7aea6d21100de63a1463b8fd7841343c4183185a31d2618f2b3e3ae587"
    , "chatml.programs" )
  ; ( "program_stmt -> module_stmt"
    , "58794c5bf009a4a210f75537f5aa8ed2a686843295dc6938f3391ba010d97b4f"
    , "chatml.programs" )
  ; ( "program_stmts -> "
    , "c2996c2a3edc6db165c2afd4ba76c4bae8e79b4cdae071c15ae8e643011a2e70"
    , "chatml.programs" )
  ; ( "program_stmts -> program_stmts SEMISEMI"
    , "94945cd73b21b8d03a9a51d226b98ee73f38f9ea3217961e4dad07cca49e8a34"
    , "chatml.programs" )
  ; ( "program_stmts -> program_stmts program_stmt"
    , "00099d29534710d61b79af3811b0e155912026efbee51ce8da707c9829cc9b2a"
    , "chatml.programs" )
  ; ( "rec_binding -> LIDENT COLON type_expr EQ expr_sequence"
    , "d1709173fbf4a116ac3ff1143bd16c7200215a21138f6705deb2489394f42258"
    , "chatml.programs" )
  ; ( "rec_binding -> LIDENT EQ expr_sequence"
    , "6984b777001e52a4f831139a1793a6cc20a1b41ed4ce2d3191eceb314d639c86"
    , "chatml.programs" )
  ; ( "rec_binding -> LIDENT LPAREN RPAREN EQ expr_sequence"
    , "8eedceee6abca93e13cbc10ab4786d87e99ea8259d417be6bad9417fa6c7b03a"
    , "chatml.programs" )
  ; ( "rec_binding -> LIDENT params EQ expr_sequence"
    , "79a6e826d7aa8d7429f1814298b8deca01b5f11fbbb0d03e12f0d6716b6aaa6f"
    , "chatml.programs" )
  ; ( "rec_bindings -> rec_binding"
    , "eac0103500dd1fda84fcfd14de00d835efe14744313016c33c2275def4ee5528"
    , "chatml.programs" )
  ; ( "rec_bindings -> rec_binding AND rec_bindings"
    , "34869410922a768afe9a6fd6c9ed765e54b3827e8c83554b68731883d8aa689e"
    , "chatml.programs" )
  ; ( "separated_nonempty_list(AND,signature_requirement) -> signature_requirement"
    , "e81a2cdd82cc91df1dd18336b5067f9b4961639c03bb4467977410922acd1412"
    , "chatml.programs" )
  ; ( "separated_nonempty_list(AND,signature_requirement) -> signature_requirement AND \
       separated_nonempty_list(AND,signature_requirement)"
    , "de74c92f5a266462eae59c719beebaa97800986a3128c51d51e48c1871c58a6a"
    , "chatml.programs" )
  ; ( "separated_nonempty_list(COMMA,type_parameter_name) -> type_parameter_name"
    , "d14d74a1654364254d5f33a9577273cc3ac0a4fca10cac394c14d506256dadbb"
    , "chatml.programs" )
  ; ( "separated_nonempty_list(COMMA,type_parameter_name) -> type_parameter_name COMMA \
       separated_nonempty_list(COMMA,type_parameter_name)"
    , "1f4d02ccac1cb6750751eab34122a9b3252ee1c9484ffb1580b00a0f7f7ddf23"
    , "chatml.programs" )
  ; ( "separated_nonempty_list(COMMA,type_expr) -> type_expr"
    , "4f8fc775dbdabbacacefadb0146226ad7829a3da3f112a64d69fca51a5dc0de9"
    , "chatml.programs" )
  ; ( "separated_nonempty_list(COMMA,type_expr) -> type_expr COMMA \
       separated_nonempty_list(COMMA,type_expr)"
    , "e57a26f25b11a93a1272d01157808d784a90cc67fca2b9ea9986a04411c87e07"
    , "chatml.programs" )
  ; ( "signature_item -> MODULE UIDENT COLON SIG signature_items END"
    , "e6b31a6603d1c03a322511171cd5d4b9e3011c296cec8207747119de10a001e9"
    , "chatml.programs" )
  ; ( "signature_item -> TYPE type_parameters LIDENT EQ type_expr"
    , "53e0fd1dfb853f7511d7e083c3bbbe2ba3f03b5750e72793b3567c3f22ab7657"
    , "chatml.programs" )
  ; ( "signature_item -> VAL LIDENT COLON type_expr signature_requirements"
    , "063c89319de530cbe3842c7f4e31c9cfb25af8dd11eb269f63b94e5e571230a2"
    , "chatml.programs" )
  ; ( "signature_items -> "
    , "90e26fe4701c5fed333e1d355bcf9a8e83e7de4506f4982749fcc982eb8bbfe5"
    , "chatml.programs" )
  ; ( "signature_items -> signature_items SEMISEMI"
    , "c342de023aae7aabb69ecac9883c29330d50a59488ce1fae0ef8ed66a2fb4eb9"
    , "chatml.programs" )
  ; ( "signature_items -> signature_items signature_item"
    , "7f1832e2918496440006d324c1b3020987aa4f285280c78fe3c833870ac0b327"
    , "chatml.programs" )
  ; ( "signature_requirement -> LIDENT LPAREN TYPEVAR RPAREN"
    , "199e5ae425b8d1f63a5892f817f2c12a0035f227c6cb4f4ab0cc7229ca3fba4a"
    , "chatml.programs" )
  ; ( "signature_requirements -> "
    , "24ea778bd7bda6d25505ff8ca8273969f55aad19bb8245cdd60833f4641361f3"
    , "chatml.programs" )
  ; ( "signature_requirements -> WHERE separated_nonempty_list(AND,signature_requirement)"
    , "beff65e54433c84281809b34ccdbf47554e210470d14a1400546a5b6bf6224d7"
    , "chatml.programs" )
  ; ( "task_let_binder -> LIDENT"
    , "399cab802587e936a95798539a02efe86bb7ad4eee21c874fe293772cc3a8874"
    , "chatml.task-effects" )
  ; ( "task_let_binder -> LPAREN RPAREN"
    , "b8628924e6f5bc154c8bbba9ffb5fe0bb1a6753d8b50bd76ceb2b1818ddb1e4e"
    , "chatml.task-effects" )
  ; ( "type_arrow -> FUN LPAREN loption(separated_nonempty_list(COMMA,type_expr)) RPAREN \
       ARROW type_arrow"
    , "3e911c4653dfc44d2c3f294de9c55cef16f4c6895b3c8804a38913e65ae4e6eb"
    , "chatml.programs" )
  ; ( "type_arrow -> type_product"
    , "0cab4607f82c3457032a89da5e3015fe5c3a459456cebea5313e035d21e4650f"
    , "chatml.programs" )
  ; ( "type_arrow -> type_product ARROW type_arrow"
    , "e6d1d9f394c1021d8636a21b64cb6adb697bc5e42d1deea8986e261428e8f69f"
    , "chatml.programs" )
  ; ( "type_expr -> type_arrow"
    , "deb63956ae9e7c61bc3bdce30c864d0a70bd961324db8982e526edadd1a954d2"
    , "chatml.programs" )
  ; ( "type_expr_list -> type_expr"
    , "a5c4aca8de673c2531744efcb7fe75834018800899bd65c45574906c54109ab5"
    , "chatml.programs" )
  ; ( "type_expr_list -> type_expr COMMA type_expr_list"
    , "e5955de0bb397d064917a987a073cdbb51712f2dcd757ea10eef1120546d3f57"
    , "chatml.programs" )
  ; ( "type_field -> LIDENT COLON type_expr"
    , "3b27049bc28a8271af23e51738cec0bd4516cf7c3f134601f2acf150e0f76c05"
    , "chatml.programs" )
  ; ( "type_field_list -> "
    , "fb9cc4e09564679864d8dde3e29c47d099a819405adaa28fa0becf6b49844ee7"
    , "chatml.programs" )
  ; ( "type_field_list -> type_fields"
    , "ec6a99bd444ca4d9e5419ac53f1891b14ba6a969d774ec4c14ddbfbeb7ef5338"
    , "chatml.programs" )
  ; ( "type_fields -> type_field"
    , "a26070ba27928c71e67f2527cf88f55155e2d9d6b077134a178ee9f374cbf24f"
    , "chatml.programs" )
  ; ( "type_fields -> type_field SEMI"
    , "16de13be19884a6a429d8cc2c92405b5afd89e0383b81c77cd9fa4966280b7ed"
    , "chatml.programs" )
  ; ( "type_fields -> type_field SEMI type_fields"
    , "9de604591335feee5569c44e6b3f57b4b32bb381633441a2681c49cec59af8cc"
    , "chatml.programs" )
  ; ( "type_module_path -> UIDENT"
    , "1f16fa03301870512346bbcb272e908d6b21edf0e35212bb90d3e396c1c6002e"
    , "chatml.programs" )
  ; ( "type_module_path -> type_module_path DOT UIDENT"
    , "a9775758dc2862747ae79a25bb58ef257d1c6e12877e05b44e23f8411f308a4b"
    , "chatml.programs" )
  ; ( "type_name -> LIDENT"
    , "62bb859df0f268866965840f393d207d315dbe954f6550bc314f7e2073e5fd48"
    , "chatml.programs" )
  ; ( "type_name -> type_module_path DOT LIDENT"
    , "97e4532b7c8e7b1c027a1fa2cb113058c8deec9c3cdf012d6053b15983d17d2e"
    , "chatml.programs" )
  ; ( "type_parameters -> "
    , "335e1c23c586436ae0109876fcb3606fcb0cda0fb773b0a4258f8b612d82f48f"
    , "chatml.programs" )
  ; ( "type_parameters -> LPAREN separated_nonempty_list(COMMA,type_parameter_name) RPAREN"
    , "f95df831b86e13f948643b1aa7ff429714151c8df2588f1fa4b695375e9267e6"
    , "chatml.programs" )
  ; ( "type_parameters -> type_parameter_name"
    , "628364e02653183dfe3b0e5b33cf394dd4bd9ef2c2c73aafa0f94ebeac368d9e"
    , "chatml.programs" )
  ; ( "type_postfix -> LPAREN type_expr COMMA type_expr_list RPAREN type_name"
    , "d275c0aad40120a2a2c13e78342983e7bb4fb3017cd1f589947d407f580f4e3e"
    , "chatml.programs" )
  ; ( "type_postfix -> type_postfix type_name"
    , "2b6520095b4c69438f1d83fdbb5093c7ea6505872930cb564f9cd234dfe64d43"
    , "chatml.programs" )
  ; ( "type_postfix -> type_primary"
    , "205376817f93c40f5034bfb40c705c333d8dbaf704a83d827c551ff6a1603dbc"
    , "chatml.programs" )
  ; ( "type_primary -> LBRACE type_field_list RBRACE"
    , "f937ff90a813e823d897ca81a08cb75defd914ac513db33b7a3eca99901af128"
    , "chatml.programs" )
  ; ( "type_primary -> LBRACKET type_variant_cases_opt RBRACKET"
    , "0e2dd53119c8a313e3f73bf08da75f2d24f1dd06d47850fec056be6718eac49d"
    , "chatml.programs" )
  ; ( "type_primary -> LPAREN type_expr RPAREN"
    , "60551e60d9edde1e6dabac4cee8a5a9c67a53c986c46dfd1f633d56f85f0352a"
    , "chatml.programs" )
  ; ( "type_primary -> TYPEVAR"
    , "aecd8a9292cd7b84a4689f42ef84d316fcb3c7d068f4037f32ea7cc7da6c7806"
    , "chatml.programs" )
  ; ( "type_primary -> type_name"
    , "95e0d6c016a26f19435ba735bbc5179210a605ac40ec22cf8e92a54fac4e6724"
    , "chatml.programs" )
  ; ( "type_variant_case -> TICKIDENT"
    , "76446843c5e00b3197804f184ce25dec6cc2f7d26175a75ef83d39d42f312508"
    , "chatml.programs" )
  ; ( "type_variant_case -> TICKIDENT LPAREN type_expr_list RPAREN"
    , "76b5e4976836fc1178a47da77e2e8bf81548eefb7480b540255e2d5bd3580521"
    , "chatml.programs" )
  ; ( "type_variant_cases -> type_variant_case"
    , "0cc70d0583838725c7d4181e20803ddd1c58949a374bf3511b08a57eaacee9bf"
    , "chatml.programs" )
  ; ( "type_variant_cases -> type_variant_case BAR type_variant_cases"
    , "ed76f06dbbbd6678aa291ba73cfba213f464c1c1a7b08ddf9b645f7704069142"
    , "chatml.programs" )
  ; ( "type_variant_cases_opt -> "
    , "66a51fa0378ce657e7141a8fcb884e86880797e1af18a47c29f8a167676fbf2a"
    , "chatml.programs" )
  ; ( "type_variant_cases_opt -> type_variant_cases"
    , "e79e300c05ef0ebdc0a87ee8b026ac27e137822b0a19d423c0dd8a7180256c78"
    , "chatml.programs" )
  ; ( "type_parameter_name -> TYPEVAR"
    , "0ad1ee03203ca3b211f6613fc562b6e1dfb2b76faf76bed1d613bf9867a6fa06"
    , "chatml.programs" )
  ]
;;

let topic_contracts =
  [ ( "one_off_v1"
    , "chatml.programs"
    , "e42f62e62669484346bc191239440a0a5af6f1283d8dcb2152159bc4bbe0f7ae" )
  ; ( "one_off_v1"
    , "chatml.task-effects"
    , "4a4f9617b016c491dfd397876bbb32d01e95f727064471e2a4432dca0478a17e" )
  ; ( "tool_v1"
    , "chatml.programs"
    , "6108cb9f14a766d9b672586412a68103fbd1e67f7c59a36973f25c39ca534dc2" )
  ; ( "moderator_v1"
    , "chatml.programs"
    , "4cc897db6cd131324a2c7c708fbbd5bd80c07b2b7266138b3c2e9d21327137ca" )
  ; ( "delegated_moderator_v1"
    , "chatml.programs"
    , "815167ced652829517e79a19e67fcbbba6cb83d2845fc3652d0fc8a541868e7f" )
  ; ( "tool_v1"
    , "chatml.task-effects"
    , "26ab23b3bcf104bdd0c5e34e6123c552f4bc1adf38a16bfca7cf9696c5077d02" )
  ; ( "moderator_v1"
    , "chatml.task-effects"
    , "b7f328cc4186ccde448fc23cc6158c198e81fd55cf493efa8c812889a51ee0c3" )
  ; ( "delegated_moderator_v1"
    , "chatml.task-effects"
    , "d12925247f47d7caef5cf2abe69229efe2625413de4beb44e2f20783a17c890a" )
  ]
;;
