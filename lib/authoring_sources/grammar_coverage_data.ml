(* Reviewed against chatml_parser.mly and the checked program/task guides.
   Literal production IDs and action hashes are intentional: do not regenerate
   this file in CI. A grammar production is not a claim of executable syntax;
   structural, explicit-rejection and never-reduced branches are included. *)
let productions =
  [ ( "expr -> BANG expr"
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
  ; ( "expr -> LET LIDENT COLON type_expr EQ expr_sequence IN expr_sequence"
    , "f924cee462ea30ac892d5d99312970177a6d53a4d646271fbe91c5e0b69269ca"
    , "chatml.programs" )
  ; ( "expr -> LET LIDENT EQ expr_sequence IN expr_sequence"
    , "e704cb6fd06f5e0bf00eb399f84494a36d5874559390c11e564c9aa536778427"
    , "chatml.programs" )
  ; ( "expr -> LET LIDENT LPAREN RPAREN EQ expr_sequence IN expr_sequence"
    , "ea75dc7578d4f9ea02182a163fbbd8e88cdba0904f0151af270300c2ce4e14b0"
    , "chatml.programs" )
  ; ( "expr -> LET LIDENT params EQ expr_sequence IN expr_sequence"
    , "96ff18651fabd34c003f62da640ea68d46c6b644b2bc8536fcdba684b38f513b"
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
    , "7cd3f8f4eb0e150f657499dc94bed8401d197788d1a6f10468a122e8eef905c4"
    , "chatml.programs" )
  ; ( "expr -> TICKIDENT LPAREN expr_list RPAREN"
    , "fc9dc5e44bfed0365f06aff454b8cd8b73ed688a9b04b558f618554cfeb32a08"
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
    , "d4f906dcc12f965856df37238663196077e38f2b2f9e788098cf89a97d99dd7d"
    , "chatml.programs" )
  ; ( "expr -> expr DOT LIDENT LPAREN expr_list RPAREN"
    , "c22d839fc46b72e5a6ef3bebfae053f1861a3a2aff2072d5f2df76173ccce61f"
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
    , "7cb7cd40f2d0778b7b611bf9cf3125753113687473f3483a9410fd328a1e89a9"
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
    , "5e28dc1163b08e6632c15ab31d4b6f2bea91f1944b0b98946ab57005cd9afe13"
    , "chatml.programs" )
  ; ( "ident -> UIDENT"
    , "76012b1af4c6efeca89d90e629d00afd6616078b3993593510e9afe4fc353b2f"
    , "chatml.programs" )
  ; ( "module_stmt -> LET LIDENT COLON type_expr EQ expr_sequence"
    , "f26bf0731db0e65a20b3a1f3332d4e3fb5176fedba82a72075519dfc2663046b"
    , "chatml.programs" )
  ; ( "module_stmt -> LET LIDENT EQ expr_sequence"
    , "017dcb48f2fe8167c9017f133fd5059215258c35e62c8b34269105cb9fdf712c"
    , "chatml.programs" )
  ; ( "module_stmt -> LET LIDENT LPAREN RPAREN EQ expr_sequence"
    , "7791b205c33a3d4608c84e87036996d0e91fb7e14ee81ff916ee21936173c427"
    , "chatml.programs" )
  ; ( "module_stmt -> LET LIDENT params EQ expr_sequence"
    , "c3a675d69d5af18af2a7fb3266309f4c7e6ceb4271cbcf55c523845514ee7f5e"
    , "chatml.programs" )
  ; ( "module_stmt -> LET REC rec_bindings"
    , "23054fa2658bc521d7e6fbe7c43abfc3fb4eae2918bc6ddcbe0e51297e838faf"
    , "chatml.programs" )
  ; ( "module_stmt -> LET REC rec_bindings IN expr_sequence"
    , "aff9a294b32141958564e1dc981ca5700864ce86d7eddb2a0151ab7a3a23b6e6"
    , "chatml.programs" )
  ; ( "module_stmt -> MODULE UIDENT EQ STRUCT module_stmts END"
    , "38483c6e4ee774f548e7bca690c10751da6e679760c5abe60c5f4619698781fe"
    , "chatml.programs" )
  ; ( "module_stmt -> OPEN UIDENT"
    , "1a5206247c3cedacc03fa99eed5ee192dd6d33d1a9ef9e79ab5c443919a32dc7"
    , "chatml.programs" )
  ; ( "module_stmt -> expr"
    , "b5ad1d57b820bf02e7e1de21c4b10876178daf4e6965c34b43cbfe0399e8ad9f"
    , "chatml.programs" )
  ; ( "module_stmts -> "
    , "cbc9eac1d02c7d0af57c4325b1f4b12e78b090160e62d549ec274fd59086243d"
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
  ; ( "params -> LIDENT"
    , "454ae02acffe3b0e9e79695ff9f63822d343836b8e6cb03de37ab895fb7f3da7"
    , "chatml.programs" )
  ; ( "params -> LIDENT params"
    , "53fd7bd65fb7b0d2cc68b3df7da28827dc0588dcae2b3c7d4ffc33607d7077e4"
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
    , "025dceb86720d4f807807fa9c4eefb24d2ab80c41e2f3959320afea6f8866522"
    , "chatml.programs" )
  ; ( "pattern -> LPAREN RPAREN"
    , "ca35b7d52bcf68e5ecf188923d8071dd000ad990ebc2610d84561ef528fa90f4"
    , "chatml.programs" )
  ; ( "pattern -> STRING"
    , "723773369aab9c6e4490019b57ad7ea7397bee167d106c44b1a7b6aeab31b648"
    , "chatml.programs" )
  ; ( "pattern -> TICKIDENT"
    , "2095661aa75a4d2a291d40cb3e3816a11f96803fd857199512d7c856f8fa784e"
    , "chatml.programs" )
  ; ( "pattern -> TICKIDENT LPAREN pattern_list RPAREN"
    , "4b05fc11f7c88994192ff2e0ccb507dc4e6706cf479257a921d9ca65e22f6d65"
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
    , "92a9b8160451558f2cf0c17222f103014a76c7fe3f87d49ac132382159b9cedd"
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
  ; ( "program_stmt -> TYPE LIDENT EQ type_expr"
    , "91ba8dee794454817889ff24ccd7aebf71d57c284798f5e6782308e79b30b71f"
    , "chatml.programs" )
  ; ( "program_stmt -> module_stmt"
    , "58794c5bf009a4a210f75537f5aa8ed2a686843295dc6938f3391ba010d97b4f"
    , "chatml.programs" )
  ; ( "program_stmts -> "
    , "c2996c2a3edc6db165c2afd4ba76c4bae8e79b4cdae071c15ae8e643011a2e70"
    , "chatml.programs" )
  ; ( "program_stmts -> program_stmts program_stmt"
    , "00099d29534710d61b79af3811b0e155912026efbee51ce8da707c9829cc9b2a"
    , "chatml.programs" )
  ; ( "rec_binding -> LIDENT COLON type_expr EQ expr_sequence"
    , "bab4a889b192b7bda1b347ab97f95f085f1b43ff090290a9253bd76ffda88982"
    , "chatml.programs" )
  ; ( "rec_binding -> LIDENT EQ expr_sequence"
    , "fd1a915da06499fe621270497719907f5865b2cc0748a7abf5a4116ec8e54725"
    , "chatml.programs" )
  ; ( "rec_binding -> LIDENT LPAREN RPAREN EQ expr_sequence"
    , "8f109cd5878b7104e2240a3f5bfb82152a211d9e02fdf30a41c2011b5d4f7841"
    , "chatml.programs" )
  ; ( "rec_binding -> LIDENT params EQ expr_sequence"
    , "274b645a0aa3f4da0fa61794a862e8c4affe717242999c3d27305fb3d15c8ef6"
    , "chatml.programs" )
  ; ( "rec_bindings -> rec_binding"
    , "eac0103500dd1fda84fcfd14de00d835efe14744313016c33c2275def4ee5528"
    , "chatml.programs" )
  ; ( "rec_bindings -> rec_binding AND rec_bindings"
    , "34869410922a768afe9a6fd6c9ed765e54b3827e8c83554b68731883d8aa689e"
    , "chatml.programs" )
  ; ( "task_let_binder -> LIDENT"
    , "049d0ff8eefe67c009ae963c418d0ca0b5a14d5cbb7e6c4ee4a17275342826b6"
    , "chatml.task-effects" )
  ; ( "task_let_binder -> LPAREN RPAREN"
    , "9ba5488a96bc63b1272a87dc8504f108c44a7fe212eade661c23454265c68492"
    , "chatml.task-effects" )
  ; ( "type_arrow -> type_postfix"
    , "143e041e450b12b13fafb7a172ce00a9559af222e203b5af0aae9b4b78c8f0af"
    , "chatml.programs" )
  ; ( "type_arrow -> type_postfix ARROW type_arrow"
    , "6e67410799682b20d3b7d3f0da5b405927cc98b75dc9a700cb9805a81c4e4aec"
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
    , "858de38276a8fc4722acd2799009893f2d732c06560c6ffb45aa25c6d2dfe6aa"
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
  ; ( "type_postfix -> type_postfix LIDENT"
    , "5cab8f71b4117c756c2f30b9a308f69ed55b585c085fc7572f7f984f73a8283c"
    , "chatml.programs" )
  ; ( "type_postfix -> type_primary"
    , "205376817f93c40f5034bfb40c705c333d8dbaf704a83d827c551ff6a1603dbc"
    , "chatml.programs" )
  ; ( "type_primary -> LBRACE type_field_list RBRACE"
    , "017d0b1e0e1694231287c7d83a1d0e5954c35f178bc0b1dfd387720b0757562c"
    , "chatml.programs" )
  ; ( "type_primary -> LBRACKET type_variant_cases_opt RBRACKET"
    , "123a3fa003edeef8994a0ba62f20dcdb301b8436f63e6cc531391e2dfe2e32ea"
    , "chatml.programs" )
  ; ( "type_primary -> LIDENT"
    , "219a9355cbe0497ad343e9034e53f9dcfe0dd2a5140546a2c908268b3420071b"
    , "chatml.programs" )
  ; ( "type_primary -> LPAREN type_expr RPAREN"
    , "60551e60d9edde1e6dabac4cee8a5a9c67a53c986c46dfd1f633d56f85f0352a"
    , "chatml.programs" )
  ; ( "type_variant_case -> TICKIDENT"
    , "fa382a98c29ca76294e0df0e9e86c32eb2fcace045f1aca219364588b102d73d"
    , "chatml.programs" )
  ; ( "type_variant_case -> TICKIDENT LPAREN type_expr_list RPAREN"
    , "34abef5a960415a5ff16bd4832aee6889c06d7192f16cb706613faf37a083c98"
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
  ]
;;

let topic_contracts =
  [ ( "one_off_v1"
    , "chatml.programs"
    , "79ad269851735994bfe1ec4e07783e13e4af7ed022ba44efcd146e5bd40cd1cd" )
  ; ( "one_off_v1"
    , "chatml.task-effects"
    , "de4730371e88a3bea91ba98f2ef303c9ebf38aac237ba8d701153c8e644e4d19" )
  ; ( "tool_v1"
    , "chatml.programs"
    , "696d4f167aea68172739ca008ae47b878f5d8580a99fa2a58c3d9f28ac19a7a6" )
  ; ( "moderator_v1"
    , "chatml.programs"
    , "cc43822cdfacbe554acda4ec1dcfb8d7231ff166a8cec6570a70c45cf4d23089" )
  ; ( "delegated_moderator_v1"
    , "chatml.programs"
    , "2d5e08a2be6d893bf1596dbf3f0d9b8592c40e5dc6bad73ddedada59147c0244" )
  ; ( "tool_v1"
    , "chatml.task-effects"
    , "891d098b492e63eceafc9c1eba9b6069a82e08a06c18661be8ce155dcfc89d1f" )
  ; ( "moderator_v1"
    , "chatml.task-effects"
    , "739d75ff8d4b2183b00b13967ccc35b26d23f07fe319370c0575bbdf28564b65" )
  ; ( "delegated_moderator_v1"
    , "chatml.task-effects"
    , "11f63fb503104c5515168c91d1d13a53043320ead580f0062c459aaa39ea40e4" )
  ]
;;
