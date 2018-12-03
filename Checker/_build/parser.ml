
module MenhirBasics = struct
  
  exception Error
  
  type token = 
    | UNPAIR
    | UNITTYPE
    | UNIT
    | TREETYPE
    | TL
    | TIMES
    | THEN
    | SETREF
    | SET
    | SEMICOLON
    | RPAREN
    | REFTYPE
    | RBRACE
    | PROC
    | PLUS
    | PAIR
    | NULL_QUESTION
    | NULLT_QUESTION
    | NODE
    | NEWREF
    | MINUS
    | LPAREN
    | LISTTYPE
    | LETREC
    | LET
    | LESS_THAN
    | LBRACE
    | ISZERO
    | INTTYPE
    | INT of (
# 22 "parser.mly"
       (int)
# 40 "parser.ml"
  )
    | IN
    | IF
    | ID of (
# 23 "parser.mly"
       (string)
# 47 "parser.ml"
  )
    | HD
    | GREATER_THAN
    | GETRST
    | GETLST
    | GETDATA
    | EQUALS
    | EOF
    | END
    | EMPTYTREE
    | EMPTYLIST
    | ELSE
    | DIVIDED
    | DEREF
    | DEBUG
    | CONS
    | COMMA
    | COLON
    | BOOLTYPE
    | BEGIN
    | ARROW
  
end

include MenhirBasics

let _eRR =
  MenhirBasics.Error

type _menhir_env = {
  _menhir_lexer: Lexing.lexbuf -> token;
  _menhir_lexbuf: Lexing.lexbuf;
  _menhir_token: token;
  mutable _menhir_error: bool
}

and _menhir_state = 
  | MenhirState166
  | MenhirState164
  | MenhirState163
  | MenhirState162
  | MenhirState160
  | MenhirState158
  | MenhirState157
  | MenhirState156
  | MenhirState155
  | MenhirState153
  | MenhirState151
  | MenhirState150
  | MenhirState149
  | MenhirState147
  | MenhirState145
  | MenhirState143
  | MenhirState142
  | MenhirState141
  | MenhirState140
  | MenhirState139
  | MenhirState137
  | MenhirState135
  | MenhirState133
  | MenhirState131
  | MenhirState130
  | MenhirState129
  | MenhirState128
  | MenhirState127
  | MenhirState126
  | MenhirState125
  | MenhirState123
  | MenhirState122
  | MenhirState121
  | MenhirState120
  | MenhirState119
  | MenhirState118
  | MenhirState116
  | MenhirState114
  | MenhirState112
  | MenhirState110
  | MenhirState108
  | MenhirState106
  | MenhirState105
  | MenhirState104
  | MenhirState103
  | MenhirState102
  | MenhirState100
  | MenhirState99
  | MenhirState98
  | MenhirState96
  | MenhirState94
  | MenhirState93
  | MenhirState88
  | MenhirState87
  | MenhirState84
  | MenhirState81
  | MenhirState79
  | MenhirState78
  | MenhirState76
  | MenhirState74
  | MenhirState72
  | MenhirState69
  | MenhirState67
  | MenhirState65
  | MenhirState62
  | MenhirState59
  | MenhirState54
  | MenhirState53
  | MenhirState52
  | MenhirState51
  | MenhirState49
  | MenhirState47
  | MenhirState45
  | MenhirState43
  | MenhirState41
  | MenhirState32
  | MenhirState29
  | MenhirState25
  | MenhirState24
  | MenhirState23
  | MenhirState22
  | MenhirState21
  | MenhirState19
  | MenhirState15
  | MenhirState12
  | MenhirState10
  | MenhirState7
  | MenhirState0

# 8 "parser.mly"
  
open Ast

# 178 "parser.ml"

let rec _menhir_run32 : _menhir_env -> 'ttv_tail * _menhir_state * (Ast.texpr) -> 'ttv_return =
  fun _menhir_env _menhir_stack ->
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | BOOLTYPE ->
        _menhir_run27 _menhir_env (Obj.magic _menhir_stack) MenhirState32
    | INTTYPE ->
        _menhir_run26 _menhir_env (Obj.magic _menhir_stack) MenhirState32
    | LESS_THAN ->
        _menhir_run25 _menhir_env (Obj.magic _menhir_stack) MenhirState32
    | LISTTYPE ->
        _menhir_run24 _menhir_env (Obj.magic _menhir_stack) MenhirState32
    | LPAREN ->
        _menhir_run23 _menhir_env (Obj.magic _menhir_stack) MenhirState32
    | REFTYPE ->
        _menhir_run22 _menhir_env (Obj.magic _menhir_stack) MenhirState32
    | TREETYPE ->
        _menhir_run21 _menhir_env (Obj.magic _menhir_stack) MenhirState32
    | UNITTYPE ->
        _menhir_run20 _menhir_env (Obj.magic _menhir_stack) MenhirState32
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState32

and _menhir_goto_texpr : _menhir_env -> 'ttv_tail -> _menhir_state -> (Ast.texpr) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
    match _menhir_s with
    | MenhirState25 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | ARROW ->
            _menhir_run32 _menhir_env (Obj.magic _menhir_stack)
        | TIMES ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _menhir_env = _menhir_discard _menhir_env in
            let _tok = _menhir_env._menhir_token in
            (match _tok with
            | BOOLTYPE ->
                _menhir_run27 _menhir_env (Obj.magic _menhir_stack) MenhirState29
            | INTTYPE ->
                _menhir_run26 _menhir_env (Obj.magic _menhir_stack) MenhirState29
            | LESS_THAN ->
                _menhir_run25 _menhir_env (Obj.magic _menhir_stack) MenhirState29
            | LISTTYPE ->
                _menhir_run24 _menhir_env (Obj.magic _menhir_stack) MenhirState29
            | LPAREN ->
                _menhir_run23 _menhir_env (Obj.magic _menhir_stack) MenhirState29
            | REFTYPE ->
                _menhir_run22 _menhir_env (Obj.magic _menhir_stack) MenhirState29
            | TREETYPE ->
                _menhir_run21 _menhir_env (Obj.magic _menhir_stack) MenhirState29
            | UNITTYPE ->
                _menhir_run20 _menhir_env (Obj.magic _menhir_stack) MenhirState29
            | _ ->
                assert (not _menhir_env._menhir_error);
                _menhir_env._menhir_error <- true;
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState29)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | MenhirState29 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | ARROW ->
            _menhir_run32 _menhir_env (Obj.magic _menhir_stack)
        | GREATER_THAN ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _menhir_env = _menhir_discard _menhir_env in
            let _menhir_stack = Obj.magic _menhir_stack in
            let (((_menhir_stack, _menhir_s), _, (t1 : (Ast.texpr))), _, (t2 : (Ast.texpr))) = _menhir_stack in
            let _5 = () in
            let _3 = () in
            let _1 = () in
            let _v : (Ast.texpr) = 
# 223 "parser.mly"
                                                                 ( PairType ( t1 , t2 ) )
# 266 "parser.ml"
             in
            _menhir_goto_texpr _menhir_env _menhir_stack _menhir_s _v
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | MenhirState32 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | ARROW ->
            _menhir_run32 _menhir_env (Obj.magic _menhir_stack)
        | BEGIN | COMMA | CONS | DEBUG | DEREF | DIVIDED | ELSE | EMPTYLIST | EMPTYTREE | END | EOF | GETDATA | GETLST | GETRST | GREATER_THAN | HD | ID _ | IF | IN | INT _ | ISZERO | LET | LETREC | LPAREN | MINUS | NEWREF | NODE | NULLT_QUESTION | NULL_QUESTION | PAIR | PLUS | PROC | RBRACE | RPAREN | SEMICOLON | SET | SETREF | THEN | TIMES | TL | UNIT | UNPAIR ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let ((_menhir_stack, _menhir_s, (t1 : (Ast.texpr))), _, (t2 : (Ast.texpr))) = _menhir_stack in
            let _2 = () in
            let _v : (Ast.texpr) = 
# 217 "parser.mly"
                                    ( FuncType(t1,t2) )
# 289 "parser.ml"
             in
            _menhir_goto_texpr _menhir_env _menhir_stack _menhir_s _v
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | MenhirState24 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | ARROW ->
            _menhir_run32 _menhir_env (Obj.magic _menhir_stack)
        | BEGIN | COMMA | CONS | DEBUG | DEREF | DIVIDED | ELSE | EMPTYLIST | EMPTYTREE | END | EOF | GETDATA | GETLST | GETRST | GREATER_THAN | HD | ID _ | IF | IN | INT _ | ISZERO | LET | LETREC | LPAREN | MINUS | NEWREF | NODE | NULLT_QUESTION | NULL_QUESTION | PAIR | PLUS | PROC | RBRACE | RPAREN | SEMICOLON | SET | SETREF | THEN | TIMES | TL | UNIT | UNPAIR ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let ((_menhir_stack, _menhir_s), _, (t1 : (Ast.texpr))) = _menhir_stack in
            let _1 = () in
            let _v : (Ast.texpr) = 
# 226 "parser.mly"
                           (ListType(t1))
# 312 "parser.ml"
             in
            _menhir_goto_texpr _menhir_env _menhir_stack _menhir_s _v
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | MenhirState23 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | ARROW ->
            _menhir_run32 _menhir_env (Obj.magic _menhir_stack)
        | RPAREN ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _menhir_env = _menhir_discard _menhir_env in
            let _menhir_stack = Obj.magic _menhir_stack in
            let ((_menhir_stack, _menhir_s), _, (t1 : (Ast.texpr))) = _menhir_stack in
            let _3 = () in
            let _1 = () in
            let _v : (Ast.texpr) = 
# 218 "parser.mly"
                                 ( t1 )
# 338 "parser.ml"
             in
            _menhir_goto_texpr _menhir_env _menhir_stack _menhir_s _v
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | MenhirState22 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | ARROW ->
            _menhir_run32 _menhir_env (Obj.magic _menhir_stack)
        | BEGIN | COMMA | CONS | DEBUG | DEREF | DIVIDED | ELSE | EMPTYLIST | EMPTYTREE | END | EOF | GETDATA | GETLST | GETRST | GREATER_THAN | HD | ID _ | IF | IN | INT _ | ISZERO | LET | LETREC | LPAREN | MINUS | NEWREF | NODE | NULLT_QUESTION | NULL_QUESTION | PAIR | PLUS | PROC | RBRACE | RPAREN | SEMICOLON | SET | SETREF | THEN | TIMES | TL | UNIT | UNPAIR ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let ((_menhir_stack, _menhir_s), _, (t1 : (Ast.texpr))) = _menhir_stack in
            let _1 = () in
            let _v : (Ast.texpr) = 
# 219 "parser.mly"
                          ( RefType(t1) )
# 361 "parser.ml"
             in
            _menhir_goto_texpr _menhir_env _menhir_stack _menhir_s _v
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | MenhirState21 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | ARROW ->
            _menhir_run32 _menhir_env (Obj.magic _menhir_stack)
        | BEGIN | COMMA | CONS | DEBUG | DEREF | DIVIDED | ELSE | EMPTYLIST | EMPTYTREE | END | EOF | GETDATA | GETLST | GETRST | GREATER_THAN | HD | ID _ | IF | IN | INT _ | ISZERO | LET | LETREC | LPAREN | MINUS | NEWREF | NODE | NULLT_QUESTION | NULL_QUESTION | PAIR | PLUS | PROC | RBRACE | RPAREN | SEMICOLON | SET | SETREF | THEN | TIMES | TL | UNIT | UNPAIR ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let ((_menhir_stack, _menhir_s), _, (t1 : (Ast.texpr))) = _menhir_stack in
            let _1 = () in
            let _v : (Ast.texpr) = 
# 229 "parser.mly"
                           (TreeType(t1))
# 384 "parser.ml"
             in
            _menhir_goto_texpr _menhir_env _menhir_stack _menhir_s _v
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | MenhirState19 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | ARROW ->
            _menhir_run32 _menhir_env (Obj.magic _menhir_stack)
        | RPAREN ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _menhir_env = _menhir_discard _menhir_env in
            let _tok = _menhir_env._menhir_token in
            (match _tok with
            | LBRACE ->
                let _menhir_stack = Obj.magic _menhir_stack in
                let _menhir_env = _menhir_discard _menhir_env in
                let _tok = _menhir_env._menhir_token in
                (match _tok with
                | BEGIN ->
                    _menhir_run88 _menhir_env (Obj.magic _menhir_stack) MenhirState41
                | CONS ->
                    _menhir_run86 _menhir_env (Obj.magic _menhir_stack) MenhirState41
                | DEBUG ->
                    _menhir_run85 _menhir_env (Obj.magic _menhir_stack) MenhirState41
                | DEREF ->
                    _menhir_run83 _menhir_env (Obj.magic _menhir_stack) MenhirState41
                | EMPTYLIST ->
                    _menhir_run81 _menhir_env (Obj.magic _menhir_stack) MenhirState41
                | EMPTYTREE ->
                    _menhir_run79 _menhir_env (Obj.magic _menhir_stack) MenhirState41
                | GETDATA ->
                    _menhir_run77 _menhir_env (Obj.magic _menhir_stack) MenhirState41
                | GETLST ->
                    _menhir_run75 _menhir_env (Obj.magic _menhir_stack) MenhirState41
                | GETRST ->
                    _menhir_run73 _menhir_env (Obj.magic _menhir_stack) MenhirState41
                | HD ->
                    _menhir_run71 _menhir_env (Obj.magic _menhir_stack) MenhirState41
                | ID _v ->
                    _menhir_run70 _menhir_env (Obj.magic _menhir_stack) MenhirState41 _v
                | IF ->
                    _menhir_run69 _menhir_env (Obj.magic _menhir_stack) MenhirState41
                | INT _v ->
                    _menhir_run68 _menhir_env (Obj.magic _menhir_stack) MenhirState41 _v
                | ISZERO ->
                    _menhir_run66 _menhir_env (Obj.magic _menhir_stack) MenhirState41
                | LET ->
                    _menhir_run63 _menhir_env (Obj.magic _menhir_stack) MenhirState41
                | LETREC ->
                    _menhir_run54 _menhir_env (Obj.magic _menhir_stack) MenhirState41
                | LPAREN ->
                    _menhir_run52 _menhir_env (Obj.magic _menhir_stack) MenhirState41
                | NEWREF ->
                    _menhir_run50 _menhir_env (Obj.magic _menhir_stack) MenhirState41
                | NODE ->
                    _menhir_run48 _menhir_env (Obj.magic _menhir_stack) MenhirState41
                | NULLT_QUESTION ->
                    _menhir_run46 _menhir_env (Obj.magic _menhir_stack) MenhirState41
                | NULL_QUESTION ->
                    _menhir_run44 _menhir_env (Obj.magic _menhir_stack) MenhirState41
                | PAIR ->
                    _menhir_run42 _menhir_env (Obj.magic _menhir_stack) MenhirState41
                | PROC ->
                    _menhir_run16 _menhir_env (Obj.magic _menhir_stack) MenhirState41
                | SET ->
                    _menhir_run13 _menhir_env (Obj.magic _menhir_stack) MenhirState41
                | SETREF ->
                    _menhir_run11 _menhir_env (Obj.magic _menhir_stack) MenhirState41
                | TL ->
                    _menhir_run9 _menhir_env (Obj.magic _menhir_stack) MenhirState41
                | UNIT ->
                    _menhir_run8 _menhir_env (Obj.magic _menhir_stack) MenhirState41
                | UNPAIR ->
                    _menhir_run1 _menhir_env (Obj.magic _menhir_stack) MenhirState41
                | _ ->
                    assert (not _menhir_env._menhir_error);
                    _menhir_env._menhir_error <- true;
                    _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState41)
            | _ ->
                assert (not _menhir_env._menhir_error);
                _menhir_env._menhir_error <- true;
                let _menhir_stack = Obj.magic _menhir_stack in
                let (_menhir_stack, _menhir_s, _) = _menhir_stack in
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | MenhirState54 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | ARROW ->
            _menhir_run32 _menhir_env (Obj.magic _menhir_stack)
        | ID _v ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _menhir_stack = (_menhir_stack, _v) in
            let _menhir_env = _menhir_discard _menhir_env in
            let _tok = _menhir_env._menhir_token in
            (match _tok with
            | LPAREN ->
                let _menhir_stack = Obj.magic _menhir_stack in
                let _menhir_env = _menhir_discard _menhir_env in
                let _tok = _menhir_env._menhir_token in
                (match _tok with
                | ID _v ->
                    let _menhir_stack = Obj.magic _menhir_stack in
                    let _menhir_stack = (_menhir_stack, _v) in
                    let _menhir_env = _menhir_discard _menhir_env in
                    let _tok = _menhir_env._menhir_token in
                    (match _tok with
                    | COLON ->
                        let _menhir_stack = Obj.magic _menhir_stack in
                        let _menhir_env = _menhir_discard _menhir_env in
                        let _tok = _menhir_env._menhir_token in
                        (match _tok with
                        | BOOLTYPE ->
                            _menhir_run27 _menhir_env (Obj.magic _menhir_stack) MenhirState59
                        | INTTYPE ->
                            _menhir_run26 _menhir_env (Obj.magic _menhir_stack) MenhirState59
                        | LESS_THAN ->
                            _menhir_run25 _menhir_env (Obj.magic _menhir_stack) MenhirState59
                        | LISTTYPE ->
                            _menhir_run24 _menhir_env (Obj.magic _menhir_stack) MenhirState59
                        | LPAREN ->
                            _menhir_run23 _menhir_env (Obj.magic _menhir_stack) MenhirState59
                        | REFTYPE ->
                            _menhir_run22 _menhir_env (Obj.magic _menhir_stack) MenhirState59
                        | TREETYPE ->
                            _menhir_run21 _menhir_env (Obj.magic _menhir_stack) MenhirState59
                        | UNITTYPE ->
                            _menhir_run20 _menhir_env (Obj.magic _menhir_stack) MenhirState59
                        | _ ->
                            assert (not _menhir_env._menhir_error);
                            _menhir_env._menhir_error <- true;
                            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState59)
                    | _ ->
                        assert (not _menhir_env._menhir_error);
                        _menhir_env._menhir_error <- true;
                        let _menhir_stack = Obj.magic _menhir_stack in
                        let (((_menhir_stack, _menhir_s, _), _), _) = _menhir_stack in
                        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
                | _ ->
                    assert (not _menhir_env._menhir_error);
                    _menhir_env._menhir_error <- true;
                    let _menhir_stack = Obj.magic _menhir_stack in
                    let ((_menhir_stack, _menhir_s, _), _) = _menhir_stack in
                    _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
            | _ ->
                assert (not _menhir_env._menhir_error);
                _menhir_env._menhir_error <- true;
                let _menhir_stack = Obj.magic _menhir_stack in
                let ((_menhir_stack, _menhir_s, _), _) = _menhir_stack in
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | MenhirState59 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | ARROW ->
            _menhir_run32 _menhir_env (Obj.magic _menhir_stack)
        | RPAREN ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _menhir_env = _menhir_discard _menhir_env in
            let _tok = _menhir_env._menhir_token in
            (match _tok with
            | EQUALS ->
                let _menhir_stack = Obj.magic _menhir_stack in
                let _menhir_env = _menhir_discard _menhir_env in
                let _tok = _menhir_env._menhir_token in
                (match _tok with
                | BEGIN ->
                    _menhir_run88 _menhir_env (Obj.magic _menhir_stack) MenhirState62
                | CONS ->
                    _menhir_run86 _menhir_env (Obj.magic _menhir_stack) MenhirState62
                | DEBUG ->
                    _menhir_run85 _menhir_env (Obj.magic _menhir_stack) MenhirState62
                | DEREF ->
                    _menhir_run83 _menhir_env (Obj.magic _menhir_stack) MenhirState62
                | EMPTYLIST ->
                    _menhir_run81 _menhir_env (Obj.magic _menhir_stack) MenhirState62
                | EMPTYTREE ->
                    _menhir_run79 _menhir_env (Obj.magic _menhir_stack) MenhirState62
                | GETDATA ->
                    _menhir_run77 _menhir_env (Obj.magic _menhir_stack) MenhirState62
                | GETLST ->
                    _menhir_run75 _menhir_env (Obj.magic _menhir_stack) MenhirState62
                | GETRST ->
                    _menhir_run73 _menhir_env (Obj.magic _menhir_stack) MenhirState62
                | HD ->
                    _menhir_run71 _menhir_env (Obj.magic _menhir_stack) MenhirState62
                | ID _v ->
                    _menhir_run70 _menhir_env (Obj.magic _menhir_stack) MenhirState62 _v
                | IF ->
                    _menhir_run69 _menhir_env (Obj.magic _menhir_stack) MenhirState62
                | INT _v ->
                    _menhir_run68 _menhir_env (Obj.magic _menhir_stack) MenhirState62 _v
                | ISZERO ->
                    _menhir_run66 _menhir_env (Obj.magic _menhir_stack) MenhirState62
                | LET ->
                    _menhir_run63 _menhir_env (Obj.magic _menhir_stack) MenhirState62
                | LETREC ->
                    _menhir_run54 _menhir_env (Obj.magic _menhir_stack) MenhirState62
                | LPAREN ->
                    _menhir_run52 _menhir_env (Obj.magic _menhir_stack) MenhirState62
                | NEWREF ->
                    _menhir_run50 _menhir_env (Obj.magic _menhir_stack) MenhirState62
                | NODE ->
                    _menhir_run48 _menhir_env (Obj.magic _menhir_stack) MenhirState62
                | NULLT_QUESTION ->
                    _menhir_run46 _menhir_env (Obj.magic _menhir_stack) MenhirState62
                | NULL_QUESTION ->
                    _menhir_run44 _menhir_env (Obj.magic _menhir_stack) MenhirState62
                | PAIR ->
                    _menhir_run42 _menhir_env (Obj.magic _menhir_stack) MenhirState62
                | PROC ->
                    _menhir_run16 _menhir_env (Obj.magic _menhir_stack) MenhirState62
                | SET ->
                    _menhir_run13 _menhir_env (Obj.magic _menhir_stack) MenhirState62
                | SETREF ->
                    _menhir_run11 _menhir_env (Obj.magic _menhir_stack) MenhirState62
                | TL ->
                    _menhir_run9 _menhir_env (Obj.magic _menhir_stack) MenhirState62
                | UNIT ->
                    _menhir_run8 _menhir_env (Obj.magic _menhir_stack) MenhirState62
                | UNPAIR ->
                    _menhir_run1 _menhir_env (Obj.magic _menhir_stack) MenhirState62
                | _ ->
                    assert (not _menhir_env._menhir_error);
                    _menhir_env._menhir_error <- true;
                    _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState62)
            | _ ->
                assert (not _menhir_env._menhir_error);
                _menhir_env._menhir_error <- true;
                let _menhir_stack = Obj.magic _menhir_stack in
                let (_menhir_stack, _menhir_s, _) = _menhir_stack in
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | MenhirState79 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | ARROW ->
            _menhir_run32 _menhir_env (Obj.magic _menhir_stack)
        | BEGIN | COMMA | CONS | DEBUG | DEREF | DIVIDED | ELSE | EMPTYLIST | EMPTYTREE | END | EOF | GETDATA | GETLST | GETRST | HD | ID _ | IF | IN | INT _ | ISZERO | LET | LETREC | LPAREN | MINUS | NEWREF | NODE | NULLT_QUESTION | NULL_QUESTION | PAIR | PLUS | PROC | RBRACE | RPAREN | SEMICOLON | SET | SETREF | THEN | TIMES | TL | UNIT | UNPAIR ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let ((_menhir_stack, _menhir_s), _, (e : (Ast.texpr))) = _menhir_stack in
            let _1 = () in
            let _v : (Ast.expr) = 
# 201 "parser.mly"
                         (EmptyTree(e))
# 658 "parser.ml"
             in
            _menhir_goto_expr _menhir_env _menhir_stack _menhir_s _v
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | MenhirState81 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | ARROW ->
            _menhir_run32 _menhir_env (Obj.magic _menhir_stack)
        | BEGIN | COMMA | CONS | DEBUG | DEREF | DIVIDED | ELSE | EMPTYLIST | EMPTYTREE | END | EOF | GETDATA | GETLST | GETRST | HD | ID _ | IF | IN | INT _ | ISZERO | LET | LETREC | LPAREN | MINUS | NEWREF | NODE | NULLT_QUESTION | NULL_QUESTION | PAIR | PLUS | PROC | RBRACE | RPAREN | SEMICOLON | SET | SETREF | THEN | TIMES | TL | UNIT | UNPAIR ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let ((_menhir_stack, _menhir_s), _, (e : (Ast.texpr))) = _menhir_stack in
            let _1 = () in
            let _v : (Ast.expr) = 
# 193 "parser.mly"
                           ( EmptyList(e) )
# 681 "parser.ml"
             in
            _menhir_goto_expr _menhir_env _menhir_stack _menhir_s _v
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | _ ->
        _menhir_fail ()

and _menhir_fail : unit -> 'a =
  fun () ->
    Printf.fprintf Pervasives.stderr "Internal failure -- please contact the parser generator's developers.\n%!";
    assert false

and _menhir_goto_separated_nonempty_list_SEMICOLON_expr_ : _menhir_env -> 'ttv_tail -> _menhir_state -> (Ast.expr list) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    match _menhir_s with
    | MenhirState88 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_stack = Obj.magic _menhir_stack in
        let (x : (Ast.expr list)) = _v in
        let _v : (Ast.expr list) = 
# 144 "/home/nikhil/.opam/system/lib/menhir/standard.mly"
    ( x )
# 708 "parser.ml"
         in
        _menhir_goto_loption_separated_nonempty_list_SEMICOLON_expr__ _menhir_env _menhir_stack _menhir_s _v
    | MenhirState96 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_stack = Obj.magic _menhir_stack in
        let (xs : (Ast.expr list)) = _v in
        let ((_menhir_stack, _menhir_s, (x : (Ast.expr))), _) = _menhir_stack in
        let _2 = () in
        let _v : (Ast.expr list) = 
# 231 "/home/nikhil/.opam/system/lib/menhir/standard.mly"
    ( x :: xs )
# 720 "parser.ml"
         in
        _menhir_goto_separated_nonempty_list_SEMICOLON_expr_ _menhir_env _menhir_stack _menhir_s _v
    | _ ->
        _menhir_fail ()

and _menhir_run94 : _menhir_env -> 'ttv_tail * _menhir_state * (Ast.expr) -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_stack = (_menhir_stack, _menhir_s) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | BEGIN ->
        _menhir_run88 _menhir_env (Obj.magic _menhir_stack) MenhirState94
    | CONS ->
        _menhir_run86 _menhir_env (Obj.magic _menhir_stack) MenhirState94
    | DEBUG ->
        _menhir_run85 _menhir_env (Obj.magic _menhir_stack) MenhirState94
    | DEREF ->
        _menhir_run83 _menhir_env (Obj.magic _menhir_stack) MenhirState94
    | EMPTYLIST ->
        _menhir_run81 _menhir_env (Obj.magic _menhir_stack) MenhirState94
    | EMPTYTREE ->
        _menhir_run79 _menhir_env (Obj.magic _menhir_stack) MenhirState94
    | GETDATA ->
        _menhir_run77 _menhir_env (Obj.magic _menhir_stack) MenhirState94
    | GETLST ->
        _menhir_run75 _menhir_env (Obj.magic _menhir_stack) MenhirState94
    | GETRST ->
        _menhir_run73 _menhir_env (Obj.magic _menhir_stack) MenhirState94
    | HD ->
        _menhir_run71 _menhir_env (Obj.magic _menhir_stack) MenhirState94
    | ID _v ->
        _menhir_run70 _menhir_env (Obj.magic _menhir_stack) MenhirState94 _v
    | IF ->
        _menhir_run69 _menhir_env (Obj.magic _menhir_stack) MenhirState94
    | INT _v ->
        _menhir_run68 _menhir_env (Obj.magic _menhir_stack) MenhirState94 _v
    | ISZERO ->
        _menhir_run66 _menhir_env (Obj.magic _menhir_stack) MenhirState94
    | LET ->
        _menhir_run63 _menhir_env (Obj.magic _menhir_stack) MenhirState94
    | LETREC ->
        _menhir_run54 _menhir_env (Obj.magic _menhir_stack) MenhirState94
    | LPAREN ->
        _menhir_run52 _menhir_env (Obj.magic _menhir_stack) MenhirState94
    | NEWREF ->
        _menhir_run50 _menhir_env (Obj.magic _menhir_stack) MenhirState94
    | NODE ->
        _menhir_run48 _menhir_env (Obj.magic _menhir_stack) MenhirState94
    | NULLT_QUESTION ->
        _menhir_run46 _menhir_env (Obj.magic _menhir_stack) MenhirState94
    | NULL_QUESTION ->
        _menhir_run44 _menhir_env (Obj.magic _menhir_stack) MenhirState94
    | PAIR ->
        _menhir_run42 _menhir_env (Obj.magic _menhir_stack) MenhirState94
    | PROC ->
        _menhir_run16 _menhir_env (Obj.magic _menhir_stack) MenhirState94
    | SET ->
        _menhir_run13 _menhir_env (Obj.magic _menhir_stack) MenhirState94
    | SETREF ->
        _menhir_run11 _menhir_env (Obj.magic _menhir_stack) MenhirState94
    | TL ->
        _menhir_run9 _menhir_env (Obj.magic _menhir_stack) MenhirState94
    | UNIT ->
        _menhir_run8 _menhir_env (Obj.magic _menhir_stack) MenhirState94
    | UNPAIR ->
        _menhir_run1 _menhir_env (Obj.magic _menhir_stack) MenhirState94
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState94

and _menhir_run98 : _menhir_env -> 'ttv_tail * _menhir_state * (Ast.expr) -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_stack = (_menhir_stack, _menhir_s) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | BEGIN ->
        _menhir_run88 _menhir_env (Obj.magic _menhir_stack) MenhirState98
    | CONS ->
        _menhir_run86 _menhir_env (Obj.magic _menhir_stack) MenhirState98
    | DEBUG ->
        _menhir_run85 _menhir_env (Obj.magic _menhir_stack) MenhirState98
    | DEREF ->
        _menhir_run83 _menhir_env (Obj.magic _menhir_stack) MenhirState98
    | EMPTYLIST ->
        _menhir_run81 _menhir_env (Obj.magic _menhir_stack) MenhirState98
    | EMPTYTREE ->
        _menhir_run79 _menhir_env (Obj.magic _menhir_stack) MenhirState98
    | GETDATA ->
        _menhir_run77 _menhir_env (Obj.magic _menhir_stack) MenhirState98
    | GETLST ->
        _menhir_run75 _menhir_env (Obj.magic _menhir_stack) MenhirState98
    | GETRST ->
        _menhir_run73 _menhir_env (Obj.magic _menhir_stack) MenhirState98
    | HD ->
        _menhir_run71 _menhir_env (Obj.magic _menhir_stack) MenhirState98
    | ID _v ->
        _menhir_run70 _menhir_env (Obj.magic _menhir_stack) MenhirState98 _v
    | IF ->
        _menhir_run69 _menhir_env (Obj.magic _menhir_stack) MenhirState98
    | INT _v ->
        _menhir_run68 _menhir_env (Obj.magic _menhir_stack) MenhirState98 _v
    | ISZERO ->
        _menhir_run66 _menhir_env (Obj.magic _menhir_stack) MenhirState98
    | LET ->
        _menhir_run63 _menhir_env (Obj.magic _menhir_stack) MenhirState98
    | LETREC ->
        _menhir_run54 _menhir_env (Obj.magic _menhir_stack) MenhirState98
    | LPAREN ->
        _menhir_run52 _menhir_env (Obj.magic _menhir_stack) MenhirState98
    | NEWREF ->
        _menhir_run50 _menhir_env (Obj.magic _menhir_stack) MenhirState98
    | NODE ->
        _menhir_run48 _menhir_env (Obj.magic _menhir_stack) MenhirState98
    | NULLT_QUESTION ->
        _menhir_run46 _menhir_env (Obj.magic _menhir_stack) MenhirState98
    | NULL_QUESTION ->
        _menhir_run44 _menhir_env (Obj.magic _menhir_stack) MenhirState98
    | PAIR ->
        _menhir_run42 _menhir_env (Obj.magic _menhir_stack) MenhirState98
    | PROC ->
        _menhir_run16 _menhir_env (Obj.magic _menhir_stack) MenhirState98
    | SET ->
        _menhir_run13 _menhir_env (Obj.magic _menhir_stack) MenhirState98
    | SETREF ->
        _menhir_run11 _menhir_env (Obj.magic _menhir_stack) MenhirState98
    | TL ->
        _menhir_run9 _menhir_env (Obj.magic _menhir_stack) MenhirState98
    | UNIT ->
        _menhir_run8 _menhir_env (Obj.magic _menhir_stack) MenhirState98
    | UNPAIR ->
        _menhir_run1 _menhir_env (Obj.magic _menhir_stack) MenhirState98
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState98

and _menhir_run102 : _menhir_env -> 'ttv_tail * _menhir_state * (Ast.expr) -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_stack = (_menhir_stack, _menhir_s) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | BEGIN ->
        _menhir_run88 _menhir_env (Obj.magic _menhir_stack) MenhirState102
    | CONS ->
        _menhir_run86 _menhir_env (Obj.magic _menhir_stack) MenhirState102
    | DEBUG ->
        _menhir_run85 _menhir_env (Obj.magic _menhir_stack) MenhirState102
    | DEREF ->
        _menhir_run83 _menhir_env (Obj.magic _menhir_stack) MenhirState102
    | EMPTYLIST ->
        _menhir_run81 _menhir_env (Obj.magic _menhir_stack) MenhirState102
    | EMPTYTREE ->
        _menhir_run79 _menhir_env (Obj.magic _menhir_stack) MenhirState102
    | GETDATA ->
        _menhir_run77 _menhir_env (Obj.magic _menhir_stack) MenhirState102
    | GETLST ->
        _menhir_run75 _menhir_env (Obj.magic _menhir_stack) MenhirState102
    | GETRST ->
        _menhir_run73 _menhir_env (Obj.magic _menhir_stack) MenhirState102
    | HD ->
        _menhir_run71 _menhir_env (Obj.magic _menhir_stack) MenhirState102
    | ID _v ->
        _menhir_run70 _menhir_env (Obj.magic _menhir_stack) MenhirState102 _v
    | IF ->
        _menhir_run69 _menhir_env (Obj.magic _menhir_stack) MenhirState102
    | INT _v ->
        _menhir_run68 _menhir_env (Obj.magic _menhir_stack) MenhirState102 _v
    | ISZERO ->
        _menhir_run66 _menhir_env (Obj.magic _menhir_stack) MenhirState102
    | LET ->
        _menhir_run63 _menhir_env (Obj.magic _menhir_stack) MenhirState102
    | LETREC ->
        _menhir_run54 _menhir_env (Obj.magic _menhir_stack) MenhirState102
    | LPAREN ->
        _menhir_run52 _menhir_env (Obj.magic _menhir_stack) MenhirState102
    | NEWREF ->
        _menhir_run50 _menhir_env (Obj.magic _menhir_stack) MenhirState102
    | NODE ->
        _menhir_run48 _menhir_env (Obj.magic _menhir_stack) MenhirState102
    | NULLT_QUESTION ->
        _menhir_run46 _menhir_env (Obj.magic _menhir_stack) MenhirState102
    | NULL_QUESTION ->
        _menhir_run44 _menhir_env (Obj.magic _menhir_stack) MenhirState102
    | PAIR ->
        _menhir_run42 _menhir_env (Obj.magic _menhir_stack) MenhirState102
    | PROC ->
        _menhir_run16 _menhir_env (Obj.magic _menhir_stack) MenhirState102
    | SET ->
        _menhir_run13 _menhir_env (Obj.magic _menhir_stack) MenhirState102
    | SETREF ->
        _menhir_run11 _menhir_env (Obj.magic _menhir_stack) MenhirState102
    | TL ->
        _menhir_run9 _menhir_env (Obj.magic _menhir_stack) MenhirState102
    | UNIT ->
        _menhir_run8 _menhir_env (Obj.magic _menhir_stack) MenhirState102
    | UNPAIR ->
        _menhir_run1 _menhir_env (Obj.magic _menhir_stack) MenhirState102
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState102

and _menhir_run100 : _menhir_env -> 'ttv_tail * _menhir_state * (Ast.expr) -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_stack = (_menhir_stack, _menhir_s) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | BEGIN ->
        _menhir_run88 _menhir_env (Obj.magic _menhir_stack) MenhirState100
    | CONS ->
        _menhir_run86 _menhir_env (Obj.magic _menhir_stack) MenhirState100
    | DEBUG ->
        _menhir_run85 _menhir_env (Obj.magic _menhir_stack) MenhirState100
    | DEREF ->
        _menhir_run83 _menhir_env (Obj.magic _menhir_stack) MenhirState100
    | EMPTYLIST ->
        _menhir_run81 _menhir_env (Obj.magic _menhir_stack) MenhirState100
    | EMPTYTREE ->
        _menhir_run79 _menhir_env (Obj.magic _menhir_stack) MenhirState100
    | GETDATA ->
        _menhir_run77 _menhir_env (Obj.magic _menhir_stack) MenhirState100
    | GETLST ->
        _menhir_run75 _menhir_env (Obj.magic _menhir_stack) MenhirState100
    | GETRST ->
        _menhir_run73 _menhir_env (Obj.magic _menhir_stack) MenhirState100
    | HD ->
        _menhir_run71 _menhir_env (Obj.magic _menhir_stack) MenhirState100
    | ID _v ->
        _menhir_run70 _menhir_env (Obj.magic _menhir_stack) MenhirState100 _v
    | IF ->
        _menhir_run69 _menhir_env (Obj.magic _menhir_stack) MenhirState100
    | INT _v ->
        _menhir_run68 _menhir_env (Obj.magic _menhir_stack) MenhirState100 _v
    | ISZERO ->
        _menhir_run66 _menhir_env (Obj.magic _menhir_stack) MenhirState100
    | LET ->
        _menhir_run63 _menhir_env (Obj.magic _menhir_stack) MenhirState100
    | LETREC ->
        _menhir_run54 _menhir_env (Obj.magic _menhir_stack) MenhirState100
    | LPAREN ->
        _menhir_run52 _menhir_env (Obj.magic _menhir_stack) MenhirState100
    | NEWREF ->
        _menhir_run50 _menhir_env (Obj.magic _menhir_stack) MenhirState100
    | NODE ->
        _menhir_run48 _menhir_env (Obj.magic _menhir_stack) MenhirState100
    | NULLT_QUESTION ->
        _menhir_run46 _menhir_env (Obj.magic _menhir_stack) MenhirState100
    | NULL_QUESTION ->
        _menhir_run44 _menhir_env (Obj.magic _menhir_stack) MenhirState100
    | PAIR ->
        _menhir_run42 _menhir_env (Obj.magic _menhir_stack) MenhirState100
    | PROC ->
        _menhir_run16 _menhir_env (Obj.magic _menhir_stack) MenhirState100
    | SET ->
        _menhir_run13 _menhir_env (Obj.magic _menhir_stack) MenhirState100
    | SETREF ->
        _menhir_run11 _menhir_env (Obj.magic _menhir_stack) MenhirState100
    | TL ->
        _menhir_run9 _menhir_env (Obj.magic _menhir_stack) MenhirState100
    | UNIT ->
        _menhir_run8 _menhir_env (Obj.magic _menhir_stack) MenhirState100
    | UNPAIR ->
        _menhir_run1 _menhir_env (Obj.magic _menhir_stack) MenhirState100
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState100

and _menhir_run20 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_env = _menhir_discard _menhir_env in
    let _menhir_stack = Obj.magic _menhir_stack in
    let _1 = () in
    let _v : (Ast.texpr) = 
# 220 "parser.mly"
               (UnitType)
# 1002 "parser.ml"
     in
    _menhir_goto_texpr _menhir_env _menhir_stack _menhir_s _v

and _menhir_run21 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_stack = (_menhir_stack, _menhir_s) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | BOOLTYPE ->
        _menhir_run27 _menhir_env (Obj.magic _menhir_stack) MenhirState21
    | INTTYPE ->
        _menhir_run26 _menhir_env (Obj.magic _menhir_stack) MenhirState21
    | LESS_THAN ->
        _menhir_run25 _menhir_env (Obj.magic _menhir_stack) MenhirState21
    | LISTTYPE ->
        _menhir_run24 _menhir_env (Obj.magic _menhir_stack) MenhirState21
    | LPAREN ->
        _menhir_run23 _menhir_env (Obj.magic _menhir_stack) MenhirState21
    | REFTYPE ->
        _menhir_run22 _menhir_env (Obj.magic _menhir_stack) MenhirState21
    | TREETYPE ->
        _menhir_run21 _menhir_env (Obj.magic _menhir_stack) MenhirState21
    | UNITTYPE ->
        _menhir_run20 _menhir_env (Obj.magic _menhir_stack) MenhirState21
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState21

and _menhir_run22 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_stack = (_menhir_stack, _menhir_s) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | BOOLTYPE ->
        _menhir_run27 _menhir_env (Obj.magic _menhir_stack) MenhirState22
    | INTTYPE ->
        _menhir_run26 _menhir_env (Obj.magic _menhir_stack) MenhirState22
    | LESS_THAN ->
        _menhir_run25 _menhir_env (Obj.magic _menhir_stack) MenhirState22
    | LISTTYPE ->
        _menhir_run24 _menhir_env (Obj.magic _menhir_stack) MenhirState22
    | LPAREN ->
        _menhir_run23 _menhir_env (Obj.magic _menhir_stack) MenhirState22
    | REFTYPE ->
        _menhir_run22 _menhir_env (Obj.magic _menhir_stack) MenhirState22
    | TREETYPE ->
        _menhir_run21 _menhir_env (Obj.magic _menhir_stack) MenhirState22
    | UNITTYPE ->
        _menhir_run20 _menhir_env (Obj.magic _menhir_stack) MenhirState22
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState22

and _menhir_run23 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_stack = (_menhir_stack, _menhir_s) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | BOOLTYPE ->
        _menhir_run27 _menhir_env (Obj.magic _menhir_stack) MenhirState23
    | INTTYPE ->
        _menhir_run26 _menhir_env (Obj.magic _menhir_stack) MenhirState23
    | LESS_THAN ->
        _menhir_run25 _menhir_env (Obj.magic _menhir_stack) MenhirState23
    | LISTTYPE ->
        _menhir_run24 _menhir_env (Obj.magic _menhir_stack) MenhirState23
    | LPAREN ->
        _menhir_run23 _menhir_env (Obj.magic _menhir_stack) MenhirState23
    | REFTYPE ->
        _menhir_run22 _menhir_env (Obj.magic _menhir_stack) MenhirState23
    | TREETYPE ->
        _menhir_run21 _menhir_env (Obj.magic _menhir_stack) MenhirState23
    | UNITTYPE ->
        _menhir_run20 _menhir_env (Obj.magic _menhir_stack) MenhirState23
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState23

and _menhir_run24 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_stack = (_menhir_stack, _menhir_s) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | BOOLTYPE ->
        _menhir_run27 _menhir_env (Obj.magic _menhir_stack) MenhirState24
    | INTTYPE ->
        _menhir_run26 _menhir_env (Obj.magic _menhir_stack) MenhirState24
    | LESS_THAN ->
        _menhir_run25 _menhir_env (Obj.magic _menhir_stack) MenhirState24
    | LISTTYPE ->
        _menhir_run24 _menhir_env (Obj.magic _menhir_stack) MenhirState24
    | LPAREN ->
        _menhir_run23 _menhir_env (Obj.magic _menhir_stack) MenhirState24
    | REFTYPE ->
        _menhir_run22 _menhir_env (Obj.magic _menhir_stack) MenhirState24
    | TREETYPE ->
        _menhir_run21 _menhir_env (Obj.magic _menhir_stack) MenhirState24
    | UNITTYPE ->
        _menhir_run20 _menhir_env (Obj.magic _menhir_stack) MenhirState24
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState24

and _menhir_run25 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_stack = (_menhir_stack, _menhir_s) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | BOOLTYPE ->
        _menhir_run27 _menhir_env (Obj.magic _menhir_stack) MenhirState25
    | INTTYPE ->
        _menhir_run26 _menhir_env (Obj.magic _menhir_stack) MenhirState25
    | LESS_THAN ->
        _menhir_run25 _menhir_env (Obj.magic _menhir_stack) MenhirState25
    | LISTTYPE ->
        _menhir_run24 _menhir_env (Obj.magic _menhir_stack) MenhirState25
    | LPAREN ->
        _menhir_run23 _menhir_env (Obj.magic _menhir_stack) MenhirState25
    | REFTYPE ->
        _menhir_run22 _menhir_env (Obj.magic _menhir_stack) MenhirState25
    | TREETYPE ->
        _menhir_run21 _menhir_env (Obj.magic _menhir_stack) MenhirState25
    | UNITTYPE ->
        _menhir_run20 _menhir_env (Obj.magic _menhir_stack) MenhirState25
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState25

and _menhir_run26 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_env = _menhir_discard _menhir_env in
    let _menhir_stack = Obj.magic _menhir_stack in
    let _1 = () in
    let _v : (Ast.texpr) = 
# 215 "parser.mly"
              ( IntType )
# 1149 "parser.ml"
     in
    _menhir_goto_texpr _menhir_env _menhir_stack _menhir_s _v

and _menhir_run27 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_env = _menhir_discard _menhir_env in
    let _menhir_stack = Obj.magic _menhir_stack in
    let _1 = () in
    let _v : (Ast.texpr) = 
# 216 "parser.mly"
               ( BoolType )
# 1161 "parser.ml"
     in
    _menhir_goto_texpr _menhir_env _menhir_stack _menhir_s _v

and _menhir_goto_expr : _menhir_env -> 'ttv_tail -> _menhir_state -> (Ast.expr) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
    match _menhir_s with
    | MenhirState96 | MenhirState88 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | DIVIDED ->
            _menhir_run100 _menhir_env (Obj.magic _menhir_stack) MenhirState93
        | MINUS ->
            _menhir_run102 _menhir_env (Obj.magic _menhir_stack) MenhirState93
        | PLUS ->
            _menhir_run98 _menhir_env (Obj.magic _menhir_stack) MenhirState93
        | SEMICOLON ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _menhir_s = MenhirState93 in
            let _menhir_stack = (_menhir_stack, _menhir_s) in
            let _menhir_env = _menhir_discard _menhir_env in
            let _tok = _menhir_env._menhir_token in
            (match _tok with
            | BEGIN ->
                _menhir_run88 _menhir_env (Obj.magic _menhir_stack) MenhirState96
            | CONS ->
                _menhir_run86 _menhir_env (Obj.magic _menhir_stack) MenhirState96
            | DEBUG ->
                _menhir_run85 _menhir_env (Obj.magic _menhir_stack) MenhirState96
            | DEREF ->
                _menhir_run83 _menhir_env (Obj.magic _menhir_stack) MenhirState96
            | EMPTYLIST ->
                _menhir_run81 _menhir_env (Obj.magic _menhir_stack) MenhirState96
            | EMPTYTREE ->
                _menhir_run79 _menhir_env (Obj.magic _menhir_stack) MenhirState96
            | GETDATA ->
                _menhir_run77 _menhir_env (Obj.magic _menhir_stack) MenhirState96
            | GETLST ->
                _menhir_run75 _menhir_env (Obj.magic _menhir_stack) MenhirState96
            | GETRST ->
                _menhir_run73 _menhir_env (Obj.magic _menhir_stack) MenhirState96
            | HD ->
                _menhir_run71 _menhir_env (Obj.magic _menhir_stack) MenhirState96
            | ID _v ->
                _menhir_run70 _menhir_env (Obj.magic _menhir_stack) MenhirState96 _v
            | IF ->
                _menhir_run69 _menhir_env (Obj.magic _menhir_stack) MenhirState96
            | INT _v ->
                _menhir_run68 _menhir_env (Obj.magic _menhir_stack) MenhirState96 _v
            | ISZERO ->
                _menhir_run66 _menhir_env (Obj.magic _menhir_stack) MenhirState96
            | LET ->
                _menhir_run63 _menhir_env (Obj.magic _menhir_stack) MenhirState96
            | LETREC ->
                _menhir_run54 _menhir_env (Obj.magic _menhir_stack) MenhirState96
            | LPAREN ->
                _menhir_run52 _menhir_env (Obj.magic _menhir_stack) MenhirState96
            | NEWREF ->
                _menhir_run50 _menhir_env (Obj.magic _menhir_stack) MenhirState96
            | NODE ->
                _menhir_run48 _menhir_env (Obj.magic _menhir_stack) MenhirState96
            | NULLT_QUESTION ->
                _menhir_run46 _menhir_env (Obj.magic _menhir_stack) MenhirState96
            | NULL_QUESTION ->
                _menhir_run44 _menhir_env (Obj.magic _menhir_stack) MenhirState96
            | PAIR ->
                _menhir_run42 _menhir_env (Obj.magic _menhir_stack) MenhirState96
            | PROC ->
                _menhir_run16 _menhir_env (Obj.magic _menhir_stack) MenhirState96
            | SET ->
                _menhir_run13 _menhir_env (Obj.magic _menhir_stack) MenhirState96
            | SETREF ->
                _menhir_run11 _menhir_env (Obj.magic _menhir_stack) MenhirState96
            | TL ->
                _menhir_run9 _menhir_env (Obj.magic _menhir_stack) MenhirState96
            | UNIT ->
                _menhir_run8 _menhir_env (Obj.magic _menhir_stack) MenhirState96
            | UNPAIR ->
                _menhir_run1 _menhir_env (Obj.magic _menhir_stack) MenhirState96
            | _ ->
                assert (not _menhir_env._menhir_error);
                _menhir_env._menhir_error <- true;
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState96)
        | TIMES ->
            _menhir_run94 _menhir_env (Obj.magic _menhir_stack) MenhirState93
        | END ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, (x : (Ast.expr))) = _menhir_stack in
            let _v : (Ast.expr list) = 
# 229 "/home/nikhil/.opam/system/lib/menhir/standard.mly"
    ( [ x ] )
# 1255 "parser.ml"
             in
            _menhir_goto_separated_nonempty_list_SEMICOLON_expr_ _menhir_env _menhir_stack _menhir_s _v
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState93)
    | MenhirState94 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_stack = Obj.magic _menhir_stack in
        let (((_menhir_stack, _menhir_s, (e1 : (Ast.expr))), _), _, (e2 : (Ast.expr))) = _menhir_stack in
        let _2 = () in
        let _v : (Ast.expr) = 
# 170 "parser.mly"
                                  ( Mul(e1,e2) )
# 1270 "parser.ml"
         in
        _menhir_goto_expr _menhir_env _menhir_stack _menhir_s _v
    | MenhirState98 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | DIVIDED ->
            _menhir_run100 _menhir_env (Obj.magic _menhir_stack) MenhirState99
        | TIMES ->
            _menhir_run94 _menhir_env (Obj.magic _menhir_stack) MenhirState99
        | BEGIN | COMMA | CONS | DEBUG | DEREF | ELSE | EMPTYLIST | EMPTYTREE | END | EOF | GETDATA | GETLST | GETRST | HD | ID _ | IF | IN | INT _ | ISZERO | LET | LETREC | LPAREN | MINUS | NEWREF | NODE | NULLT_QUESTION | NULL_QUESTION | PAIR | PLUS | PROC | RBRACE | RPAREN | SEMICOLON | SET | SETREF | THEN | TL | UNIT | UNPAIR ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let (((_menhir_stack, _menhir_s, (e1 : (Ast.expr))), _), _, (e2 : (Ast.expr))) = _menhir_stack in
            let _2 = () in
            let _v : (Ast.expr) = 
# 168 "parser.mly"
                                 ( Add(e1,e2) )
# 1289 "parser.ml"
             in
            _menhir_goto_expr _menhir_env _menhir_stack _menhir_s _v
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState99)
    | MenhirState100 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_stack = Obj.magic _menhir_stack in
        let (((_menhir_stack, _menhir_s, (e1 : (Ast.expr))), _), _, (e2 : (Ast.expr))) = _menhir_stack in
        let _2 = () in
        let _v : (Ast.expr) = 
# 171 "parser.mly"
                                    ( Div(e1,e2) )
# 1304 "parser.ml"
         in
        _menhir_goto_expr _menhir_env _menhir_stack _menhir_s _v
    | MenhirState102 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | DIVIDED ->
            _menhir_run100 _menhir_env (Obj.magic _menhir_stack) MenhirState103
        | TIMES ->
            _menhir_run94 _menhir_env (Obj.magic _menhir_stack) MenhirState103
        | BEGIN | COMMA | CONS | DEBUG | DEREF | ELSE | EMPTYLIST | EMPTYTREE | END | EOF | GETDATA | GETLST | GETRST | HD | ID _ | IF | IN | INT _ | ISZERO | LET | LETREC | LPAREN | MINUS | NEWREF | NODE | NULLT_QUESTION | NULL_QUESTION | PAIR | PLUS | PROC | RBRACE | RPAREN | SEMICOLON | SET | SETREF | THEN | TL | UNIT | UNPAIR ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let (((_menhir_stack, _menhir_s, (e1 : (Ast.expr))), _), _, (e2 : (Ast.expr))) = _menhir_stack in
            let _2 = () in
            let _v : (Ast.expr) = 
# 169 "parser.mly"
                                  ( Sub(e1,e2) )
# 1323 "parser.ml"
             in
            _menhir_goto_expr _menhir_env _menhir_stack _menhir_s _v
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState103)
    | MenhirState87 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | COMMA ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _menhir_s = MenhirState104 in
            let _menhir_stack = (_menhir_stack, _menhir_s) in
            let _menhir_env = _menhir_discard _menhir_env in
            let _tok = _menhir_env._menhir_token in
            (match _tok with
            | BEGIN ->
                _menhir_run88 _menhir_env (Obj.magic _menhir_stack) MenhirState105
            | CONS ->
                _menhir_run86 _menhir_env (Obj.magic _menhir_stack) MenhirState105
            | DEBUG ->
                _menhir_run85 _menhir_env (Obj.magic _menhir_stack) MenhirState105
            | DEREF ->
                _menhir_run83 _menhir_env (Obj.magic _menhir_stack) MenhirState105
            | EMPTYLIST ->
                _menhir_run81 _menhir_env (Obj.magic _menhir_stack) MenhirState105
            | EMPTYTREE ->
                _menhir_run79 _menhir_env (Obj.magic _menhir_stack) MenhirState105
            | GETDATA ->
                _menhir_run77 _menhir_env (Obj.magic _menhir_stack) MenhirState105
            | GETLST ->
                _menhir_run75 _menhir_env (Obj.magic _menhir_stack) MenhirState105
            | GETRST ->
                _menhir_run73 _menhir_env (Obj.magic _menhir_stack) MenhirState105
            | HD ->
                _menhir_run71 _menhir_env (Obj.magic _menhir_stack) MenhirState105
            | ID _v ->
                _menhir_run70 _menhir_env (Obj.magic _menhir_stack) MenhirState105 _v
            | IF ->
                _menhir_run69 _menhir_env (Obj.magic _menhir_stack) MenhirState105
            | INT _v ->
                _menhir_run68 _menhir_env (Obj.magic _menhir_stack) MenhirState105 _v
            | ISZERO ->
                _menhir_run66 _menhir_env (Obj.magic _menhir_stack) MenhirState105
            | LET ->
                _menhir_run63 _menhir_env (Obj.magic _menhir_stack) MenhirState105
            | LETREC ->
                _menhir_run54 _menhir_env (Obj.magic _menhir_stack) MenhirState105
            | LPAREN ->
                _menhir_run52 _menhir_env (Obj.magic _menhir_stack) MenhirState105
            | NEWREF ->
                _menhir_run50 _menhir_env (Obj.magic _menhir_stack) MenhirState105
            | NODE ->
                _menhir_run48 _menhir_env (Obj.magic _menhir_stack) MenhirState105
            | NULLT_QUESTION ->
                _menhir_run46 _menhir_env (Obj.magic _menhir_stack) MenhirState105
            | NULL_QUESTION ->
                _menhir_run44 _menhir_env (Obj.magic _menhir_stack) MenhirState105
            | PAIR ->
                _menhir_run42 _menhir_env (Obj.magic _menhir_stack) MenhirState105
            | PROC ->
                _menhir_run16 _menhir_env (Obj.magic _menhir_stack) MenhirState105
            | SET ->
                _menhir_run13 _menhir_env (Obj.magic _menhir_stack) MenhirState105
            | SETREF ->
                _menhir_run11 _menhir_env (Obj.magic _menhir_stack) MenhirState105
            | TL ->
                _menhir_run9 _menhir_env (Obj.magic _menhir_stack) MenhirState105
            | UNIT ->
                _menhir_run8 _menhir_env (Obj.magic _menhir_stack) MenhirState105
            | UNPAIR ->
                _menhir_run1 _menhir_env (Obj.magic _menhir_stack) MenhirState105
            | _ ->
                assert (not _menhir_env._menhir_error);
                _menhir_env._menhir_error <- true;
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState105)
        | DIVIDED ->
            _menhir_run100 _menhir_env (Obj.magic _menhir_stack) MenhirState104
        | MINUS ->
            _menhir_run102 _menhir_env (Obj.magic _menhir_stack) MenhirState104
        | PLUS ->
            _menhir_run98 _menhir_env (Obj.magic _menhir_stack) MenhirState104
        | TIMES ->
            _menhir_run94 _menhir_env (Obj.magic _menhir_stack) MenhirState104
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState104)
    | MenhirState105 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | DIVIDED ->
            _menhir_run100 _menhir_env (Obj.magic _menhir_stack) MenhirState106
        | MINUS ->
            _menhir_run102 _menhir_env (Obj.magic _menhir_stack) MenhirState106
        | PLUS ->
            _menhir_run98 _menhir_env (Obj.magic _menhir_stack) MenhirState106
        | RPAREN ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _menhir_s = MenhirState106 in
            let _menhir_env = _menhir_discard _menhir_env in
            let _menhir_stack = Obj.magic _menhir_stack in
            let ((((_menhir_stack, _menhir_s), _, (e1 : (Ast.expr))), _), _, (e2 : (Ast.expr))) = _menhir_stack in
            let _6 = () in
            let _4 = () in
            let _2 = () in
            let _1 = () in
            let _v : (Ast.expr) = 
# 197 "parser.mly"
                                                        ( Cons(e1, e2) )
# 1438 "parser.ml"
             in
            _menhir_goto_expr _menhir_env _menhir_stack _menhir_s _v
        | TIMES ->
            _menhir_run94 _menhir_env (Obj.magic _menhir_stack) MenhirState106
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState106)
    | MenhirState84 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | DIVIDED ->
            _menhir_run100 _menhir_env (Obj.magic _menhir_stack) MenhirState108
        | MINUS ->
            _menhir_run102 _menhir_env (Obj.magic _menhir_stack) MenhirState108
        | PLUS ->
            _menhir_run98 _menhir_env (Obj.magic _menhir_stack) MenhirState108
        | RPAREN ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _menhir_s = MenhirState108 in
            let _menhir_env = _menhir_discard _menhir_env in
            let _menhir_stack = Obj.magic _menhir_stack in
            let ((_menhir_stack, _menhir_s), _, (e : (Ast.expr))) = _menhir_stack in
            let _4 = () in
            let _2 = () in
            let _1 = () in
            let _v : (Ast.expr) = 
# 178 "parser.mly"
                                      ( DeRef(e) )
# 1470 "parser.ml"
             in
            _menhir_goto_expr _menhir_env _menhir_stack _menhir_s _v
        | TIMES ->
            _menhir_run94 _menhir_env (Obj.magic _menhir_stack) MenhirState108
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState108)
    | MenhirState78 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | DIVIDED ->
            _menhir_run100 _menhir_env (Obj.magic _menhir_stack) MenhirState110
        | MINUS ->
            _menhir_run102 _menhir_env (Obj.magic _menhir_stack) MenhirState110
        | PLUS ->
            _menhir_run98 _menhir_env (Obj.magic _menhir_stack) MenhirState110
        | RPAREN ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _menhir_s = MenhirState110 in
            let _menhir_env = _menhir_discard _menhir_env in
            let _menhir_stack = Obj.magic _menhir_stack in
            let ((_menhir_stack, _menhir_s), _, (e : (Ast.expr))) = _menhir_stack in
            let _4 = () in
            let _2 = () in
            let _1 = () in
            let _v : (Ast.expr) = 
# 204 "parser.mly"
                                      (GetData(e))
# 1502 "parser.ml"
             in
            _menhir_goto_expr _menhir_env _menhir_stack _menhir_s _v
        | TIMES ->
            _menhir_run94 _menhir_env (Obj.magic _menhir_stack) MenhirState110
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState110)
    | MenhirState76 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | DIVIDED ->
            _menhir_run100 _menhir_env (Obj.magic _menhir_stack) MenhirState112
        | MINUS ->
            _menhir_run102 _menhir_env (Obj.magic _menhir_stack) MenhirState112
        | PLUS ->
            _menhir_run98 _menhir_env (Obj.magic _menhir_stack) MenhirState112
        | RPAREN ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _menhir_s = MenhirState112 in
            let _menhir_env = _menhir_discard _menhir_env in
            let _menhir_stack = Obj.magic _menhir_stack in
            let ((_menhir_stack, _menhir_s), _, (e : (Ast.expr))) = _menhir_stack in
            let _4 = () in
            let _2 = () in
            let _1 = () in
            let _v : (Ast.expr) = 
# 205 "parser.mly"
                                      (GetLST(e))
# 1534 "parser.ml"
             in
            _menhir_goto_expr _menhir_env _menhir_stack _menhir_s _v
        | TIMES ->
            _menhir_run94 _menhir_env (Obj.magic _menhir_stack) MenhirState112
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState112)
    | MenhirState74 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | DIVIDED ->
            _menhir_run100 _menhir_env (Obj.magic _menhir_stack) MenhirState114
        | MINUS ->
            _menhir_run102 _menhir_env (Obj.magic _menhir_stack) MenhirState114
        | PLUS ->
            _menhir_run98 _menhir_env (Obj.magic _menhir_stack) MenhirState114
        | RPAREN ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _menhir_s = MenhirState114 in
            let _menhir_env = _menhir_discard _menhir_env in
            let _menhir_stack = Obj.magic _menhir_stack in
            let ((_menhir_stack, _menhir_s), _, (e1 : (Ast.expr))) = _menhir_stack in
            let _4 = () in
            let _2 = () in
            let _1 = () in
            let _v : (Ast.expr) = 
# 206 "parser.mly"
                                       (GetRST(e1))
# 1566 "parser.ml"
             in
            _menhir_goto_expr _menhir_env _menhir_stack _menhir_s _v
        | TIMES ->
            _menhir_run94 _menhir_env (Obj.magic _menhir_stack) MenhirState114
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState114)
    | MenhirState72 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | DIVIDED ->
            _menhir_run100 _menhir_env (Obj.magic _menhir_stack) MenhirState116
        | MINUS ->
            _menhir_run102 _menhir_env (Obj.magic _menhir_stack) MenhirState116
        | PLUS ->
            _menhir_run98 _menhir_env (Obj.magic _menhir_stack) MenhirState116
        | RPAREN ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _menhir_s = MenhirState116 in
            let _menhir_env = _menhir_discard _menhir_env in
            let _menhir_stack = Obj.magic _menhir_stack in
            let ((_menhir_stack, _menhir_s), _, (e : (Ast.expr))) = _menhir_stack in
            let _4 = () in
            let _2 = () in
            let _1 = () in
            let _v : (Ast.expr) = 
# 194 "parser.mly"
                                   ( Hd(e) )
# 1598 "parser.ml"
             in
            _menhir_goto_expr _menhir_env _menhir_stack _menhir_s _v
        | TIMES ->
            _menhir_run94 _menhir_env (Obj.magic _menhir_stack) MenhirState116
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState116)
    | MenhirState69 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | DIVIDED ->
            _menhir_run100 _menhir_env (Obj.magic _menhir_stack) MenhirState118
        | MINUS ->
            _menhir_run102 _menhir_env (Obj.magic _menhir_stack) MenhirState118
        | PLUS ->
            _menhir_run98 _menhir_env (Obj.magic _menhir_stack) MenhirState118
        | THEN ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _menhir_s = MenhirState118 in
            let _menhir_stack = (_menhir_stack, _menhir_s) in
            let _menhir_env = _menhir_discard _menhir_env in
            let _tok = _menhir_env._menhir_token in
            (match _tok with
            | BEGIN ->
                _menhir_run88 _menhir_env (Obj.magic _menhir_stack) MenhirState119
            | CONS ->
                _menhir_run86 _menhir_env (Obj.magic _menhir_stack) MenhirState119
            | DEBUG ->
                _menhir_run85 _menhir_env (Obj.magic _menhir_stack) MenhirState119
            | DEREF ->
                _menhir_run83 _menhir_env (Obj.magic _menhir_stack) MenhirState119
            | EMPTYLIST ->
                _menhir_run81 _menhir_env (Obj.magic _menhir_stack) MenhirState119
            | EMPTYTREE ->
                _menhir_run79 _menhir_env (Obj.magic _menhir_stack) MenhirState119
            | GETDATA ->
                _menhir_run77 _menhir_env (Obj.magic _menhir_stack) MenhirState119
            | GETLST ->
                _menhir_run75 _menhir_env (Obj.magic _menhir_stack) MenhirState119
            | GETRST ->
                _menhir_run73 _menhir_env (Obj.magic _menhir_stack) MenhirState119
            | HD ->
                _menhir_run71 _menhir_env (Obj.magic _menhir_stack) MenhirState119
            | ID _v ->
                _menhir_run70 _menhir_env (Obj.magic _menhir_stack) MenhirState119 _v
            | IF ->
                _menhir_run69 _menhir_env (Obj.magic _menhir_stack) MenhirState119
            | INT _v ->
                _menhir_run68 _menhir_env (Obj.magic _menhir_stack) MenhirState119 _v
            | ISZERO ->
                _menhir_run66 _menhir_env (Obj.magic _menhir_stack) MenhirState119
            | LET ->
                _menhir_run63 _menhir_env (Obj.magic _menhir_stack) MenhirState119
            | LETREC ->
                _menhir_run54 _menhir_env (Obj.magic _menhir_stack) MenhirState119
            | LPAREN ->
                _menhir_run52 _menhir_env (Obj.magic _menhir_stack) MenhirState119
            | NEWREF ->
                _menhir_run50 _menhir_env (Obj.magic _menhir_stack) MenhirState119
            | NODE ->
                _menhir_run48 _menhir_env (Obj.magic _menhir_stack) MenhirState119
            | NULLT_QUESTION ->
                _menhir_run46 _menhir_env (Obj.magic _menhir_stack) MenhirState119
            | NULL_QUESTION ->
                _menhir_run44 _menhir_env (Obj.magic _menhir_stack) MenhirState119
            | PAIR ->
                _menhir_run42 _menhir_env (Obj.magic _menhir_stack) MenhirState119
            | PROC ->
                _menhir_run16 _menhir_env (Obj.magic _menhir_stack) MenhirState119
            | SET ->
                _menhir_run13 _menhir_env (Obj.magic _menhir_stack) MenhirState119
            | SETREF ->
                _menhir_run11 _menhir_env (Obj.magic _menhir_stack) MenhirState119
            | TL ->
                _menhir_run9 _menhir_env (Obj.magic _menhir_stack) MenhirState119
            | UNIT ->
                _menhir_run8 _menhir_env (Obj.magic _menhir_stack) MenhirState119
            | UNPAIR ->
                _menhir_run1 _menhir_env (Obj.magic _menhir_stack) MenhirState119
            | _ ->
                assert (not _menhir_env._menhir_error);
                _menhir_env._menhir_error <- true;
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState119)
        | TIMES ->
            _menhir_run94 _menhir_env (Obj.magic _menhir_stack) MenhirState118
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState118)
    | MenhirState119 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | DIVIDED ->
            _menhir_run100 _menhir_env (Obj.magic _menhir_stack) MenhirState120
        | ELSE ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _menhir_s = MenhirState120 in
            let _menhir_stack = (_menhir_stack, _menhir_s) in
            let _menhir_env = _menhir_discard _menhir_env in
            let _tok = _menhir_env._menhir_token in
            (match _tok with
            | BEGIN ->
                _menhir_run88 _menhir_env (Obj.magic _menhir_stack) MenhirState121
            | CONS ->
                _menhir_run86 _menhir_env (Obj.magic _menhir_stack) MenhirState121
            | DEBUG ->
                _menhir_run85 _menhir_env (Obj.magic _menhir_stack) MenhirState121
            | DEREF ->
                _menhir_run83 _menhir_env (Obj.magic _menhir_stack) MenhirState121
            | EMPTYLIST ->
                _menhir_run81 _menhir_env (Obj.magic _menhir_stack) MenhirState121
            | EMPTYTREE ->
                _menhir_run79 _menhir_env (Obj.magic _menhir_stack) MenhirState121
            | GETDATA ->
                _menhir_run77 _menhir_env (Obj.magic _menhir_stack) MenhirState121
            | GETLST ->
                _menhir_run75 _menhir_env (Obj.magic _menhir_stack) MenhirState121
            | GETRST ->
                _menhir_run73 _menhir_env (Obj.magic _menhir_stack) MenhirState121
            | HD ->
                _menhir_run71 _menhir_env (Obj.magic _menhir_stack) MenhirState121
            | ID _v ->
                _menhir_run70 _menhir_env (Obj.magic _menhir_stack) MenhirState121 _v
            | IF ->
                _menhir_run69 _menhir_env (Obj.magic _menhir_stack) MenhirState121
            | INT _v ->
                _menhir_run68 _menhir_env (Obj.magic _menhir_stack) MenhirState121 _v
            | ISZERO ->
                _menhir_run66 _menhir_env (Obj.magic _menhir_stack) MenhirState121
            | LET ->
                _menhir_run63 _menhir_env (Obj.magic _menhir_stack) MenhirState121
            | LETREC ->
                _menhir_run54 _menhir_env (Obj.magic _menhir_stack) MenhirState121
            | LPAREN ->
                _menhir_run52 _menhir_env (Obj.magic _menhir_stack) MenhirState121
            | NEWREF ->
                _menhir_run50 _menhir_env (Obj.magic _menhir_stack) MenhirState121
            | NODE ->
                _menhir_run48 _menhir_env (Obj.magic _menhir_stack) MenhirState121
            | NULLT_QUESTION ->
                _menhir_run46 _menhir_env (Obj.magic _menhir_stack) MenhirState121
            | NULL_QUESTION ->
                _menhir_run44 _menhir_env (Obj.magic _menhir_stack) MenhirState121
            | PAIR ->
                _menhir_run42 _menhir_env (Obj.magic _menhir_stack) MenhirState121
            | PROC ->
                _menhir_run16 _menhir_env (Obj.magic _menhir_stack) MenhirState121
            | SET ->
                _menhir_run13 _menhir_env (Obj.magic _menhir_stack) MenhirState121
            | SETREF ->
                _menhir_run11 _menhir_env (Obj.magic _menhir_stack) MenhirState121
            | TL ->
                _menhir_run9 _menhir_env (Obj.magic _menhir_stack) MenhirState121
            | UNIT ->
                _menhir_run8 _menhir_env (Obj.magic _menhir_stack) MenhirState121
            | UNPAIR ->
                _menhir_run1 _menhir_env (Obj.magic _menhir_stack) MenhirState121
            | _ ->
                assert (not _menhir_env._menhir_error);
                _menhir_env._menhir_error <- true;
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState121)
        | MINUS ->
            _menhir_run102 _menhir_env (Obj.magic _menhir_stack) MenhirState120
        | PLUS ->
            _menhir_run98 _menhir_env (Obj.magic _menhir_stack) MenhirState120
        | TIMES ->
            _menhir_run94 _menhir_env (Obj.magic _menhir_stack) MenhirState120
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState120)
    | MenhirState121 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | DIVIDED ->
            _menhir_run100 _menhir_env (Obj.magic _menhir_stack) MenhirState122
        | MINUS ->
            _menhir_run102 _menhir_env (Obj.magic _menhir_stack) MenhirState122
        | PLUS ->
            _menhir_run98 _menhir_env (Obj.magic _menhir_stack) MenhirState122
        | TIMES ->
            _menhir_run94 _menhir_env (Obj.magic _menhir_stack) MenhirState122
        | BEGIN | COMMA | CONS | DEBUG | DEREF | ELSE | EMPTYLIST | EMPTYTREE | END | EOF | GETDATA | GETLST | GETRST | HD | ID _ | IF | IN | INT _ | ISZERO | LET | LETREC | LPAREN | NEWREF | NODE | NULLT_QUESTION | NULL_QUESTION | PAIR | PROC | RBRACE | RPAREN | SEMICOLON | SET | SETREF | THEN | TL | UNIT | UNPAIR ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let ((((((_menhir_stack, _menhir_s), _, (e1 : (Ast.expr))), _), _, (e2 : (Ast.expr))), _), _, (e3 : (Ast.expr))) = _menhir_stack in
            let _5 = () in
            let _3 = () in
            let _1 = () in
            let _v : (Ast.expr) = 
# 180 "parser.mly"
                                                      ( ITE(e1,e2,e3) )
# 1797 "parser.ml"
             in
            _menhir_goto_expr _menhir_env _menhir_stack _menhir_s _v
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState122)
    | MenhirState67 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | DIVIDED ->
            _menhir_run100 _menhir_env (Obj.magic _menhir_stack) MenhirState123
        | MINUS ->
            _menhir_run102 _menhir_env (Obj.magic _menhir_stack) MenhirState123
        | PLUS ->
            _menhir_run98 _menhir_env (Obj.magic _menhir_stack) MenhirState123
        | RPAREN ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _menhir_s = MenhirState123 in
            let _menhir_env = _menhir_discard _menhir_env in
            let _menhir_stack = Obj.magic _menhir_stack in
            let ((_menhir_stack, _menhir_s), _, (e : (Ast.expr))) = _menhir_stack in
            let _4 = () in
            let _2 = () in
            let _1 = () in
            let _v : (Ast.expr) = 
# 176 "parser.mly"
                                       ( IsZero(e) )
# 1827 "parser.ml"
             in
            _menhir_goto_expr _menhir_env _menhir_stack _menhir_s _v
        | TIMES ->
            _menhir_run94 _menhir_env (Obj.magic _menhir_stack) MenhirState123
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState123)
    | MenhirState65 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | DIVIDED ->
            _menhir_run100 _menhir_env (Obj.magic _menhir_stack) MenhirState125
        | IN ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _menhir_s = MenhirState125 in
            let _menhir_stack = (_menhir_stack, _menhir_s) in
            let _menhir_env = _menhir_discard _menhir_env in
            let _tok = _menhir_env._menhir_token in
            (match _tok with
            | BEGIN ->
                _menhir_run88 _menhir_env (Obj.magic _menhir_stack) MenhirState126
            | CONS ->
                _menhir_run86 _menhir_env (Obj.magic _menhir_stack) MenhirState126
            | DEBUG ->
                _menhir_run85 _menhir_env (Obj.magic _menhir_stack) MenhirState126
            | DEREF ->
                _menhir_run83 _menhir_env (Obj.magic _menhir_stack) MenhirState126
            | EMPTYLIST ->
                _menhir_run81 _menhir_env (Obj.magic _menhir_stack) MenhirState126
            | EMPTYTREE ->
                _menhir_run79 _menhir_env (Obj.magic _menhir_stack) MenhirState126
            | GETDATA ->
                _menhir_run77 _menhir_env (Obj.magic _menhir_stack) MenhirState126
            | GETLST ->
                _menhir_run75 _menhir_env (Obj.magic _menhir_stack) MenhirState126
            | GETRST ->
                _menhir_run73 _menhir_env (Obj.magic _menhir_stack) MenhirState126
            | HD ->
                _menhir_run71 _menhir_env (Obj.magic _menhir_stack) MenhirState126
            | ID _v ->
                _menhir_run70 _menhir_env (Obj.magic _menhir_stack) MenhirState126 _v
            | IF ->
                _menhir_run69 _menhir_env (Obj.magic _menhir_stack) MenhirState126
            | INT _v ->
                _menhir_run68 _menhir_env (Obj.magic _menhir_stack) MenhirState126 _v
            | ISZERO ->
                _menhir_run66 _menhir_env (Obj.magic _menhir_stack) MenhirState126
            | LET ->
                _menhir_run63 _menhir_env (Obj.magic _menhir_stack) MenhirState126
            | LETREC ->
                _menhir_run54 _menhir_env (Obj.magic _menhir_stack) MenhirState126
            | LPAREN ->
                _menhir_run52 _menhir_env (Obj.magic _menhir_stack) MenhirState126
            | NEWREF ->
                _menhir_run50 _menhir_env (Obj.magic _menhir_stack) MenhirState126
            | NODE ->
                _menhir_run48 _menhir_env (Obj.magic _menhir_stack) MenhirState126
            | NULLT_QUESTION ->
                _menhir_run46 _menhir_env (Obj.magic _menhir_stack) MenhirState126
            | NULL_QUESTION ->
                _menhir_run44 _menhir_env (Obj.magic _menhir_stack) MenhirState126
            | PAIR ->
                _menhir_run42 _menhir_env (Obj.magic _menhir_stack) MenhirState126
            | PROC ->
                _menhir_run16 _menhir_env (Obj.magic _menhir_stack) MenhirState126
            | SET ->
                _menhir_run13 _menhir_env (Obj.magic _menhir_stack) MenhirState126
            | SETREF ->
                _menhir_run11 _menhir_env (Obj.magic _menhir_stack) MenhirState126
            | TL ->
                _menhir_run9 _menhir_env (Obj.magic _menhir_stack) MenhirState126
            | UNIT ->
                _menhir_run8 _menhir_env (Obj.magic _menhir_stack) MenhirState126
            | UNPAIR ->
                _menhir_run1 _menhir_env (Obj.magic _menhir_stack) MenhirState126
            | _ ->
                assert (not _menhir_env._menhir_error);
                _menhir_env._menhir_error <- true;
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState126)
        | MINUS ->
            _menhir_run102 _menhir_env (Obj.magic _menhir_stack) MenhirState125
        | PLUS ->
            _menhir_run98 _menhir_env (Obj.magic _menhir_stack) MenhirState125
        | TIMES ->
            _menhir_run94 _menhir_env (Obj.magic _menhir_stack) MenhirState125
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState125)
    | MenhirState126 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | DIVIDED ->
            _menhir_run100 _menhir_env (Obj.magic _menhir_stack) MenhirState127
        | MINUS ->
            _menhir_run102 _menhir_env (Obj.magic _menhir_stack) MenhirState127
        | PLUS ->
            _menhir_run98 _menhir_env (Obj.magic _menhir_stack) MenhirState127
        | TIMES ->
            _menhir_run94 _menhir_env (Obj.magic _menhir_stack) MenhirState127
        | BEGIN | COMMA | CONS | DEBUG | DEREF | ELSE | EMPTYLIST | EMPTYTREE | END | EOF | GETDATA | GETLST | GETRST | HD | ID _ | IF | IN | INT _ | ISZERO | LET | LETREC | LPAREN | NEWREF | NODE | NULLT_QUESTION | NULL_QUESTION | PAIR | PROC | RBRACE | RPAREN | SEMICOLON | SET | SETREF | THEN | TL | UNIT | UNPAIR ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let (((((_menhir_stack, _menhir_s), (x : (
# 23 "parser.mly"
       (string)
# 1938 "parser.ml"
            ))), _, (e1 : (Ast.expr))), _), _, (e2 : (Ast.expr))) = _menhir_stack in
            let _5 = () in
            let _3 = () in
            let _1 = () in
            let _v : (Ast.expr) = 
# 172 "parser.mly"
                                                    ( Let(x,e1,e2) )
# 1946 "parser.ml"
             in
            _menhir_goto_expr _menhir_env _menhir_stack _menhir_s _v
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState127)
    | MenhirState62 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | DIVIDED ->
            _menhir_run100 _menhir_env (Obj.magic _menhir_stack) MenhirState128
        | IN ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _menhir_s = MenhirState128 in
            let _menhir_stack = (_menhir_stack, _menhir_s) in
            let _menhir_env = _menhir_discard _menhir_env in
            let _tok = _menhir_env._menhir_token in
            (match _tok with
            | BEGIN ->
                _menhir_run88 _menhir_env (Obj.magic _menhir_stack) MenhirState129
            | CONS ->
                _menhir_run86 _menhir_env (Obj.magic _menhir_stack) MenhirState129
            | DEBUG ->
                _menhir_run85 _menhir_env (Obj.magic _menhir_stack) MenhirState129
            | DEREF ->
                _menhir_run83 _menhir_env (Obj.magic _menhir_stack) MenhirState129
            | EMPTYLIST ->
                _menhir_run81 _menhir_env (Obj.magic _menhir_stack) MenhirState129
            | EMPTYTREE ->
                _menhir_run79 _menhir_env (Obj.magic _menhir_stack) MenhirState129
            | GETDATA ->
                _menhir_run77 _menhir_env (Obj.magic _menhir_stack) MenhirState129
            | GETLST ->
                _menhir_run75 _menhir_env (Obj.magic _menhir_stack) MenhirState129
            | GETRST ->
                _menhir_run73 _menhir_env (Obj.magic _menhir_stack) MenhirState129
            | HD ->
                _menhir_run71 _menhir_env (Obj.magic _menhir_stack) MenhirState129
            | ID _v ->
                _menhir_run70 _menhir_env (Obj.magic _menhir_stack) MenhirState129 _v
            | IF ->
                _menhir_run69 _menhir_env (Obj.magic _menhir_stack) MenhirState129
            | INT _v ->
                _menhir_run68 _menhir_env (Obj.magic _menhir_stack) MenhirState129 _v
            | ISZERO ->
                _menhir_run66 _menhir_env (Obj.magic _menhir_stack) MenhirState129
            | LET ->
                _menhir_run63 _menhir_env (Obj.magic _menhir_stack) MenhirState129
            | LETREC ->
                _menhir_run54 _menhir_env (Obj.magic _menhir_stack) MenhirState129
            | LPAREN ->
                _menhir_run52 _menhir_env (Obj.magic _menhir_stack) MenhirState129
            | NEWREF ->
                _menhir_run50 _menhir_env (Obj.magic _menhir_stack) MenhirState129
            | NODE ->
                _menhir_run48 _menhir_env (Obj.magic _menhir_stack) MenhirState129
            | NULLT_QUESTION ->
                _menhir_run46 _menhir_env (Obj.magic _menhir_stack) MenhirState129
            | NULL_QUESTION ->
                _menhir_run44 _menhir_env (Obj.magic _menhir_stack) MenhirState129
            | PAIR ->
                _menhir_run42 _menhir_env (Obj.magic _menhir_stack) MenhirState129
            | PROC ->
                _menhir_run16 _menhir_env (Obj.magic _menhir_stack) MenhirState129
            | SET ->
                _menhir_run13 _menhir_env (Obj.magic _menhir_stack) MenhirState129
            | SETREF ->
                _menhir_run11 _menhir_env (Obj.magic _menhir_stack) MenhirState129
            | TL ->
                _menhir_run9 _menhir_env (Obj.magic _menhir_stack) MenhirState129
            | UNIT ->
                _menhir_run8 _menhir_env (Obj.magic _menhir_stack) MenhirState129
            | UNPAIR ->
                _menhir_run1 _menhir_env (Obj.magic _menhir_stack) MenhirState129
            | _ ->
                assert (not _menhir_env._menhir_error);
                _menhir_env._menhir_error <- true;
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState129)
        | MINUS ->
            _menhir_run102 _menhir_env (Obj.magic _menhir_stack) MenhirState128
        | PLUS ->
            _menhir_run98 _menhir_env (Obj.magic _menhir_stack) MenhirState128
        | TIMES ->
            _menhir_run94 _menhir_env (Obj.magic _menhir_stack) MenhirState128
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState128)
    | MenhirState129 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | DIVIDED ->
            _menhir_run100 _menhir_env (Obj.magic _menhir_stack) MenhirState130
        | MINUS ->
            _menhir_run102 _menhir_env (Obj.magic _menhir_stack) MenhirState130
        | PLUS ->
            _menhir_run98 _menhir_env (Obj.magic _menhir_stack) MenhirState130
        | TIMES ->
            _menhir_run94 _menhir_env (Obj.magic _menhir_stack) MenhirState130
        | BEGIN | COMMA | CONS | DEBUG | DEREF | ELSE | EMPTYLIST | EMPTYTREE | END | EOF | GETDATA | GETLST | GETRST | HD | ID _ | IF | IN | INT _ | ISZERO | LET | LETREC | LPAREN | NEWREF | NODE | NULLT_QUESTION | NULL_QUESTION | PAIR | PROC | RBRACE | RPAREN | SEMICOLON | SET | SETREF | THEN | TL | UNIT | UNPAIR ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let ((((((((_menhir_stack, _menhir_s), _, (tr : (Ast.texpr))), (x : (
# 23 "parser.mly"
       (string)
# 2055 "parser.ml"
            ))), (y : (
# 23 "parser.mly"
       (string)
# 2059 "parser.ml"
            ))), _, (targ : (Ast.texpr))), _, (e1 : (Ast.expr))), _), _, (e2 : (Ast.expr))) = _menhir_stack in
            let _11 = () in
            let _9 = () in
            let _8 = () in
            let _6 = () in
            let _4 = () in
            let _1 = () in
            let _v : (Ast.expr) = 
# 173 "parser.mly"
                                                                                                            ( Letrec(tr,x,y,targ,e1,e2) )
# 2070 "parser.ml"
             in
            _menhir_goto_expr _menhir_env _menhir_stack _menhir_s _v
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState130)
    | MenhirState53 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | DIVIDED ->
            _menhir_run100 _menhir_env (Obj.magic _menhir_stack) MenhirState131
        | MINUS ->
            _menhir_run102 _menhir_env (Obj.magic _menhir_stack) MenhirState131
        | PLUS ->
            _menhir_run98 _menhir_env (Obj.magic _menhir_stack) MenhirState131
        | RPAREN ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _menhir_s = MenhirState131 in
            let _menhir_env = _menhir_discard _menhir_env in
            let _menhir_stack = Obj.magic _menhir_stack in
            let (((_menhir_stack, _menhir_s), _), _, (e : (Ast.expr))) = _menhir_stack in
            let _4 = () in
            let _2 = () in
            let _1 = () in
            let _v : (Ast.expr) = 
# 184 "parser.mly"
                                      ( Sub(Int 0, e) )
# 2100 "parser.ml"
             in
            _menhir_goto_expr _menhir_env _menhir_stack _menhir_s _v
        | TIMES ->
            _menhir_run94 _menhir_env (Obj.magic _menhir_stack) MenhirState131
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState131)
    | MenhirState52 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | BEGIN ->
            _menhir_run88 _menhir_env (Obj.magic _menhir_stack) MenhirState133
        | CONS ->
            _menhir_run86 _menhir_env (Obj.magic _menhir_stack) MenhirState133
        | DEBUG ->
            _menhir_run85 _menhir_env (Obj.magic _menhir_stack) MenhirState133
        | DEREF ->
            _menhir_run83 _menhir_env (Obj.magic _menhir_stack) MenhirState133
        | DIVIDED ->
            _menhir_run100 _menhir_env (Obj.magic _menhir_stack) MenhirState133
        | EMPTYLIST ->
            _menhir_run81 _menhir_env (Obj.magic _menhir_stack) MenhirState133
        | EMPTYTREE ->
            _menhir_run79 _menhir_env (Obj.magic _menhir_stack) MenhirState133
        | GETDATA ->
            _menhir_run77 _menhir_env (Obj.magic _menhir_stack) MenhirState133
        | GETLST ->
            _menhir_run75 _menhir_env (Obj.magic _menhir_stack) MenhirState133
        | GETRST ->
            _menhir_run73 _menhir_env (Obj.magic _menhir_stack) MenhirState133
        | HD ->
            _menhir_run71 _menhir_env (Obj.magic _menhir_stack) MenhirState133
        | ID _v ->
            _menhir_run70 _menhir_env (Obj.magic _menhir_stack) MenhirState133 _v
        | IF ->
            _menhir_run69 _menhir_env (Obj.magic _menhir_stack) MenhirState133
        | INT _v ->
            _menhir_run68 _menhir_env (Obj.magic _menhir_stack) MenhirState133 _v
        | ISZERO ->
            _menhir_run66 _menhir_env (Obj.magic _menhir_stack) MenhirState133
        | LET ->
            _menhir_run63 _menhir_env (Obj.magic _menhir_stack) MenhirState133
        | LETREC ->
            _menhir_run54 _menhir_env (Obj.magic _menhir_stack) MenhirState133
        | LPAREN ->
            _menhir_run52 _menhir_env (Obj.magic _menhir_stack) MenhirState133
        | MINUS ->
            _menhir_run102 _menhir_env (Obj.magic _menhir_stack) MenhirState133
        | NEWREF ->
            _menhir_run50 _menhir_env (Obj.magic _menhir_stack) MenhirState133
        | NODE ->
            _menhir_run48 _menhir_env (Obj.magic _menhir_stack) MenhirState133
        | NULLT_QUESTION ->
            _menhir_run46 _menhir_env (Obj.magic _menhir_stack) MenhirState133
        | NULL_QUESTION ->
            _menhir_run44 _menhir_env (Obj.magic _menhir_stack) MenhirState133
        | PAIR ->
            _menhir_run42 _menhir_env (Obj.magic _menhir_stack) MenhirState133
        | PLUS ->
            _menhir_run98 _menhir_env (Obj.magic _menhir_stack) MenhirState133
        | PROC ->
            _menhir_run16 _menhir_env (Obj.magic _menhir_stack) MenhirState133
        | RPAREN ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _menhir_s = MenhirState133 in
            let _menhir_env = _menhir_discard _menhir_env in
            let _menhir_stack = Obj.magic _menhir_stack in
            let ((_menhir_stack, _menhir_s), _, (e : (Ast.expr))) = _menhir_stack in
            let _3 = () in
            let _1 = () in
            let _v : (Ast.expr) = 
# 183 "parser.mly"
                               (e)
# 2177 "parser.ml"
             in
            _menhir_goto_expr _menhir_env _menhir_stack _menhir_s _v
        | SET ->
            _menhir_run13 _menhir_env (Obj.magic _menhir_stack) MenhirState133
        | SETREF ->
            _menhir_run11 _menhir_env (Obj.magic _menhir_stack) MenhirState133
        | TIMES ->
            _menhir_run94 _menhir_env (Obj.magic _menhir_stack) MenhirState133
        | TL ->
            _menhir_run9 _menhir_env (Obj.magic _menhir_stack) MenhirState133
        | UNIT ->
            _menhir_run8 _menhir_env (Obj.magic _menhir_stack) MenhirState133
        | UNPAIR ->
            _menhir_run1 _menhir_env (Obj.magic _menhir_stack) MenhirState133
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState133)
    | MenhirState133 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | DIVIDED ->
            _menhir_run100 _menhir_env (Obj.magic _menhir_stack) MenhirState135
        | MINUS ->
            _menhir_run102 _menhir_env (Obj.magic _menhir_stack) MenhirState135
        | PLUS ->
            _menhir_run98 _menhir_env (Obj.magic _menhir_stack) MenhirState135
        | RPAREN ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _menhir_s = MenhirState135 in
            let _menhir_env = _menhir_discard _menhir_env in
            let _menhir_stack = Obj.magic _menhir_stack in
            let (((_menhir_stack, _menhir_s), _, (e1 : (Ast.expr))), _, (e2 : (Ast.expr))) = _menhir_stack in
            let _4 = () in
            let _1 = () in
            let _v : (Ast.expr) = 
# 175 "parser.mly"
                                           ( App(e1,e2) )
# 2218 "parser.ml"
             in
            _menhir_goto_expr _menhir_env _menhir_stack _menhir_s _v
        | TIMES ->
            _menhir_run94 _menhir_env (Obj.magic _menhir_stack) MenhirState135
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState135)
    | MenhirState51 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | DIVIDED ->
            _menhir_run100 _menhir_env (Obj.magic _menhir_stack) MenhirState137
        | MINUS ->
            _menhir_run102 _menhir_env (Obj.magic _menhir_stack) MenhirState137
        | PLUS ->
            _menhir_run98 _menhir_env (Obj.magic _menhir_stack) MenhirState137
        | RPAREN ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _menhir_s = MenhirState137 in
            let _menhir_env = _menhir_discard _menhir_env in
            let _menhir_stack = Obj.magic _menhir_stack in
            let ((_menhir_stack, _menhir_s), _, (e : (Ast.expr))) = _menhir_stack in
            let _4 = () in
            let _2 = () in
            let _1 = () in
            let _v : (Ast.expr) = 
# 177 "parser.mly"
                                       ( NewRef(e) )
# 2250 "parser.ml"
             in
            _menhir_goto_expr _menhir_env _menhir_stack _menhir_s _v
        | TIMES ->
            _menhir_run94 _menhir_env (Obj.magic _menhir_stack) MenhirState137
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState137)
    | MenhirState49 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | COMMA ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _menhir_s = MenhirState139 in
            let _menhir_stack = (_menhir_stack, _menhir_s) in
            let _menhir_env = _menhir_discard _menhir_env in
            let _tok = _menhir_env._menhir_token in
            (match _tok with
            | BEGIN ->
                _menhir_run88 _menhir_env (Obj.magic _menhir_stack) MenhirState140
            | CONS ->
                _menhir_run86 _menhir_env (Obj.magic _menhir_stack) MenhirState140
            | DEBUG ->
                _menhir_run85 _menhir_env (Obj.magic _menhir_stack) MenhirState140
            | DEREF ->
                _menhir_run83 _menhir_env (Obj.magic _menhir_stack) MenhirState140
            | EMPTYLIST ->
                _menhir_run81 _menhir_env (Obj.magic _menhir_stack) MenhirState140
            | EMPTYTREE ->
                _menhir_run79 _menhir_env (Obj.magic _menhir_stack) MenhirState140
            | GETDATA ->
                _menhir_run77 _menhir_env (Obj.magic _menhir_stack) MenhirState140
            | GETLST ->
                _menhir_run75 _menhir_env (Obj.magic _menhir_stack) MenhirState140
            | GETRST ->
                _menhir_run73 _menhir_env (Obj.magic _menhir_stack) MenhirState140
            | HD ->
                _menhir_run71 _menhir_env (Obj.magic _menhir_stack) MenhirState140
            | ID _v ->
                _menhir_run70 _menhir_env (Obj.magic _menhir_stack) MenhirState140 _v
            | IF ->
                _menhir_run69 _menhir_env (Obj.magic _menhir_stack) MenhirState140
            | INT _v ->
                _menhir_run68 _menhir_env (Obj.magic _menhir_stack) MenhirState140 _v
            | ISZERO ->
                _menhir_run66 _menhir_env (Obj.magic _menhir_stack) MenhirState140
            | LET ->
                _menhir_run63 _menhir_env (Obj.magic _menhir_stack) MenhirState140
            | LETREC ->
                _menhir_run54 _menhir_env (Obj.magic _menhir_stack) MenhirState140
            | LPAREN ->
                _menhir_run52 _menhir_env (Obj.magic _menhir_stack) MenhirState140
            | NEWREF ->
                _menhir_run50 _menhir_env (Obj.magic _menhir_stack) MenhirState140
            | NODE ->
                _menhir_run48 _menhir_env (Obj.magic _menhir_stack) MenhirState140
            | NULLT_QUESTION ->
                _menhir_run46 _menhir_env (Obj.magic _menhir_stack) MenhirState140
            | NULL_QUESTION ->
                _menhir_run44 _menhir_env (Obj.magic _menhir_stack) MenhirState140
            | PAIR ->
                _menhir_run42 _menhir_env (Obj.magic _menhir_stack) MenhirState140
            | PROC ->
                _menhir_run16 _menhir_env (Obj.magic _menhir_stack) MenhirState140
            | SET ->
                _menhir_run13 _menhir_env (Obj.magic _menhir_stack) MenhirState140
            | SETREF ->
                _menhir_run11 _menhir_env (Obj.magic _menhir_stack) MenhirState140
            | TL ->
                _menhir_run9 _menhir_env (Obj.magic _menhir_stack) MenhirState140
            | UNIT ->
                _menhir_run8 _menhir_env (Obj.magic _menhir_stack) MenhirState140
            | UNPAIR ->
                _menhir_run1 _menhir_env (Obj.magic _menhir_stack) MenhirState140
            | _ ->
                assert (not _menhir_env._menhir_error);
                _menhir_env._menhir_error <- true;
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState140)
        | DIVIDED ->
            _menhir_run100 _menhir_env (Obj.magic _menhir_stack) MenhirState139
        | MINUS ->
            _menhir_run102 _menhir_env (Obj.magic _menhir_stack) MenhirState139
        | PLUS ->
            _menhir_run98 _menhir_env (Obj.magic _menhir_stack) MenhirState139
        | TIMES ->
            _menhir_run94 _menhir_env (Obj.magic _menhir_stack) MenhirState139
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState139)
    | MenhirState140 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | COMMA ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _menhir_s = MenhirState141 in
            let _menhir_stack = (_menhir_stack, _menhir_s) in
            let _menhir_env = _menhir_discard _menhir_env in
            let _tok = _menhir_env._menhir_token in
            (match _tok with
            | BEGIN ->
                _menhir_run88 _menhir_env (Obj.magic _menhir_stack) MenhirState142
            | CONS ->
                _menhir_run86 _menhir_env (Obj.magic _menhir_stack) MenhirState142
            | DEBUG ->
                _menhir_run85 _menhir_env (Obj.magic _menhir_stack) MenhirState142
            | DEREF ->
                _menhir_run83 _menhir_env (Obj.magic _menhir_stack) MenhirState142
            | EMPTYLIST ->
                _menhir_run81 _menhir_env (Obj.magic _menhir_stack) MenhirState142
            | EMPTYTREE ->
                _menhir_run79 _menhir_env (Obj.magic _menhir_stack) MenhirState142
            | GETDATA ->
                _menhir_run77 _menhir_env (Obj.magic _menhir_stack) MenhirState142
            | GETLST ->
                _menhir_run75 _menhir_env (Obj.magic _menhir_stack) MenhirState142
            | GETRST ->
                _menhir_run73 _menhir_env (Obj.magic _menhir_stack) MenhirState142
            | HD ->
                _menhir_run71 _menhir_env (Obj.magic _menhir_stack) MenhirState142
            | ID _v ->
                _menhir_run70 _menhir_env (Obj.magic _menhir_stack) MenhirState142 _v
            | IF ->
                _menhir_run69 _menhir_env (Obj.magic _menhir_stack) MenhirState142
            | INT _v ->
                _menhir_run68 _menhir_env (Obj.magic _menhir_stack) MenhirState142 _v
            | ISZERO ->
                _menhir_run66 _menhir_env (Obj.magic _menhir_stack) MenhirState142
            | LET ->
                _menhir_run63 _menhir_env (Obj.magic _menhir_stack) MenhirState142
            | LETREC ->
                _menhir_run54 _menhir_env (Obj.magic _menhir_stack) MenhirState142
            | LPAREN ->
                _menhir_run52 _menhir_env (Obj.magic _menhir_stack) MenhirState142
            | NEWREF ->
                _menhir_run50 _menhir_env (Obj.magic _menhir_stack) MenhirState142
            | NODE ->
                _menhir_run48 _menhir_env (Obj.magic _menhir_stack) MenhirState142
            | NULLT_QUESTION ->
                _menhir_run46 _menhir_env (Obj.magic _menhir_stack) MenhirState142
            | NULL_QUESTION ->
                _menhir_run44 _menhir_env (Obj.magic _menhir_stack) MenhirState142
            | PAIR ->
                _menhir_run42 _menhir_env (Obj.magic _menhir_stack) MenhirState142
            | PROC ->
                _menhir_run16 _menhir_env (Obj.magic _menhir_stack) MenhirState142
            | SET ->
                _menhir_run13 _menhir_env (Obj.magic _menhir_stack) MenhirState142
            | SETREF ->
                _menhir_run11 _menhir_env (Obj.magic _menhir_stack) MenhirState142
            | TL ->
                _menhir_run9 _menhir_env (Obj.magic _menhir_stack) MenhirState142
            | UNIT ->
                _menhir_run8 _menhir_env (Obj.magic _menhir_stack) MenhirState142
            | UNPAIR ->
                _menhir_run1 _menhir_env (Obj.magic _menhir_stack) MenhirState142
            | _ ->
                assert (not _menhir_env._menhir_error);
                _menhir_env._menhir_error <- true;
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState142)
        | DIVIDED ->
            _menhir_run100 _menhir_env (Obj.magic _menhir_stack) MenhirState141
        | MINUS ->
            _menhir_run102 _menhir_env (Obj.magic _menhir_stack) MenhirState141
        | PLUS ->
            _menhir_run98 _menhir_env (Obj.magic _menhir_stack) MenhirState141
        | TIMES ->
            _menhir_run94 _menhir_env (Obj.magic _menhir_stack) MenhirState141
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState141)
    | MenhirState142 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | DIVIDED ->
            _menhir_run100 _menhir_env (Obj.magic _menhir_stack) MenhirState143
        | MINUS ->
            _menhir_run102 _menhir_env (Obj.magic _menhir_stack) MenhirState143
        | PLUS ->
            _menhir_run98 _menhir_env (Obj.magic _menhir_stack) MenhirState143
        | RPAREN ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _menhir_s = MenhirState143 in
            let _menhir_env = _menhir_discard _menhir_env in
            let _menhir_stack = Obj.magic _menhir_stack in
            let ((((((_menhir_stack, _menhir_s), _, (e1 : (Ast.expr))), _), _, (e2 : (Ast.expr))), _), _, (e3 : (Ast.expr))) = _menhir_stack in
            let _8 = () in
            let _6 = () in
            let _4 = () in
            let _2 = () in
            let _1 = () in
            let _v : (Ast.expr) = 
# 202 "parser.mly"
                                                             (Node(e1,e2,e3))
# 2452 "parser.ml"
             in
            _menhir_goto_expr _menhir_env _menhir_stack _menhir_s _v
        | TIMES ->
            _menhir_run94 _menhir_env (Obj.magic _menhir_stack) MenhirState143
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState143)
    | MenhirState47 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | DIVIDED ->
            _menhir_run100 _menhir_env (Obj.magic _menhir_stack) MenhirState145
        | MINUS ->
            _menhir_run102 _menhir_env (Obj.magic _menhir_stack) MenhirState145
        | PLUS ->
            _menhir_run98 _menhir_env (Obj.magic _menhir_stack) MenhirState145
        | RPAREN ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _menhir_s = MenhirState145 in
            let _menhir_env = _menhir_discard _menhir_env in
            let _menhir_stack = Obj.magic _menhir_stack in
            let ((_menhir_stack, _menhir_s), _, (e : (Ast.expr))) = _menhir_stack in
            let _4 = () in
            let _2 = () in
            let _1 = () in
            let _v : (Ast.expr) = 
# 203 "parser.mly"
                                             (NullT(e))
# 2484 "parser.ml"
             in
            _menhir_goto_expr _menhir_env _menhir_stack _menhir_s _v
        | TIMES ->
            _menhir_run94 _menhir_env (Obj.magic _menhir_stack) MenhirState145
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState145)
    | MenhirState45 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | DIVIDED ->
            _menhir_run100 _menhir_env (Obj.magic _menhir_stack) MenhirState147
        | MINUS ->
            _menhir_run102 _menhir_env (Obj.magic _menhir_stack) MenhirState147
        | PLUS ->
            _menhir_run98 _menhir_env (Obj.magic _menhir_stack) MenhirState147
        | RPAREN ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _menhir_s = MenhirState147 in
            let _menhir_env = _menhir_discard _menhir_env in
            let _menhir_stack = Obj.magic _menhir_stack in
            let ((_menhir_stack, _menhir_s), _, (e : (Ast.expr))) = _menhir_stack in
            let _4 = () in
            let _2 = () in
            let _1 = () in
            let _v : (Ast.expr) = 
# 196 "parser.mly"
                                              ( Null(e) )
# 2516 "parser.ml"
             in
            _menhir_goto_expr _menhir_env _menhir_stack _menhir_s _v
        | TIMES ->
            _menhir_run94 _menhir_env (Obj.magic _menhir_stack) MenhirState147
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState147)
    | MenhirState43 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | COMMA ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _menhir_s = MenhirState149 in
            let _menhir_stack = (_menhir_stack, _menhir_s) in
            let _menhir_env = _menhir_discard _menhir_env in
            let _tok = _menhir_env._menhir_token in
            (match _tok with
            | BEGIN ->
                _menhir_run88 _menhir_env (Obj.magic _menhir_stack) MenhirState150
            | CONS ->
                _menhir_run86 _menhir_env (Obj.magic _menhir_stack) MenhirState150
            | DEBUG ->
                _menhir_run85 _menhir_env (Obj.magic _menhir_stack) MenhirState150
            | DEREF ->
                _menhir_run83 _menhir_env (Obj.magic _menhir_stack) MenhirState150
            | EMPTYLIST ->
                _menhir_run81 _menhir_env (Obj.magic _menhir_stack) MenhirState150
            | EMPTYTREE ->
                _menhir_run79 _menhir_env (Obj.magic _menhir_stack) MenhirState150
            | GETDATA ->
                _menhir_run77 _menhir_env (Obj.magic _menhir_stack) MenhirState150
            | GETLST ->
                _menhir_run75 _menhir_env (Obj.magic _menhir_stack) MenhirState150
            | GETRST ->
                _menhir_run73 _menhir_env (Obj.magic _menhir_stack) MenhirState150
            | HD ->
                _menhir_run71 _menhir_env (Obj.magic _menhir_stack) MenhirState150
            | ID _v ->
                _menhir_run70 _menhir_env (Obj.magic _menhir_stack) MenhirState150 _v
            | IF ->
                _menhir_run69 _menhir_env (Obj.magic _menhir_stack) MenhirState150
            | INT _v ->
                _menhir_run68 _menhir_env (Obj.magic _menhir_stack) MenhirState150 _v
            | ISZERO ->
                _menhir_run66 _menhir_env (Obj.magic _menhir_stack) MenhirState150
            | LET ->
                _menhir_run63 _menhir_env (Obj.magic _menhir_stack) MenhirState150
            | LETREC ->
                _menhir_run54 _menhir_env (Obj.magic _menhir_stack) MenhirState150
            | LPAREN ->
                _menhir_run52 _menhir_env (Obj.magic _menhir_stack) MenhirState150
            | NEWREF ->
                _menhir_run50 _menhir_env (Obj.magic _menhir_stack) MenhirState150
            | NODE ->
                _menhir_run48 _menhir_env (Obj.magic _menhir_stack) MenhirState150
            | NULLT_QUESTION ->
                _menhir_run46 _menhir_env (Obj.magic _menhir_stack) MenhirState150
            | NULL_QUESTION ->
                _menhir_run44 _menhir_env (Obj.magic _menhir_stack) MenhirState150
            | PAIR ->
                _menhir_run42 _menhir_env (Obj.magic _menhir_stack) MenhirState150
            | PROC ->
                _menhir_run16 _menhir_env (Obj.magic _menhir_stack) MenhirState150
            | SET ->
                _menhir_run13 _menhir_env (Obj.magic _menhir_stack) MenhirState150
            | SETREF ->
                _menhir_run11 _menhir_env (Obj.magic _menhir_stack) MenhirState150
            | TL ->
                _menhir_run9 _menhir_env (Obj.magic _menhir_stack) MenhirState150
            | UNIT ->
                _menhir_run8 _menhir_env (Obj.magic _menhir_stack) MenhirState150
            | UNPAIR ->
                _menhir_run1 _menhir_env (Obj.magic _menhir_stack) MenhirState150
            | _ ->
                assert (not _menhir_env._menhir_error);
                _menhir_env._menhir_error <- true;
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState150)
        | DIVIDED ->
            _menhir_run100 _menhir_env (Obj.magic _menhir_stack) MenhirState149
        | MINUS ->
            _menhir_run102 _menhir_env (Obj.magic _menhir_stack) MenhirState149
        | PLUS ->
            _menhir_run98 _menhir_env (Obj.magic _menhir_stack) MenhirState149
        | TIMES ->
            _menhir_run94 _menhir_env (Obj.magic _menhir_stack) MenhirState149
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState149)
    | MenhirState150 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | DIVIDED ->
            _menhir_run100 _menhir_env (Obj.magic _menhir_stack) MenhirState151
        | MINUS ->
            _menhir_run102 _menhir_env (Obj.magic _menhir_stack) MenhirState151
        | PLUS ->
            _menhir_run98 _menhir_env (Obj.magic _menhir_stack) MenhirState151
        | RPAREN ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _menhir_s = MenhirState151 in
            let _menhir_env = _menhir_discard _menhir_env in
            let _menhir_stack = Obj.magic _menhir_stack in
            let ((((_menhir_stack, _menhir_s), _, (e1 : (Ast.expr))), _), _, (e2 : (Ast.expr))) = _menhir_stack in
            let _6 = () in
            let _4 = () in
            let _2 = () in
            let _1 = () in
            let _v : (Ast.expr) = 
# 188 "parser.mly"
                                                             ( Pair ( e1 , e2 ) )
# 2633 "parser.ml"
             in
            _menhir_goto_expr _menhir_env _menhir_stack _menhir_s _v
        | TIMES ->
            _menhir_run94 _menhir_env (Obj.magic _menhir_stack) MenhirState151
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState151)
    | MenhirState41 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | DIVIDED ->
            _menhir_run100 _menhir_env (Obj.magic _menhir_stack) MenhirState153
        | MINUS ->
            _menhir_run102 _menhir_env (Obj.magic _menhir_stack) MenhirState153
        | PLUS ->
            _menhir_run98 _menhir_env (Obj.magic _menhir_stack) MenhirState153
        | RBRACE ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _menhir_s = MenhirState153 in
            let _menhir_env = _menhir_discard _menhir_env in
            let _menhir_stack = Obj.magic _menhir_stack in
            let ((((_menhir_stack, _menhir_s), (x : (
# 23 "parser.mly"
       (string)
# 2661 "parser.ml"
            ))), _, (t : (Ast.texpr))), _, (e : (Ast.expr))) = _menhir_stack in
            let _9 = () in
            let _7 = () in
            let _6 = () in
            let _4 = () in
            let _2 = () in
            let _1 = () in
            let _v : (Ast.expr) = 
# 174 "parser.mly"
                                                                             ( Proc(x,t,e) )
# 2672 "parser.ml"
             in
            _menhir_goto_expr _menhir_env _menhir_stack _menhir_s _v
        | TIMES ->
            _menhir_run94 _menhir_env (Obj.magic _menhir_stack) MenhirState153
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState153)
    | MenhirState15 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | DIVIDED ->
            _menhir_run100 _menhir_env (Obj.magic _menhir_stack) MenhirState155
        | MINUS ->
            _menhir_run102 _menhir_env (Obj.magic _menhir_stack) MenhirState155
        | PLUS ->
            _menhir_run98 _menhir_env (Obj.magic _menhir_stack) MenhirState155
        | TIMES ->
            _menhir_run94 _menhir_env (Obj.magic _menhir_stack) MenhirState155
        | BEGIN | COMMA | CONS | DEBUG | DEREF | ELSE | EMPTYLIST | EMPTYTREE | END | EOF | GETDATA | GETLST | GETRST | HD | ID _ | IF | IN | INT _ | ISZERO | LET | LETREC | LPAREN | NEWREF | NODE | NULLT_QUESTION | NULL_QUESTION | PAIR | PROC | RBRACE | RPAREN | SEMICOLON | SET | SETREF | THEN | TL | UNIT | UNPAIR ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let (((_menhir_stack, _menhir_s), (x : (
# 23 "parser.mly"
       (string)
# 2699 "parser.ml"
            ))), _, (e : (Ast.expr))) = _menhir_stack in
            let _3 = () in
            let _1 = () in
            let _v : (Ast.expr) = 
# 181 "parser.mly"
                                    ( Set(x,e) )
# 2706 "parser.ml"
             in
            _menhir_goto_expr _menhir_env _menhir_stack _menhir_s _v
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState155)
    | MenhirState12 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | COMMA ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _menhir_s = MenhirState156 in
            let _menhir_stack = (_menhir_stack, _menhir_s) in
            let _menhir_env = _menhir_discard _menhir_env in
            let _tok = _menhir_env._menhir_token in
            (match _tok with
            | BEGIN ->
                _menhir_run88 _menhir_env (Obj.magic _menhir_stack) MenhirState157
            | CONS ->
                _menhir_run86 _menhir_env (Obj.magic _menhir_stack) MenhirState157
            | DEBUG ->
                _menhir_run85 _menhir_env (Obj.magic _menhir_stack) MenhirState157
            | DEREF ->
                _menhir_run83 _menhir_env (Obj.magic _menhir_stack) MenhirState157
            | EMPTYLIST ->
                _menhir_run81 _menhir_env (Obj.magic _menhir_stack) MenhirState157
            | EMPTYTREE ->
                _menhir_run79 _menhir_env (Obj.magic _menhir_stack) MenhirState157
            | GETDATA ->
                _menhir_run77 _menhir_env (Obj.magic _menhir_stack) MenhirState157
            | GETLST ->
                _menhir_run75 _menhir_env (Obj.magic _menhir_stack) MenhirState157
            | GETRST ->
                _menhir_run73 _menhir_env (Obj.magic _menhir_stack) MenhirState157
            | HD ->
                _menhir_run71 _menhir_env (Obj.magic _menhir_stack) MenhirState157
            | ID _v ->
                _menhir_run70 _menhir_env (Obj.magic _menhir_stack) MenhirState157 _v
            | IF ->
                _menhir_run69 _menhir_env (Obj.magic _menhir_stack) MenhirState157
            | INT _v ->
                _menhir_run68 _menhir_env (Obj.magic _menhir_stack) MenhirState157 _v
            | ISZERO ->
                _menhir_run66 _menhir_env (Obj.magic _menhir_stack) MenhirState157
            | LET ->
                _menhir_run63 _menhir_env (Obj.magic _menhir_stack) MenhirState157
            | LETREC ->
                _menhir_run54 _menhir_env (Obj.magic _menhir_stack) MenhirState157
            | LPAREN ->
                _menhir_run52 _menhir_env (Obj.magic _menhir_stack) MenhirState157
            | NEWREF ->
                _menhir_run50 _menhir_env (Obj.magic _menhir_stack) MenhirState157
            | NODE ->
                _menhir_run48 _menhir_env (Obj.magic _menhir_stack) MenhirState157
            | NULLT_QUESTION ->
                _menhir_run46 _menhir_env (Obj.magic _menhir_stack) MenhirState157
            | NULL_QUESTION ->
                _menhir_run44 _menhir_env (Obj.magic _menhir_stack) MenhirState157
            | PAIR ->
                _menhir_run42 _menhir_env (Obj.magic _menhir_stack) MenhirState157
            | PROC ->
                _menhir_run16 _menhir_env (Obj.magic _menhir_stack) MenhirState157
            | SET ->
                _menhir_run13 _menhir_env (Obj.magic _menhir_stack) MenhirState157
            | SETREF ->
                _menhir_run11 _menhir_env (Obj.magic _menhir_stack) MenhirState157
            | TL ->
                _menhir_run9 _menhir_env (Obj.magic _menhir_stack) MenhirState157
            | UNIT ->
                _menhir_run8 _menhir_env (Obj.magic _menhir_stack) MenhirState157
            | UNPAIR ->
                _menhir_run1 _menhir_env (Obj.magic _menhir_stack) MenhirState157
            | _ ->
                assert (not _menhir_env._menhir_error);
                _menhir_env._menhir_error <- true;
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState157)
        | DIVIDED ->
            _menhir_run100 _menhir_env (Obj.magic _menhir_stack) MenhirState156
        | MINUS ->
            _menhir_run102 _menhir_env (Obj.magic _menhir_stack) MenhirState156
        | PLUS ->
            _menhir_run98 _menhir_env (Obj.magic _menhir_stack) MenhirState156
        | TIMES ->
            _menhir_run94 _menhir_env (Obj.magic _menhir_stack) MenhirState156
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState156)
    | MenhirState157 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | DIVIDED ->
            _menhir_run100 _menhir_env (Obj.magic _menhir_stack) MenhirState158
        | MINUS ->
            _menhir_run102 _menhir_env (Obj.magic _menhir_stack) MenhirState158
        | PLUS ->
            _menhir_run98 _menhir_env (Obj.magic _menhir_stack) MenhirState158
        | RPAREN ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _menhir_s = MenhirState158 in
            let _menhir_env = _menhir_discard _menhir_env in
            let _menhir_stack = Obj.magic _menhir_stack in
            let ((((_menhir_stack, _menhir_s), _, (e1 : (Ast.expr))), _), _, (e2 : (Ast.expr))) = _menhir_stack in
            let _6 = () in
            let _4 = () in
            let _2 = () in
            let _1 = () in
            let _v : (Ast.expr) = 
# 179 "parser.mly"
                                                          ( SetRef(e1,e2) )
# 2821 "parser.ml"
             in
            _menhir_goto_expr _menhir_env _menhir_stack _menhir_s _v
        | TIMES ->
            _menhir_run94 _menhir_env (Obj.magic _menhir_stack) MenhirState158
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState158)
    | MenhirState10 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | DIVIDED ->
            _menhir_run100 _menhir_env (Obj.magic _menhir_stack) MenhirState160
        | MINUS ->
            _menhir_run102 _menhir_env (Obj.magic _menhir_stack) MenhirState160
        | PLUS ->
            _menhir_run98 _menhir_env (Obj.magic _menhir_stack) MenhirState160
        | RPAREN ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _menhir_s = MenhirState160 in
            let _menhir_env = _menhir_discard _menhir_env in
            let _menhir_stack = Obj.magic _menhir_stack in
            let ((_menhir_stack, _menhir_s), _, (e : (Ast.expr))) = _menhir_stack in
            let _4 = () in
            let _2 = () in
            let _1 = () in
            let _v : (Ast.expr) = 
# 195 "parser.mly"
                                   ( Tl(e) )
# 2853 "parser.ml"
             in
            _menhir_goto_expr _menhir_env _menhir_stack _menhir_s _v
        | TIMES ->
            _menhir_run94 _menhir_env (Obj.magic _menhir_stack) MenhirState160
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState160)
    | MenhirState7 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | DIVIDED ->
            _menhir_run100 _menhir_env (Obj.magic _menhir_stack) MenhirState162
        | IN ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _menhir_s = MenhirState162 in
            let _menhir_stack = (_menhir_stack, _menhir_s) in
            let _menhir_env = _menhir_discard _menhir_env in
            let _tok = _menhir_env._menhir_token in
            (match _tok with
            | BEGIN ->
                _menhir_run88 _menhir_env (Obj.magic _menhir_stack) MenhirState163
            | CONS ->
                _menhir_run86 _menhir_env (Obj.magic _menhir_stack) MenhirState163
            | DEBUG ->
                _menhir_run85 _menhir_env (Obj.magic _menhir_stack) MenhirState163
            | DEREF ->
                _menhir_run83 _menhir_env (Obj.magic _menhir_stack) MenhirState163
            | EMPTYLIST ->
                _menhir_run81 _menhir_env (Obj.magic _menhir_stack) MenhirState163
            | EMPTYTREE ->
                _menhir_run79 _menhir_env (Obj.magic _menhir_stack) MenhirState163
            | GETDATA ->
                _menhir_run77 _menhir_env (Obj.magic _menhir_stack) MenhirState163
            | GETLST ->
                _menhir_run75 _menhir_env (Obj.magic _menhir_stack) MenhirState163
            | GETRST ->
                _menhir_run73 _menhir_env (Obj.magic _menhir_stack) MenhirState163
            | HD ->
                _menhir_run71 _menhir_env (Obj.magic _menhir_stack) MenhirState163
            | ID _v ->
                _menhir_run70 _menhir_env (Obj.magic _menhir_stack) MenhirState163 _v
            | IF ->
                _menhir_run69 _menhir_env (Obj.magic _menhir_stack) MenhirState163
            | INT _v ->
                _menhir_run68 _menhir_env (Obj.magic _menhir_stack) MenhirState163 _v
            | ISZERO ->
                _menhir_run66 _menhir_env (Obj.magic _menhir_stack) MenhirState163
            | LET ->
                _menhir_run63 _menhir_env (Obj.magic _menhir_stack) MenhirState163
            | LETREC ->
                _menhir_run54 _menhir_env (Obj.magic _menhir_stack) MenhirState163
            | LPAREN ->
                _menhir_run52 _menhir_env (Obj.magic _menhir_stack) MenhirState163
            | NEWREF ->
                _menhir_run50 _menhir_env (Obj.magic _menhir_stack) MenhirState163
            | NODE ->
                _menhir_run48 _menhir_env (Obj.magic _menhir_stack) MenhirState163
            | NULLT_QUESTION ->
                _menhir_run46 _menhir_env (Obj.magic _menhir_stack) MenhirState163
            | NULL_QUESTION ->
                _menhir_run44 _menhir_env (Obj.magic _menhir_stack) MenhirState163
            | PAIR ->
                _menhir_run42 _menhir_env (Obj.magic _menhir_stack) MenhirState163
            | PROC ->
                _menhir_run16 _menhir_env (Obj.magic _menhir_stack) MenhirState163
            | SET ->
                _menhir_run13 _menhir_env (Obj.magic _menhir_stack) MenhirState163
            | SETREF ->
                _menhir_run11 _menhir_env (Obj.magic _menhir_stack) MenhirState163
            | TL ->
                _menhir_run9 _menhir_env (Obj.magic _menhir_stack) MenhirState163
            | UNIT ->
                _menhir_run8 _menhir_env (Obj.magic _menhir_stack) MenhirState163
            | UNPAIR ->
                _menhir_run1 _menhir_env (Obj.magic _menhir_stack) MenhirState163
            | _ ->
                assert (not _menhir_env._menhir_error);
                _menhir_env._menhir_error <- true;
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState163)
        | MINUS ->
            _menhir_run102 _menhir_env (Obj.magic _menhir_stack) MenhirState162
        | PLUS ->
            _menhir_run98 _menhir_env (Obj.magic _menhir_stack) MenhirState162
        | TIMES ->
            _menhir_run94 _menhir_env (Obj.magic _menhir_stack) MenhirState162
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState162)
    | MenhirState163 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | DIVIDED ->
            _menhir_run100 _menhir_env (Obj.magic _menhir_stack) MenhirState164
        | MINUS ->
            _menhir_run102 _menhir_env (Obj.magic _menhir_stack) MenhirState164
        | PLUS ->
            _menhir_run98 _menhir_env (Obj.magic _menhir_stack) MenhirState164
        | TIMES ->
            _menhir_run94 _menhir_env (Obj.magic _menhir_stack) MenhirState164
        | BEGIN | COMMA | CONS | DEBUG | DEREF | ELSE | EMPTYLIST | EMPTYTREE | END | EOF | GETDATA | GETLST | GETRST | HD | ID _ | IF | IN | INT _ | ISZERO | LET | LETREC | LPAREN | NEWREF | NODE | NULLT_QUESTION | NULL_QUESTION | PAIR | PROC | RBRACE | RPAREN | SEMICOLON | SET | SETREF | THEN | TL | UNIT | UNPAIR ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let ((((((_menhir_stack, _menhir_s), (id1 : (
# 23 "parser.mly"
       (string)
# 2964 "parser.ml"
            ))), (id2 : (
# 23 "parser.mly"
       (string)
# 2968 "parser.ml"
            ))), _, (e_pair : (Ast.expr))), _), _, (e_body : (Ast.expr))) = _menhir_stack in
            let _9 = () in
            let _7 = () in
            let _6 = () in
            let _4 = () in
            let _2 = () in
            let _1 = () in
            let _v : (Ast.expr) = 
# 189 "parser.mly"
                                                                                                          ( Unpair ( id1 , id2 , e_pair , e_body ) )
# 2979 "parser.ml"
             in
            _menhir_goto_expr _menhir_env _menhir_stack _menhir_s _v
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState164)
    | MenhirState0 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | DIVIDED ->
            _menhir_run100 _menhir_env (Obj.magic _menhir_stack) MenhirState166
        | EOF ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _menhir_s = MenhirState166 in
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, (e : (Ast.expr))) = _menhir_stack in
            let _2 = () in
            let _v : (
# 104 "parser.mly"
       (Ast.prog)
# 3002 "parser.ml"
            ) = 
# 136 "parser.mly"
                 ( AProg e )
# 3006 "parser.ml"
             in
            let _menhir_stack = Obj.magic _menhir_stack in
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_1 : (
# 104 "parser.mly"
       (Ast.prog)
# 3013 "parser.ml"
            )) = _v in
            Obj.magic _1
        | MINUS ->
            _menhir_run102 _menhir_env (Obj.magic _menhir_stack) MenhirState166
        | PLUS ->
            _menhir_run98 _menhir_env (Obj.magic _menhir_stack) MenhirState166
        | TIMES ->
            _menhir_run94 _menhir_env (Obj.magic _menhir_stack) MenhirState166
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState166)
    | _ ->
        _menhir_fail ()

and _menhir_goto_loption_separated_nonempty_list_SEMICOLON_expr__ : _menhir_env -> 'ttv_tail -> _menhir_state -> (Ast.expr list) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_stack = Obj.magic _menhir_stack in
    let _menhir_stack = Obj.magic _menhir_stack in
    let (xs0 : (Ast.expr list)) = _v in
    let _v : (Ast.expr list) = let es =
      let xs = xs0 in
      
# 220 "/home/nikhil/.opam/system/lib/menhir/standard.mly"
    ( xs )
# 3039 "parser.ml"
      
    in
    
# 212 "parser.mly"
                                            ( es )
# 3045 "parser.ml"
     in
    let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
    let _menhir_stack = Obj.magic _menhir_stack in
    assert (not _menhir_env._menhir_error);
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | END ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_env = _menhir_discard _menhir_env in
        let _menhir_stack = Obj.magic _menhir_stack in
        let ((_menhir_stack, _menhir_s), _, (es : (Ast.expr list))) = _menhir_stack in
        let _3 = () in
        let _1 = () in
        let _v : (Ast.expr) = 
# 182 "parser.mly"
                             ( BeginEnd(es) )
# 3062 "parser.ml"
         in
        _menhir_goto_expr _menhir_env _menhir_stack _menhir_s _v
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s

and _menhir_errorcase : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    match _menhir_s with
    | MenhirState166 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState164 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState163 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState162 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState160 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState158 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState157 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState156 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState155 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState153 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState151 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState150 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState149 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState147 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState145 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState143 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState142 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState141 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState140 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState139 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState137 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState135 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState133 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState131 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState130 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState129 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState128 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState127 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState126 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState125 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState123 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState122 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState121 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState120 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState119 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState118 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState116 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState114 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState112 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState110 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState108 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState106 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState105 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState104 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState103 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState102 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState100 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState99 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState98 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState96 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState94 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState93 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState88 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState87 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState84 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState81 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState79 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState78 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState76 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState74 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState72 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState69 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState67 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState65 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let ((_menhir_stack, _menhir_s), _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState62 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState59 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (((_menhir_stack, _menhir_s, _), _), _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState54 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState53 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState52 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState51 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState49 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState47 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState45 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState43 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState41 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState32 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState29 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState25 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState24 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState23 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState22 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState21 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState19 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let ((_menhir_stack, _menhir_s), _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState15 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let ((_menhir_stack, _menhir_s), _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState12 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState10 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState7 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (((_menhir_stack, _menhir_s), _), _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState0 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        raise _eRR

and _menhir_run1 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_stack = (_menhir_stack, _menhir_s) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | LPAREN ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_env = _menhir_discard _menhir_env in
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | ID _v ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _menhir_stack = (_menhir_stack, _v) in
            let _menhir_env = _menhir_discard _menhir_env in
            let _tok = _menhir_env._menhir_token in
            (match _tok with
            | COMMA ->
                let _menhir_stack = Obj.magic _menhir_stack in
                let _menhir_env = _menhir_discard _menhir_env in
                let _tok = _menhir_env._menhir_token in
                (match _tok with
                | ID _v ->
                    let _menhir_stack = Obj.magic _menhir_stack in
                    let _menhir_stack = (_menhir_stack, _v) in
                    let _menhir_env = _menhir_discard _menhir_env in
                    let _tok = _menhir_env._menhir_token in
                    (match _tok with
                    | RPAREN ->
                        let _menhir_stack = Obj.magic _menhir_stack in
                        let _menhir_env = _menhir_discard _menhir_env in
                        let _tok = _menhir_env._menhir_token in
                        (match _tok with
                        | EQUALS ->
                            let _menhir_stack = Obj.magic _menhir_stack in
                            let _menhir_env = _menhir_discard _menhir_env in
                            let _tok = _menhir_env._menhir_token in
                            (match _tok with
                            | BEGIN ->
                                _menhir_run88 _menhir_env (Obj.magic _menhir_stack) MenhirState7
                            | CONS ->
                                _menhir_run86 _menhir_env (Obj.magic _menhir_stack) MenhirState7
                            | DEBUG ->
                                _menhir_run85 _menhir_env (Obj.magic _menhir_stack) MenhirState7
                            | DEREF ->
                                _menhir_run83 _menhir_env (Obj.magic _menhir_stack) MenhirState7
                            | EMPTYLIST ->
                                _menhir_run81 _menhir_env (Obj.magic _menhir_stack) MenhirState7
                            | EMPTYTREE ->
                                _menhir_run79 _menhir_env (Obj.magic _menhir_stack) MenhirState7
                            | GETDATA ->
                                _menhir_run77 _menhir_env (Obj.magic _menhir_stack) MenhirState7
                            | GETLST ->
                                _menhir_run75 _menhir_env (Obj.magic _menhir_stack) MenhirState7
                            | GETRST ->
                                _menhir_run73 _menhir_env (Obj.magic _menhir_stack) MenhirState7
                            | HD ->
                                _menhir_run71 _menhir_env (Obj.magic _menhir_stack) MenhirState7
                            | ID _v ->
                                _menhir_run70 _menhir_env (Obj.magic _menhir_stack) MenhirState7 _v
                            | IF ->
                                _menhir_run69 _menhir_env (Obj.magic _menhir_stack) MenhirState7
                            | INT _v ->
                                _menhir_run68 _menhir_env (Obj.magic _menhir_stack) MenhirState7 _v
                            | ISZERO ->
                                _menhir_run66 _menhir_env (Obj.magic _menhir_stack) MenhirState7
                            | LET ->
                                _menhir_run63 _menhir_env (Obj.magic _menhir_stack) MenhirState7
                            | LETREC ->
                                _menhir_run54 _menhir_env (Obj.magic _menhir_stack) MenhirState7
                            | LPAREN ->
                                _menhir_run52 _menhir_env (Obj.magic _menhir_stack) MenhirState7
                            | NEWREF ->
                                _menhir_run50 _menhir_env (Obj.magic _menhir_stack) MenhirState7
                            | NODE ->
                                _menhir_run48 _menhir_env (Obj.magic _menhir_stack) MenhirState7
                            | NULLT_QUESTION ->
                                _menhir_run46 _menhir_env (Obj.magic _menhir_stack) MenhirState7
                            | NULL_QUESTION ->
                                _menhir_run44 _menhir_env (Obj.magic _menhir_stack) MenhirState7
                            | PAIR ->
                                _menhir_run42 _menhir_env (Obj.magic _menhir_stack) MenhirState7
                            | PROC ->
                                _menhir_run16 _menhir_env (Obj.magic _menhir_stack) MenhirState7
                            | SET ->
                                _menhir_run13 _menhir_env (Obj.magic _menhir_stack) MenhirState7
                            | SETREF ->
                                _menhir_run11 _menhir_env (Obj.magic _menhir_stack) MenhirState7
                            | TL ->
                                _menhir_run9 _menhir_env (Obj.magic _menhir_stack) MenhirState7
                            | UNIT ->
                                _menhir_run8 _menhir_env (Obj.magic _menhir_stack) MenhirState7
                            | UNPAIR ->
                                _menhir_run1 _menhir_env (Obj.magic _menhir_stack) MenhirState7
                            | _ ->
                                assert (not _menhir_env._menhir_error);
                                _menhir_env._menhir_error <- true;
                                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState7)
                        | _ ->
                            assert (not _menhir_env._menhir_error);
                            _menhir_env._menhir_error <- true;
                            let _menhir_stack = Obj.magic _menhir_stack in
                            let (((_menhir_stack, _menhir_s), _), _) = _menhir_stack in
                            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
                    | _ ->
                        assert (not _menhir_env._menhir_error);
                        _menhir_env._menhir_error <- true;
                        let _menhir_stack = Obj.magic _menhir_stack in
                        let (((_menhir_stack, _menhir_s), _), _) = _menhir_stack in
                        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
                | _ ->
                    assert (not _menhir_env._menhir_error);
                    _menhir_env._menhir_error <- true;
                    let _menhir_stack = Obj.magic _menhir_stack in
                    let ((_menhir_stack, _menhir_s), _) = _menhir_stack in
                    _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
            | _ ->
                assert (not _menhir_env._menhir_error);
                _menhir_env._menhir_error <- true;
                let _menhir_stack = Obj.magic _menhir_stack in
                let ((_menhir_stack, _menhir_s), _) = _menhir_stack in
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s

and _menhir_run8 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_env = _menhir_discard _menhir_env in
    let _menhir_stack = Obj.magic _menhir_stack in
    let _1 = () in
    let _v : (Ast.expr) = 
# 185 "parser.mly"
           (Unit)
# 3570 "parser.ml"
     in
    _menhir_goto_expr _menhir_env _menhir_stack _menhir_s _v

and _menhir_run9 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_stack = (_menhir_stack, _menhir_s) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | LPAREN ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_env = _menhir_discard _menhir_env in
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | BEGIN ->
            _menhir_run88 _menhir_env (Obj.magic _menhir_stack) MenhirState10
        | CONS ->
            _menhir_run86 _menhir_env (Obj.magic _menhir_stack) MenhirState10
        | DEBUG ->
            _menhir_run85 _menhir_env (Obj.magic _menhir_stack) MenhirState10
        | DEREF ->
            _menhir_run83 _menhir_env (Obj.magic _menhir_stack) MenhirState10
        | EMPTYLIST ->
            _menhir_run81 _menhir_env (Obj.magic _menhir_stack) MenhirState10
        | EMPTYTREE ->
            _menhir_run79 _menhir_env (Obj.magic _menhir_stack) MenhirState10
        | GETDATA ->
            _menhir_run77 _menhir_env (Obj.magic _menhir_stack) MenhirState10
        | GETLST ->
            _menhir_run75 _menhir_env (Obj.magic _menhir_stack) MenhirState10
        | GETRST ->
            _menhir_run73 _menhir_env (Obj.magic _menhir_stack) MenhirState10
        | HD ->
            _menhir_run71 _menhir_env (Obj.magic _menhir_stack) MenhirState10
        | ID _v ->
            _menhir_run70 _menhir_env (Obj.magic _menhir_stack) MenhirState10 _v
        | IF ->
            _menhir_run69 _menhir_env (Obj.magic _menhir_stack) MenhirState10
        | INT _v ->
            _menhir_run68 _menhir_env (Obj.magic _menhir_stack) MenhirState10 _v
        | ISZERO ->
            _menhir_run66 _menhir_env (Obj.magic _menhir_stack) MenhirState10
        | LET ->
            _menhir_run63 _menhir_env (Obj.magic _menhir_stack) MenhirState10
        | LETREC ->
            _menhir_run54 _menhir_env (Obj.magic _menhir_stack) MenhirState10
        | LPAREN ->
            _menhir_run52 _menhir_env (Obj.magic _menhir_stack) MenhirState10
        | NEWREF ->
            _menhir_run50 _menhir_env (Obj.magic _menhir_stack) MenhirState10
        | NODE ->
            _menhir_run48 _menhir_env (Obj.magic _menhir_stack) MenhirState10
        | NULLT_QUESTION ->
            _menhir_run46 _menhir_env (Obj.magic _menhir_stack) MenhirState10
        | NULL_QUESTION ->
            _menhir_run44 _menhir_env (Obj.magic _menhir_stack) MenhirState10
        | PAIR ->
            _menhir_run42 _menhir_env (Obj.magic _menhir_stack) MenhirState10
        | PROC ->
            _menhir_run16 _menhir_env (Obj.magic _menhir_stack) MenhirState10
        | SET ->
            _menhir_run13 _menhir_env (Obj.magic _menhir_stack) MenhirState10
        | SETREF ->
            _menhir_run11 _menhir_env (Obj.magic _menhir_stack) MenhirState10
        | TL ->
            _menhir_run9 _menhir_env (Obj.magic _menhir_stack) MenhirState10
        | UNIT ->
            _menhir_run8 _menhir_env (Obj.magic _menhir_stack) MenhirState10
        | UNPAIR ->
            _menhir_run1 _menhir_env (Obj.magic _menhir_stack) MenhirState10
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState10)
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s

and _menhir_run11 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_stack = (_menhir_stack, _menhir_s) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | LPAREN ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_env = _menhir_discard _menhir_env in
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | BEGIN ->
            _menhir_run88 _menhir_env (Obj.magic _menhir_stack) MenhirState12
        | CONS ->
            _menhir_run86 _menhir_env (Obj.magic _menhir_stack) MenhirState12
        | DEBUG ->
            _menhir_run85 _menhir_env (Obj.magic _menhir_stack) MenhirState12
        | DEREF ->
            _menhir_run83 _menhir_env (Obj.magic _menhir_stack) MenhirState12
        | EMPTYLIST ->
            _menhir_run81 _menhir_env (Obj.magic _menhir_stack) MenhirState12
        | EMPTYTREE ->
            _menhir_run79 _menhir_env (Obj.magic _menhir_stack) MenhirState12
        | GETDATA ->
            _menhir_run77 _menhir_env (Obj.magic _menhir_stack) MenhirState12
        | GETLST ->
            _menhir_run75 _menhir_env (Obj.magic _menhir_stack) MenhirState12
        | GETRST ->
            _menhir_run73 _menhir_env (Obj.magic _menhir_stack) MenhirState12
        | HD ->
            _menhir_run71 _menhir_env (Obj.magic _menhir_stack) MenhirState12
        | ID _v ->
            _menhir_run70 _menhir_env (Obj.magic _menhir_stack) MenhirState12 _v
        | IF ->
            _menhir_run69 _menhir_env (Obj.magic _menhir_stack) MenhirState12
        | INT _v ->
            _menhir_run68 _menhir_env (Obj.magic _menhir_stack) MenhirState12 _v
        | ISZERO ->
            _menhir_run66 _menhir_env (Obj.magic _menhir_stack) MenhirState12
        | LET ->
            _menhir_run63 _menhir_env (Obj.magic _menhir_stack) MenhirState12
        | LETREC ->
            _menhir_run54 _menhir_env (Obj.magic _menhir_stack) MenhirState12
        | LPAREN ->
            _menhir_run52 _menhir_env (Obj.magic _menhir_stack) MenhirState12
        | NEWREF ->
            _menhir_run50 _menhir_env (Obj.magic _menhir_stack) MenhirState12
        | NODE ->
            _menhir_run48 _menhir_env (Obj.magic _menhir_stack) MenhirState12
        | NULLT_QUESTION ->
            _menhir_run46 _menhir_env (Obj.magic _menhir_stack) MenhirState12
        | NULL_QUESTION ->
            _menhir_run44 _menhir_env (Obj.magic _menhir_stack) MenhirState12
        | PAIR ->
            _menhir_run42 _menhir_env (Obj.magic _menhir_stack) MenhirState12
        | PROC ->
            _menhir_run16 _menhir_env (Obj.magic _menhir_stack) MenhirState12
        | SET ->
            _menhir_run13 _menhir_env (Obj.magic _menhir_stack) MenhirState12
        | SETREF ->
            _menhir_run11 _menhir_env (Obj.magic _menhir_stack) MenhirState12
        | TL ->
            _menhir_run9 _menhir_env (Obj.magic _menhir_stack) MenhirState12
        | UNIT ->
            _menhir_run8 _menhir_env (Obj.magic _menhir_stack) MenhirState12
        | UNPAIR ->
            _menhir_run1 _menhir_env (Obj.magic _menhir_stack) MenhirState12
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState12)
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s

and _menhir_run13 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_stack = (_menhir_stack, _menhir_s) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | ID _v ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_stack = (_menhir_stack, _v) in
        let _menhir_env = _menhir_discard _menhir_env in
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | EQUALS ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _menhir_env = _menhir_discard _menhir_env in
            let _tok = _menhir_env._menhir_token in
            (match _tok with
            | BEGIN ->
                _menhir_run88 _menhir_env (Obj.magic _menhir_stack) MenhirState15
            | CONS ->
                _menhir_run86 _menhir_env (Obj.magic _menhir_stack) MenhirState15
            | DEBUG ->
                _menhir_run85 _menhir_env (Obj.magic _menhir_stack) MenhirState15
            | DEREF ->
                _menhir_run83 _menhir_env (Obj.magic _menhir_stack) MenhirState15
            | EMPTYLIST ->
                _menhir_run81 _menhir_env (Obj.magic _menhir_stack) MenhirState15
            | EMPTYTREE ->
                _menhir_run79 _menhir_env (Obj.magic _menhir_stack) MenhirState15
            | GETDATA ->
                _menhir_run77 _menhir_env (Obj.magic _menhir_stack) MenhirState15
            | GETLST ->
                _menhir_run75 _menhir_env (Obj.magic _menhir_stack) MenhirState15
            | GETRST ->
                _menhir_run73 _menhir_env (Obj.magic _menhir_stack) MenhirState15
            | HD ->
                _menhir_run71 _menhir_env (Obj.magic _menhir_stack) MenhirState15
            | ID _v ->
                _menhir_run70 _menhir_env (Obj.magic _menhir_stack) MenhirState15 _v
            | IF ->
                _menhir_run69 _menhir_env (Obj.magic _menhir_stack) MenhirState15
            | INT _v ->
                _menhir_run68 _menhir_env (Obj.magic _menhir_stack) MenhirState15 _v
            | ISZERO ->
                _menhir_run66 _menhir_env (Obj.magic _menhir_stack) MenhirState15
            | LET ->
                _menhir_run63 _menhir_env (Obj.magic _menhir_stack) MenhirState15
            | LETREC ->
                _menhir_run54 _menhir_env (Obj.magic _menhir_stack) MenhirState15
            | LPAREN ->
                _menhir_run52 _menhir_env (Obj.magic _menhir_stack) MenhirState15
            | NEWREF ->
                _menhir_run50 _menhir_env (Obj.magic _menhir_stack) MenhirState15
            | NODE ->
                _menhir_run48 _menhir_env (Obj.magic _menhir_stack) MenhirState15
            | NULLT_QUESTION ->
                _menhir_run46 _menhir_env (Obj.magic _menhir_stack) MenhirState15
            | NULL_QUESTION ->
                _menhir_run44 _menhir_env (Obj.magic _menhir_stack) MenhirState15
            | PAIR ->
                _menhir_run42 _menhir_env (Obj.magic _menhir_stack) MenhirState15
            | PROC ->
                _menhir_run16 _menhir_env (Obj.magic _menhir_stack) MenhirState15
            | SET ->
                _menhir_run13 _menhir_env (Obj.magic _menhir_stack) MenhirState15
            | SETREF ->
                _menhir_run11 _menhir_env (Obj.magic _menhir_stack) MenhirState15
            | TL ->
                _menhir_run9 _menhir_env (Obj.magic _menhir_stack) MenhirState15
            | UNIT ->
                _menhir_run8 _menhir_env (Obj.magic _menhir_stack) MenhirState15
            | UNPAIR ->
                _menhir_run1 _menhir_env (Obj.magic _menhir_stack) MenhirState15
            | _ ->
                assert (not _menhir_env._menhir_error);
                _menhir_env._menhir_error <- true;
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState15)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let _menhir_stack = Obj.magic _menhir_stack in
            let ((_menhir_stack, _menhir_s), _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s

and _menhir_run16 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_stack = (_menhir_stack, _menhir_s) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | LPAREN ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_env = _menhir_discard _menhir_env in
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | ID _v ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _menhir_stack = (_menhir_stack, _v) in
            let _menhir_env = _menhir_discard _menhir_env in
            let _tok = _menhir_env._menhir_token in
            (match _tok with
            | COLON ->
                let _menhir_stack = Obj.magic _menhir_stack in
                let _menhir_env = _menhir_discard _menhir_env in
                let _tok = _menhir_env._menhir_token in
                (match _tok with
                | BOOLTYPE ->
                    _menhir_run27 _menhir_env (Obj.magic _menhir_stack) MenhirState19
                | INTTYPE ->
                    _menhir_run26 _menhir_env (Obj.magic _menhir_stack) MenhirState19
                | LESS_THAN ->
                    _menhir_run25 _menhir_env (Obj.magic _menhir_stack) MenhirState19
                | LISTTYPE ->
                    _menhir_run24 _menhir_env (Obj.magic _menhir_stack) MenhirState19
                | LPAREN ->
                    _menhir_run23 _menhir_env (Obj.magic _menhir_stack) MenhirState19
                | REFTYPE ->
                    _menhir_run22 _menhir_env (Obj.magic _menhir_stack) MenhirState19
                | TREETYPE ->
                    _menhir_run21 _menhir_env (Obj.magic _menhir_stack) MenhirState19
                | UNITTYPE ->
                    _menhir_run20 _menhir_env (Obj.magic _menhir_stack) MenhirState19
                | _ ->
                    assert (not _menhir_env._menhir_error);
                    _menhir_env._menhir_error <- true;
                    _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState19)
            | _ ->
                assert (not _menhir_env._menhir_error);
                _menhir_env._menhir_error <- true;
                let _menhir_stack = Obj.magic _menhir_stack in
                let ((_menhir_stack, _menhir_s), _) = _menhir_stack in
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s

and _menhir_run42 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_stack = (_menhir_stack, _menhir_s) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | LPAREN ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_env = _menhir_discard _menhir_env in
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | BEGIN ->
            _menhir_run88 _menhir_env (Obj.magic _menhir_stack) MenhirState43
        | CONS ->
            _menhir_run86 _menhir_env (Obj.magic _menhir_stack) MenhirState43
        | DEBUG ->
            _menhir_run85 _menhir_env (Obj.magic _menhir_stack) MenhirState43
        | DEREF ->
            _menhir_run83 _menhir_env (Obj.magic _menhir_stack) MenhirState43
        | EMPTYLIST ->
            _menhir_run81 _menhir_env (Obj.magic _menhir_stack) MenhirState43
        | EMPTYTREE ->
            _menhir_run79 _menhir_env (Obj.magic _menhir_stack) MenhirState43
        | GETDATA ->
            _menhir_run77 _menhir_env (Obj.magic _menhir_stack) MenhirState43
        | GETLST ->
            _menhir_run75 _menhir_env (Obj.magic _menhir_stack) MenhirState43
        | GETRST ->
            _menhir_run73 _menhir_env (Obj.magic _menhir_stack) MenhirState43
        | HD ->
            _menhir_run71 _menhir_env (Obj.magic _menhir_stack) MenhirState43
        | ID _v ->
            _menhir_run70 _menhir_env (Obj.magic _menhir_stack) MenhirState43 _v
        | IF ->
            _menhir_run69 _menhir_env (Obj.magic _menhir_stack) MenhirState43
        | INT _v ->
            _menhir_run68 _menhir_env (Obj.magic _menhir_stack) MenhirState43 _v
        | ISZERO ->
            _menhir_run66 _menhir_env (Obj.magic _menhir_stack) MenhirState43
        | LET ->
            _menhir_run63 _menhir_env (Obj.magic _menhir_stack) MenhirState43
        | LETREC ->
            _menhir_run54 _menhir_env (Obj.magic _menhir_stack) MenhirState43
        | LPAREN ->
            _menhir_run52 _menhir_env (Obj.magic _menhir_stack) MenhirState43
        | NEWREF ->
            _menhir_run50 _menhir_env (Obj.magic _menhir_stack) MenhirState43
        | NODE ->
            _menhir_run48 _menhir_env (Obj.magic _menhir_stack) MenhirState43
        | NULLT_QUESTION ->
            _menhir_run46 _menhir_env (Obj.magic _menhir_stack) MenhirState43
        | NULL_QUESTION ->
            _menhir_run44 _menhir_env (Obj.magic _menhir_stack) MenhirState43
        | PAIR ->
            _menhir_run42 _menhir_env (Obj.magic _menhir_stack) MenhirState43
        | PROC ->
            _menhir_run16 _menhir_env (Obj.magic _menhir_stack) MenhirState43
        | SET ->
            _menhir_run13 _menhir_env (Obj.magic _menhir_stack) MenhirState43
        | SETREF ->
            _menhir_run11 _menhir_env (Obj.magic _menhir_stack) MenhirState43
        | TL ->
            _menhir_run9 _menhir_env (Obj.magic _menhir_stack) MenhirState43
        | UNIT ->
            _menhir_run8 _menhir_env (Obj.magic _menhir_stack) MenhirState43
        | UNPAIR ->
            _menhir_run1 _menhir_env (Obj.magic _menhir_stack) MenhirState43
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState43)
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s

and _menhir_run44 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_stack = (_menhir_stack, _menhir_s) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | LPAREN ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_env = _menhir_discard _menhir_env in
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | BEGIN ->
            _menhir_run88 _menhir_env (Obj.magic _menhir_stack) MenhirState45
        | CONS ->
            _menhir_run86 _menhir_env (Obj.magic _menhir_stack) MenhirState45
        | DEBUG ->
            _menhir_run85 _menhir_env (Obj.magic _menhir_stack) MenhirState45
        | DEREF ->
            _menhir_run83 _menhir_env (Obj.magic _menhir_stack) MenhirState45
        | EMPTYLIST ->
            _menhir_run81 _menhir_env (Obj.magic _menhir_stack) MenhirState45
        | EMPTYTREE ->
            _menhir_run79 _menhir_env (Obj.magic _menhir_stack) MenhirState45
        | GETDATA ->
            _menhir_run77 _menhir_env (Obj.magic _menhir_stack) MenhirState45
        | GETLST ->
            _menhir_run75 _menhir_env (Obj.magic _menhir_stack) MenhirState45
        | GETRST ->
            _menhir_run73 _menhir_env (Obj.magic _menhir_stack) MenhirState45
        | HD ->
            _menhir_run71 _menhir_env (Obj.magic _menhir_stack) MenhirState45
        | ID _v ->
            _menhir_run70 _menhir_env (Obj.magic _menhir_stack) MenhirState45 _v
        | IF ->
            _menhir_run69 _menhir_env (Obj.magic _menhir_stack) MenhirState45
        | INT _v ->
            _menhir_run68 _menhir_env (Obj.magic _menhir_stack) MenhirState45 _v
        | ISZERO ->
            _menhir_run66 _menhir_env (Obj.magic _menhir_stack) MenhirState45
        | LET ->
            _menhir_run63 _menhir_env (Obj.magic _menhir_stack) MenhirState45
        | LETREC ->
            _menhir_run54 _menhir_env (Obj.magic _menhir_stack) MenhirState45
        | LPAREN ->
            _menhir_run52 _menhir_env (Obj.magic _menhir_stack) MenhirState45
        | NEWREF ->
            _menhir_run50 _menhir_env (Obj.magic _menhir_stack) MenhirState45
        | NODE ->
            _menhir_run48 _menhir_env (Obj.magic _menhir_stack) MenhirState45
        | NULLT_QUESTION ->
            _menhir_run46 _menhir_env (Obj.magic _menhir_stack) MenhirState45
        | NULL_QUESTION ->
            _menhir_run44 _menhir_env (Obj.magic _menhir_stack) MenhirState45
        | PAIR ->
            _menhir_run42 _menhir_env (Obj.magic _menhir_stack) MenhirState45
        | PROC ->
            _menhir_run16 _menhir_env (Obj.magic _menhir_stack) MenhirState45
        | SET ->
            _menhir_run13 _menhir_env (Obj.magic _menhir_stack) MenhirState45
        | SETREF ->
            _menhir_run11 _menhir_env (Obj.magic _menhir_stack) MenhirState45
        | TL ->
            _menhir_run9 _menhir_env (Obj.magic _menhir_stack) MenhirState45
        | UNIT ->
            _menhir_run8 _menhir_env (Obj.magic _menhir_stack) MenhirState45
        | UNPAIR ->
            _menhir_run1 _menhir_env (Obj.magic _menhir_stack) MenhirState45
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState45)
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s

and _menhir_run46 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_stack = (_menhir_stack, _menhir_s) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | LPAREN ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_env = _menhir_discard _menhir_env in
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | BEGIN ->
            _menhir_run88 _menhir_env (Obj.magic _menhir_stack) MenhirState47
        | CONS ->
            _menhir_run86 _menhir_env (Obj.magic _menhir_stack) MenhirState47
        | DEBUG ->
            _menhir_run85 _menhir_env (Obj.magic _menhir_stack) MenhirState47
        | DEREF ->
            _menhir_run83 _menhir_env (Obj.magic _menhir_stack) MenhirState47
        | EMPTYLIST ->
            _menhir_run81 _menhir_env (Obj.magic _menhir_stack) MenhirState47
        | EMPTYTREE ->
            _menhir_run79 _menhir_env (Obj.magic _menhir_stack) MenhirState47
        | GETDATA ->
            _menhir_run77 _menhir_env (Obj.magic _menhir_stack) MenhirState47
        | GETLST ->
            _menhir_run75 _menhir_env (Obj.magic _menhir_stack) MenhirState47
        | GETRST ->
            _menhir_run73 _menhir_env (Obj.magic _menhir_stack) MenhirState47
        | HD ->
            _menhir_run71 _menhir_env (Obj.magic _menhir_stack) MenhirState47
        | ID _v ->
            _menhir_run70 _menhir_env (Obj.magic _menhir_stack) MenhirState47 _v
        | IF ->
            _menhir_run69 _menhir_env (Obj.magic _menhir_stack) MenhirState47
        | INT _v ->
            _menhir_run68 _menhir_env (Obj.magic _menhir_stack) MenhirState47 _v
        | ISZERO ->
            _menhir_run66 _menhir_env (Obj.magic _menhir_stack) MenhirState47
        | LET ->
            _menhir_run63 _menhir_env (Obj.magic _menhir_stack) MenhirState47
        | LETREC ->
            _menhir_run54 _menhir_env (Obj.magic _menhir_stack) MenhirState47
        | LPAREN ->
            _menhir_run52 _menhir_env (Obj.magic _menhir_stack) MenhirState47
        | NEWREF ->
            _menhir_run50 _menhir_env (Obj.magic _menhir_stack) MenhirState47
        | NODE ->
            _menhir_run48 _menhir_env (Obj.magic _menhir_stack) MenhirState47
        | NULLT_QUESTION ->
            _menhir_run46 _menhir_env (Obj.magic _menhir_stack) MenhirState47
        | NULL_QUESTION ->
            _menhir_run44 _menhir_env (Obj.magic _menhir_stack) MenhirState47
        | PAIR ->
            _menhir_run42 _menhir_env (Obj.magic _menhir_stack) MenhirState47
        | PROC ->
            _menhir_run16 _menhir_env (Obj.magic _menhir_stack) MenhirState47
        | SET ->
            _menhir_run13 _menhir_env (Obj.magic _menhir_stack) MenhirState47
        | SETREF ->
            _menhir_run11 _menhir_env (Obj.magic _menhir_stack) MenhirState47
        | TL ->
            _menhir_run9 _menhir_env (Obj.magic _menhir_stack) MenhirState47
        | UNIT ->
            _menhir_run8 _menhir_env (Obj.magic _menhir_stack) MenhirState47
        | UNPAIR ->
            _menhir_run1 _menhir_env (Obj.magic _menhir_stack) MenhirState47
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState47)
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s

and _menhir_run48 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_stack = (_menhir_stack, _menhir_s) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | LPAREN ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_env = _menhir_discard _menhir_env in
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | BEGIN ->
            _menhir_run88 _menhir_env (Obj.magic _menhir_stack) MenhirState49
        | CONS ->
            _menhir_run86 _menhir_env (Obj.magic _menhir_stack) MenhirState49
        | DEBUG ->
            _menhir_run85 _menhir_env (Obj.magic _menhir_stack) MenhirState49
        | DEREF ->
            _menhir_run83 _menhir_env (Obj.magic _menhir_stack) MenhirState49
        | EMPTYLIST ->
            _menhir_run81 _menhir_env (Obj.magic _menhir_stack) MenhirState49
        | EMPTYTREE ->
            _menhir_run79 _menhir_env (Obj.magic _menhir_stack) MenhirState49
        | GETDATA ->
            _menhir_run77 _menhir_env (Obj.magic _menhir_stack) MenhirState49
        | GETLST ->
            _menhir_run75 _menhir_env (Obj.magic _menhir_stack) MenhirState49
        | GETRST ->
            _menhir_run73 _menhir_env (Obj.magic _menhir_stack) MenhirState49
        | HD ->
            _menhir_run71 _menhir_env (Obj.magic _menhir_stack) MenhirState49
        | ID _v ->
            _menhir_run70 _menhir_env (Obj.magic _menhir_stack) MenhirState49 _v
        | IF ->
            _menhir_run69 _menhir_env (Obj.magic _menhir_stack) MenhirState49
        | INT _v ->
            _menhir_run68 _menhir_env (Obj.magic _menhir_stack) MenhirState49 _v
        | ISZERO ->
            _menhir_run66 _menhir_env (Obj.magic _menhir_stack) MenhirState49
        | LET ->
            _menhir_run63 _menhir_env (Obj.magic _menhir_stack) MenhirState49
        | LETREC ->
            _menhir_run54 _menhir_env (Obj.magic _menhir_stack) MenhirState49
        | LPAREN ->
            _menhir_run52 _menhir_env (Obj.magic _menhir_stack) MenhirState49
        | NEWREF ->
            _menhir_run50 _menhir_env (Obj.magic _menhir_stack) MenhirState49
        | NODE ->
            _menhir_run48 _menhir_env (Obj.magic _menhir_stack) MenhirState49
        | NULLT_QUESTION ->
            _menhir_run46 _menhir_env (Obj.magic _menhir_stack) MenhirState49
        | NULL_QUESTION ->
            _menhir_run44 _menhir_env (Obj.magic _menhir_stack) MenhirState49
        | PAIR ->
            _menhir_run42 _menhir_env (Obj.magic _menhir_stack) MenhirState49
        | PROC ->
            _menhir_run16 _menhir_env (Obj.magic _menhir_stack) MenhirState49
        | SET ->
            _menhir_run13 _menhir_env (Obj.magic _menhir_stack) MenhirState49
        | SETREF ->
            _menhir_run11 _menhir_env (Obj.magic _menhir_stack) MenhirState49
        | TL ->
            _menhir_run9 _menhir_env (Obj.magic _menhir_stack) MenhirState49
        | UNIT ->
            _menhir_run8 _menhir_env (Obj.magic _menhir_stack) MenhirState49
        | UNPAIR ->
            _menhir_run1 _menhir_env (Obj.magic _menhir_stack) MenhirState49
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState49)
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s

and _menhir_run50 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_stack = (_menhir_stack, _menhir_s) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | LPAREN ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_env = _menhir_discard _menhir_env in
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | BEGIN ->
            _menhir_run88 _menhir_env (Obj.magic _menhir_stack) MenhirState51
        | CONS ->
            _menhir_run86 _menhir_env (Obj.magic _menhir_stack) MenhirState51
        | DEBUG ->
            _menhir_run85 _menhir_env (Obj.magic _menhir_stack) MenhirState51
        | DEREF ->
            _menhir_run83 _menhir_env (Obj.magic _menhir_stack) MenhirState51
        | EMPTYLIST ->
            _menhir_run81 _menhir_env (Obj.magic _menhir_stack) MenhirState51
        | EMPTYTREE ->
            _menhir_run79 _menhir_env (Obj.magic _menhir_stack) MenhirState51
        | GETDATA ->
            _menhir_run77 _menhir_env (Obj.magic _menhir_stack) MenhirState51
        | GETLST ->
            _menhir_run75 _menhir_env (Obj.magic _menhir_stack) MenhirState51
        | GETRST ->
            _menhir_run73 _menhir_env (Obj.magic _menhir_stack) MenhirState51
        | HD ->
            _menhir_run71 _menhir_env (Obj.magic _menhir_stack) MenhirState51
        | ID _v ->
            _menhir_run70 _menhir_env (Obj.magic _menhir_stack) MenhirState51 _v
        | IF ->
            _menhir_run69 _menhir_env (Obj.magic _menhir_stack) MenhirState51
        | INT _v ->
            _menhir_run68 _menhir_env (Obj.magic _menhir_stack) MenhirState51 _v
        | ISZERO ->
            _menhir_run66 _menhir_env (Obj.magic _menhir_stack) MenhirState51
        | LET ->
            _menhir_run63 _menhir_env (Obj.magic _menhir_stack) MenhirState51
        | LETREC ->
            _menhir_run54 _menhir_env (Obj.magic _menhir_stack) MenhirState51
        | LPAREN ->
            _menhir_run52 _menhir_env (Obj.magic _menhir_stack) MenhirState51
        | NEWREF ->
            _menhir_run50 _menhir_env (Obj.magic _menhir_stack) MenhirState51
        | NODE ->
            _menhir_run48 _menhir_env (Obj.magic _menhir_stack) MenhirState51
        | NULLT_QUESTION ->
            _menhir_run46 _menhir_env (Obj.magic _menhir_stack) MenhirState51
        | NULL_QUESTION ->
            _menhir_run44 _menhir_env (Obj.magic _menhir_stack) MenhirState51
        | PAIR ->
            _menhir_run42 _menhir_env (Obj.magic _menhir_stack) MenhirState51
        | PROC ->
            _menhir_run16 _menhir_env (Obj.magic _menhir_stack) MenhirState51
        | SET ->
            _menhir_run13 _menhir_env (Obj.magic _menhir_stack) MenhirState51
        | SETREF ->
            _menhir_run11 _menhir_env (Obj.magic _menhir_stack) MenhirState51
        | TL ->
            _menhir_run9 _menhir_env (Obj.magic _menhir_stack) MenhirState51
        | UNIT ->
            _menhir_run8 _menhir_env (Obj.magic _menhir_stack) MenhirState51
        | UNPAIR ->
            _menhir_run1 _menhir_env (Obj.magic _menhir_stack) MenhirState51
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState51)
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s

and _menhir_run52 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_stack = (_menhir_stack, _menhir_s) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | BEGIN ->
        _menhir_run88 _menhir_env (Obj.magic _menhir_stack) MenhirState52
    | CONS ->
        _menhir_run86 _menhir_env (Obj.magic _menhir_stack) MenhirState52
    | DEBUG ->
        _menhir_run85 _menhir_env (Obj.magic _menhir_stack) MenhirState52
    | DEREF ->
        _menhir_run83 _menhir_env (Obj.magic _menhir_stack) MenhirState52
    | EMPTYLIST ->
        _menhir_run81 _menhir_env (Obj.magic _menhir_stack) MenhirState52
    | EMPTYTREE ->
        _menhir_run79 _menhir_env (Obj.magic _menhir_stack) MenhirState52
    | GETDATA ->
        _menhir_run77 _menhir_env (Obj.magic _menhir_stack) MenhirState52
    | GETLST ->
        _menhir_run75 _menhir_env (Obj.magic _menhir_stack) MenhirState52
    | GETRST ->
        _menhir_run73 _menhir_env (Obj.magic _menhir_stack) MenhirState52
    | HD ->
        _menhir_run71 _menhir_env (Obj.magic _menhir_stack) MenhirState52
    | ID _v ->
        _menhir_run70 _menhir_env (Obj.magic _menhir_stack) MenhirState52 _v
    | IF ->
        _menhir_run69 _menhir_env (Obj.magic _menhir_stack) MenhirState52
    | INT _v ->
        _menhir_run68 _menhir_env (Obj.magic _menhir_stack) MenhirState52 _v
    | ISZERO ->
        _menhir_run66 _menhir_env (Obj.magic _menhir_stack) MenhirState52
    | LET ->
        _menhir_run63 _menhir_env (Obj.magic _menhir_stack) MenhirState52
    | LETREC ->
        _menhir_run54 _menhir_env (Obj.magic _menhir_stack) MenhirState52
    | LPAREN ->
        _menhir_run52 _menhir_env (Obj.magic _menhir_stack) MenhirState52
    | MINUS ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_s = MenhirState52 in
        let _menhir_stack = (_menhir_stack, _menhir_s) in
        let _menhir_env = _menhir_discard _menhir_env in
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | BEGIN ->
            _menhir_run88 _menhir_env (Obj.magic _menhir_stack) MenhirState53
        | CONS ->
            _menhir_run86 _menhir_env (Obj.magic _menhir_stack) MenhirState53
        | DEBUG ->
            _menhir_run85 _menhir_env (Obj.magic _menhir_stack) MenhirState53
        | DEREF ->
            _menhir_run83 _menhir_env (Obj.magic _menhir_stack) MenhirState53
        | EMPTYLIST ->
            _menhir_run81 _menhir_env (Obj.magic _menhir_stack) MenhirState53
        | EMPTYTREE ->
            _menhir_run79 _menhir_env (Obj.magic _menhir_stack) MenhirState53
        | GETDATA ->
            _menhir_run77 _menhir_env (Obj.magic _menhir_stack) MenhirState53
        | GETLST ->
            _menhir_run75 _menhir_env (Obj.magic _menhir_stack) MenhirState53
        | GETRST ->
            _menhir_run73 _menhir_env (Obj.magic _menhir_stack) MenhirState53
        | HD ->
            _menhir_run71 _menhir_env (Obj.magic _menhir_stack) MenhirState53
        | ID _v ->
            _menhir_run70 _menhir_env (Obj.magic _menhir_stack) MenhirState53 _v
        | IF ->
            _menhir_run69 _menhir_env (Obj.magic _menhir_stack) MenhirState53
        | INT _v ->
            _menhir_run68 _menhir_env (Obj.magic _menhir_stack) MenhirState53 _v
        | ISZERO ->
            _menhir_run66 _menhir_env (Obj.magic _menhir_stack) MenhirState53
        | LET ->
            _menhir_run63 _menhir_env (Obj.magic _menhir_stack) MenhirState53
        | LETREC ->
            _menhir_run54 _menhir_env (Obj.magic _menhir_stack) MenhirState53
        | LPAREN ->
            _menhir_run52 _menhir_env (Obj.magic _menhir_stack) MenhirState53
        | NEWREF ->
            _menhir_run50 _menhir_env (Obj.magic _menhir_stack) MenhirState53
        | NODE ->
            _menhir_run48 _menhir_env (Obj.magic _menhir_stack) MenhirState53
        | NULLT_QUESTION ->
            _menhir_run46 _menhir_env (Obj.magic _menhir_stack) MenhirState53
        | NULL_QUESTION ->
            _menhir_run44 _menhir_env (Obj.magic _menhir_stack) MenhirState53
        | PAIR ->
            _menhir_run42 _menhir_env (Obj.magic _menhir_stack) MenhirState53
        | PROC ->
            _menhir_run16 _menhir_env (Obj.magic _menhir_stack) MenhirState53
        | SET ->
            _menhir_run13 _menhir_env (Obj.magic _menhir_stack) MenhirState53
        | SETREF ->
            _menhir_run11 _menhir_env (Obj.magic _menhir_stack) MenhirState53
        | TL ->
            _menhir_run9 _menhir_env (Obj.magic _menhir_stack) MenhirState53
        | UNIT ->
            _menhir_run8 _menhir_env (Obj.magic _menhir_stack) MenhirState53
        | UNPAIR ->
            _menhir_run1 _menhir_env (Obj.magic _menhir_stack) MenhirState53
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState53)
    | NEWREF ->
        _menhir_run50 _menhir_env (Obj.magic _menhir_stack) MenhirState52
    | NODE ->
        _menhir_run48 _menhir_env (Obj.magic _menhir_stack) MenhirState52
    | NULLT_QUESTION ->
        _menhir_run46 _menhir_env (Obj.magic _menhir_stack) MenhirState52
    | NULL_QUESTION ->
        _menhir_run44 _menhir_env (Obj.magic _menhir_stack) MenhirState52
    | PAIR ->
        _menhir_run42 _menhir_env (Obj.magic _menhir_stack) MenhirState52
    | PROC ->
        _menhir_run16 _menhir_env (Obj.magic _menhir_stack) MenhirState52
    | SET ->
        _menhir_run13 _menhir_env (Obj.magic _menhir_stack) MenhirState52
    | SETREF ->
        _menhir_run11 _menhir_env (Obj.magic _menhir_stack) MenhirState52
    | TL ->
        _menhir_run9 _menhir_env (Obj.magic _menhir_stack) MenhirState52
    | UNIT ->
        _menhir_run8 _menhir_env (Obj.magic _menhir_stack) MenhirState52
    | UNPAIR ->
        _menhir_run1 _menhir_env (Obj.magic _menhir_stack) MenhirState52
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState52

and _menhir_run54 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_stack = (_menhir_stack, _menhir_s) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | BOOLTYPE ->
        _menhir_run27 _menhir_env (Obj.magic _menhir_stack) MenhirState54
    | INTTYPE ->
        _menhir_run26 _menhir_env (Obj.magic _menhir_stack) MenhirState54
    | LESS_THAN ->
        _menhir_run25 _menhir_env (Obj.magic _menhir_stack) MenhirState54
    | LISTTYPE ->
        _menhir_run24 _menhir_env (Obj.magic _menhir_stack) MenhirState54
    | LPAREN ->
        _menhir_run23 _menhir_env (Obj.magic _menhir_stack) MenhirState54
    | REFTYPE ->
        _menhir_run22 _menhir_env (Obj.magic _menhir_stack) MenhirState54
    | TREETYPE ->
        _menhir_run21 _menhir_env (Obj.magic _menhir_stack) MenhirState54
    | UNITTYPE ->
        _menhir_run20 _menhir_env (Obj.magic _menhir_stack) MenhirState54
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState54

and _menhir_run63 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_stack = (_menhir_stack, _menhir_s) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | ID _v ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_stack = (_menhir_stack, _v) in
        let _menhir_env = _menhir_discard _menhir_env in
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | EQUALS ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _menhir_env = _menhir_discard _menhir_env in
            let _tok = _menhir_env._menhir_token in
            (match _tok with
            | BEGIN ->
                _menhir_run88 _menhir_env (Obj.magic _menhir_stack) MenhirState65
            | CONS ->
                _menhir_run86 _menhir_env (Obj.magic _menhir_stack) MenhirState65
            | DEBUG ->
                _menhir_run85 _menhir_env (Obj.magic _menhir_stack) MenhirState65
            | DEREF ->
                _menhir_run83 _menhir_env (Obj.magic _menhir_stack) MenhirState65
            | EMPTYLIST ->
                _menhir_run81 _menhir_env (Obj.magic _menhir_stack) MenhirState65
            | EMPTYTREE ->
                _menhir_run79 _menhir_env (Obj.magic _menhir_stack) MenhirState65
            | GETDATA ->
                _menhir_run77 _menhir_env (Obj.magic _menhir_stack) MenhirState65
            | GETLST ->
                _menhir_run75 _menhir_env (Obj.magic _menhir_stack) MenhirState65
            | GETRST ->
                _menhir_run73 _menhir_env (Obj.magic _menhir_stack) MenhirState65
            | HD ->
                _menhir_run71 _menhir_env (Obj.magic _menhir_stack) MenhirState65
            | ID _v ->
                _menhir_run70 _menhir_env (Obj.magic _menhir_stack) MenhirState65 _v
            | IF ->
                _menhir_run69 _menhir_env (Obj.magic _menhir_stack) MenhirState65
            | INT _v ->
                _menhir_run68 _menhir_env (Obj.magic _menhir_stack) MenhirState65 _v
            | ISZERO ->
                _menhir_run66 _menhir_env (Obj.magic _menhir_stack) MenhirState65
            | LET ->
                _menhir_run63 _menhir_env (Obj.magic _menhir_stack) MenhirState65
            | LETREC ->
                _menhir_run54 _menhir_env (Obj.magic _menhir_stack) MenhirState65
            | LPAREN ->
                _menhir_run52 _menhir_env (Obj.magic _menhir_stack) MenhirState65
            | NEWREF ->
                _menhir_run50 _menhir_env (Obj.magic _menhir_stack) MenhirState65
            | NODE ->
                _menhir_run48 _menhir_env (Obj.magic _menhir_stack) MenhirState65
            | NULLT_QUESTION ->
                _menhir_run46 _menhir_env (Obj.magic _menhir_stack) MenhirState65
            | NULL_QUESTION ->
                _menhir_run44 _menhir_env (Obj.magic _menhir_stack) MenhirState65
            | PAIR ->
                _menhir_run42 _menhir_env (Obj.magic _menhir_stack) MenhirState65
            | PROC ->
                _menhir_run16 _menhir_env (Obj.magic _menhir_stack) MenhirState65
            | SET ->
                _menhir_run13 _menhir_env (Obj.magic _menhir_stack) MenhirState65
            | SETREF ->
                _menhir_run11 _menhir_env (Obj.magic _menhir_stack) MenhirState65
            | TL ->
                _menhir_run9 _menhir_env (Obj.magic _menhir_stack) MenhirState65
            | UNIT ->
                _menhir_run8 _menhir_env (Obj.magic _menhir_stack) MenhirState65
            | UNPAIR ->
                _menhir_run1 _menhir_env (Obj.magic _menhir_stack) MenhirState65
            | _ ->
                assert (not _menhir_env._menhir_error);
                _menhir_env._menhir_error <- true;
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState65)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let _menhir_stack = Obj.magic _menhir_stack in
            let ((_menhir_stack, _menhir_s), _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s

and _menhir_run66 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_stack = (_menhir_stack, _menhir_s) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | LPAREN ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_env = _menhir_discard _menhir_env in
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | BEGIN ->
            _menhir_run88 _menhir_env (Obj.magic _menhir_stack) MenhirState67
        | CONS ->
            _menhir_run86 _menhir_env (Obj.magic _menhir_stack) MenhirState67
        | DEBUG ->
            _menhir_run85 _menhir_env (Obj.magic _menhir_stack) MenhirState67
        | DEREF ->
            _menhir_run83 _menhir_env (Obj.magic _menhir_stack) MenhirState67
        | EMPTYLIST ->
            _menhir_run81 _menhir_env (Obj.magic _menhir_stack) MenhirState67
        | EMPTYTREE ->
            _menhir_run79 _menhir_env (Obj.magic _menhir_stack) MenhirState67
        | GETDATA ->
            _menhir_run77 _menhir_env (Obj.magic _menhir_stack) MenhirState67
        | GETLST ->
            _menhir_run75 _menhir_env (Obj.magic _menhir_stack) MenhirState67
        | GETRST ->
            _menhir_run73 _menhir_env (Obj.magic _menhir_stack) MenhirState67
        | HD ->
            _menhir_run71 _menhir_env (Obj.magic _menhir_stack) MenhirState67
        | ID _v ->
            _menhir_run70 _menhir_env (Obj.magic _menhir_stack) MenhirState67 _v
        | IF ->
            _menhir_run69 _menhir_env (Obj.magic _menhir_stack) MenhirState67
        | INT _v ->
            _menhir_run68 _menhir_env (Obj.magic _menhir_stack) MenhirState67 _v
        | ISZERO ->
            _menhir_run66 _menhir_env (Obj.magic _menhir_stack) MenhirState67
        | LET ->
            _menhir_run63 _menhir_env (Obj.magic _menhir_stack) MenhirState67
        | LETREC ->
            _menhir_run54 _menhir_env (Obj.magic _menhir_stack) MenhirState67
        | LPAREN ->
            _menhir_run52 _menhir_env (Obj.magic _menhir_stack) MenhirState67
        | NEWREF ->
            _menhir_run50 _menhir_env (Obj.magic _menhir_stack) MenhirState67
        | NODE ->
            _menhir_run48 _menhir_env (Obj.magic _menhir_stack) MenhirState67
        | NULLT_QUESTION ->
            _menhir_run46 _menhir_env (Obj.magic _menhir_stack) MenhirState67
        | NULL_QUESTION ->
            _menhir_run44 _menhir_env (Obj.magic _menhir_stack) MenhirState67
        | PAIR ->
            _menhir_run42 _menhir_env (Obj.magic _menhir_stack) MenhirState67
        | PROC ->
            _menhir_run16 _menhir_env (Obj.magic _menhir_stack) MenhirState67
        | SET ->
            _menhir_run13 _menhir_env (Obj.magic _menhir_stack) MenhirState67
        | SETREF ->
            _menhir_run11 _menhir_env (Obj.magic _menhir_stack) MenhirState67
        | TL ->
            _menhir_run9 _menhir_env (Obj.magic _menhir_stack) MenhirState67
        | UNIT ->
            _menhir_run8 _menhir_env (Obj.magic _menhir_stack) MenhirState67
        | UNPAIR ->
            _menhir_run1 _menhir_env (Obj.magic _menhir_stack) MenhirState67
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState67)
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s

and _menhir_run68 : _menhir_env -> 'ttv_tail -> _menhir_state -> (
# 22 "parser.mly"
       (int)
# 4603 "parser.ml"
) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_env = _menhir_discard _menhir_env in
    let _menhir_stack = Obj.magic _menhir_stack in
    let (i : (
# 22 "parser.mly"
       (int)
# 4611 "parser.ml"
    )) = _v in
    let _v : (Ast.expr) = 
# 165 "parser.mly"
              ( Int i )
# 4616 "parser.ml"
     in
    _menhir_goto_expr _menhir_env _menhir_stack _menhir_s _v

and _menhir_run69 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_stack = (_menhir_stack, _menhir_s) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | BEGIN ->
        _menhir_run88 _menhir_env (Obj.magic _menhir_stack) MenhirState69
    | CONS ->
        _menhir_run86 _menhir_env (Obj.magic _menhir_stack) MenhirState69
    | DEBUG ->
        _menhir_run85 _menhir_env (Obj.magic _menhir_stack) MenhirState69
    | DEREF ->
        _menhir_run83 _menhir_env (Obj.magic _menhir_stack) MenhirState69
    | EMPTYLIST ->
        _menhir_run81 _menhir_env (Obj.magic _menhir_stack) MenhirState69
    | EMPTYTREE ->
        _menhir_run79 _menhir_env (Obj.magic _menhir_stack) MenhirState69
    | GETDATA ->
        _menhir_run77 _menhir_env (Obj.magic _menhir_stack) MenhirState69
    | GETLST ->
        _menhir_run75 _menhir_env (Obj.magic _menhir_stack) MenhirState69
    | GETRST ->
        _menhir_run73 _menhir_env (Obj.magic _menhir_stack) MenhirState69
    | HD ->
        _menhir_run71 _menhir_env (Obj.magic _menhir_stack) MenhirState69
    | ID _v ->
        _menhir_run70 _menhir_env (Obj.magic _menhir_stack) MenhirState69 _v
    | IF ->
        _menhir_run69 _menhir_env (Obj.magic _menhir_stack) MenhirState69
    | INT _v ->
        _menhir_run68 _menhir_env (Obj.magic _menhir_stack) MenhirState69 _v
    | ISZERO ->
        _menhir_run66 _menhir_env (Obj.magic _menhir_stack) MenhirState69
    | LET ->
        _menhir_run63 _menhir_env (Obj.magic _menhir_stack) MenhirState69
    | LETREC ->
        _menhir_run54 _menhir_env (Obj.magic _menhir_stack) MenhirState69
    | LPAREN ->
        _menhir_run52 _menhir_env (Obj.magic _menhir_stack) MenhirState69
    | NEWREF ->
        _menhir_run50 _menhir_env (Obj.magic _menhir_stack) MenhirState69
    | NODE ->
        _menhir_run48 _menhir_env (Obj.magic _menhir_stack) MenhirState69
    | NULLT_QUESTION ->
        _menhir_run46 _menhir_env (Obj.magic _menhir_stack) MenhirState69
    | NULL_QUESTION ->
        _menhir_run44 _menhir_env (Obj.magic _menhir_stack) MenhirState69
    | PAIR ->
        _menhir_run42 _menhir_env (Obj.magic _menhir_stack) MenhirState69
    | PROC ->
        _menhir_run16 _menhir_env (Obj.magic _menhir_stack) MenhirState69
    | SET ->
        _menhir_run13 _menhir_env (Obj.magic _menhir_stack) MenhirState69
    | SETREF ->
        _menhir_run11 _menhir_env (Obj.magic _menhir_stack) MenhirState69
    | TL ->
        _menhir_run9 _menhir_env (Obj.magic _menhir_stack) MenhirState69
    | UNIT ->
        _menhir_run8 _menhir_env (Obj.magic _menhir_stack) MenhirState69
    | UNPAIR ->
        _menhir_run1 _menhir_env (Obj.magic _menhir_stack) MenhirState69
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState69

and _menhir_run70 : _menhir_env -> 'ttv_tail -> _menhir_state -> (
# 23 "parser.mly"
       (string)
# 4690 "parser.ml"
) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_env = _menhir_discard _menhir_env in
    let _menhir_stack = Obj.magic _menhir_stack in
    let (x : (
# 23 "parser.mly"
       (string)
# 4698 "parser.ml"
    )) = _v in
    let _v : (Ast.expr) = 
# 166 "parser.mly"
             ( Var x )
# 4703 "parser.ml"
     in
    _menhir_goto_expr _menhir_env _menhir_stack _menhir_s _v

and _menhir_run71 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_stack = (_menhir_stack, _menhir_s) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | LPAREN ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_env = _menhir_discard _menhir_env in
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | BEGIN ->
            _menhir_run88 _menhir_env (Obj.magic _menhir_stack) MenhirState72
        | CONS ->
            _menhir_run86 _menhir_env (Obj.magic _menhir_stack) MenhirState72
        | DEBUG ->
            _menhir_run85 _menhir_env (Obj.magic _menhir_stack) MenhirState72
        | DEREF ->
            _menhir_run83 _menhir_env (Obj.magic _menhir_stack) MenhirState72
        | EMPTYLIST ->
            _menhir_run81 _menhir_env (Obj.magic _menhir_stack) MenhirState72
        | EMPTYTREE ->
            _menhir_run79 _menhir_env (Obj.magic _menhir_stack) MenhirState72
        | GETDATA ->
            _menhir_run77 _menhir_env (Obj.magic _menhir_stack) MenhirState72
        | GETLST ->
            _menhir_run75 _menhir_env (Obj.magic _menhir_stack) MenhirState72
        | GETRST ->
            _menhir_run73 _menhir_env (Obj.magic _menhir_stack) MenhirState72
        | HD ->
            _menhir_run71 _menhir_env (Obj.magic _menhir_stack) MenhirState72
        | ID _v ->
            _menhir_run70 _menhir_env (Obj.magic _menhir_stack) MenhirState72 _v
        | IF ->
            _menhir_run69 _menhir_env (Obj.magic _menhir_stack) MenhirState72
        | INT _v ->
            _menhir_run68 _menhir_env (Obj.magic _menhir_stack) MenhirState72 _v
        | ISZERO ->
            _menhir_run66 _menhir_env (Obj.magic _menhir_stack) MenhirState72
        | LET ->
            _menhir_run63 _menhir_env (Obj.magic _menhir_stack) MenhirState72
        | LETREC ->
            _menhir_run54 _menhir_env (Obj.magic _menhir_stack) MenhirState72
        | LPAREN ->
            _menhir_run52 _menhir_env (Obj.magic _menhir_stack) MenhirState72
        | NEWREF ->
            _menhir_run50 _menhir_env (Obj.magic _menhir_stack) MenhirState72
        | NODE ->
            _menhir_run48 _menhir_env (Obj.magic _menhir_stack) MenhirState72
        | NULLT_QUESTION ->
            _menhir_run46 _menhir_env (Obj.magic _menhir_stack) MenhirState72
        | NULL_QUESTION ->
            _menhir_run44 _menhir_env (Obj.magic _menhir_stack) MenhirState72
        | PAIR ->
            _menhir_run42 _menhir_env (Obj.magic _menhir_stack) MenhirState72
        | PROC ->
            _menhir_run16 _menhir_env (Obj.magic _menhir_stack) MenhirState72
        | SET ->
            _menhir_run13 _menhir_env (Obj.magic _menhir_stack) MenhirState72
        | SETREF ->
            _menhir_run11 _menhir_env (Obj.magic _menhir_stack) MenhirState72
        | TL ->
            _menhir_run9 _menhir_env (Obj.magic _menhir_stack) MenhirState72
        | UNIT ->
            _menhir_run8 _menhir_env (Obj.magic _menhir_stack) MenhirState72
        | UNPAIR ->
            _menhir_run1 _menhir_env (Obj.magic _menhir_stack) MenhirState72
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState72)
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s

and _menhir_run73 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_stack = (_menhir_stack, _menhir_s) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | LPAREN ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_env = _menhir_discard _menhir_env in
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | BEGIN ->
            _menhir_run88 _menhir_env (Obj.magic _menhir_stack) MenhirState74
        | CONS ->
            _menhir_run86 _menhir_env (Obj.magic _menhir_stack) MenhirState74
        | DEBUG ->
            _menhir_run85 _menhir_env (Obj.magic _menhir_stack) MenhirState74
        | DEREF ->
            _menhir_run83 _menhir_env (Obj.magic _menhir_stack) MenhirState74
        | EMPTYLIST ->
            _menhir_run81 _menhir_env (Obj.magic _menhir_stack) MenhirState74
        | EMPTYTREE ->
            _menhir_run79 _menhir_env (Obj.magic _menhir_stack) MenhirState74
        | GETDATA ->
            _menhir_run77 _menhir_env (Obj.magic _menhir_stack) MenhirState74
        | GETLST ->
            _menhir_run75 _menhir_env (Obj.magic _menhir_stack) MenhirState74
        | GETRST ->
            _menhir_run73 _menhir_env (Obj.magic _menhir_stack) MenhirState74
        | HD ->
            _menhir_run71 _menhir_env (Obj.magic _menhir_stack) MenhirState74
        | ID _v ->
            _menhir_run70 _menhir_env (Obj.magic _menhir_stack) MenhirState74 _v
        | IF ->
            _menhir_run69 _menhir_env (Obj.magic _menhir_stack) MenhirState74
        | INT _v ->
            _menhir_run68 _menhir_env (Obj.magic _menhir_stack) MenhirState74 _v
        | ISZERO ->
            _menhir_run66 _menhir_env (Obj.magic _menhir_stack) MenhirState74
        | LET ->
            _menhir_run63 _menhir_env (Obj.magic _menhir_stack) MenhirState74
        | LETREC ->
            _menhir_run54 _menhir_env (Obj.magic _menhir_stack) MenhirState74
        | LPAREN ->
            _menhir_run52 _menhir_env (Obj.magic _menhir_stack) MenhirState74
        | NEWREF ->
            _menhir_run50 _menhir_env (Obj.magic _menhir_stack) MenhirState74
        | NODE ->
            _menhir_run48 _menhir_env (Obj.magic _menhir_stack) MenhirState74
        | NULLT_QUESTION ->
            _menhir_run46 _menhir_env (Obj.magic _menhir_stack) MenhirState74
        | NULL_QUESTION ->
            _menhir_run44 _menhir_env (Obj.magic _menhir_stack) MenhirState74
        | PAIR ->
            _menhir_run42 _menhir_env (Obj.magic _menhir_stack) MenhirState74
        | PROC ->
            _menhir_run16 _menhir_env (Obj.magic _menhir_stack) MenhirState74
        | SET ->
            _menhir_run13 _menhir_env (Obj.magic _menhir_stack) MenhirState74
        | SETREF ->
            _menhir_run11 _menhir_env (Obj.magic _menhir_stack) MenhirState74
        | TL ->
            _menhir_run9 _menhir_env (Obj.magic _menhir_stack) MenhirState74
        | UNIT ->
            _menhir_run8 _menhir_env (Obj.magic _menhir_stack) MenhirState74
        | UNPAIR ->
            _menhir_run1 _menhir_env (Obj.magic _menhir_stack) MenhirState74
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState74)
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s

and _menhir_run75 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_stack = (_menhir_stack, _menhir_s) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | LPAREN ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_env = _menhir_discard _menhir_env in
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | BEGIN ->
            _menhir_run88 _menhir_env (Obj.magic _menhir_stack) MenhirState76
        | CONS ->
            _menhir_run86 _menhir_env (Obj.magic _menhir_stack) MenhirState76
        | DEBUG ->
            _menhir_run85 _menhir_env (Obj.magic _menhir_stack) MenhirState76
        | DEREF ->
            _menhir_run83 _menhir_env (Obj.magic _menhir_stack) MenhirState76
        | EMPTYLIST ->
            _menhir_run81 _menhir_env (Obj.magic _menhir_stack) MenhirState76
        | EMPTYTREE ->
            _menhir_run79 _menhir_env (Obj.magic _menhir_stack) MenhirState76
        | GETDATA ->
            _menhir_run77 _menhir_env (Obj.magic _menhir_stack) MenhirState76
        | GETLST ->
            _menhir_run75 _menhir_env (Obj.magic _menhir_stack) MenhirState76
        | GETRST ->
            _menhir_run73 _menhir_env (Obj.magic _menhir_stack) MenhirState76
        | HD ->
            _menhir_run71 _menhir_env (Obj.magic _menhir_stack) MenhirState76
        | ID _v ->
            _menhir_run70 _menhir_env (Obj.magic _menhir_stack) MenhirState76 _v
        | IF ->
            _menhir_run69 _menhir_env (Obj.magic _menhir_stack) MenhirState76
        | INT _v ->
            _menhir_run68 _menhir_env (Obj.magic _menhir_stack) MenhirState76 _v
        | ISZERO ->
            _menhir_run66 _menhir_env (Obj.magic _menhir_stack) MenhirState76
        | LET ->
            _menhir_run63 _menhir_env (Obj.magic _menhir_stack) MenhirState76
        | LETREC ->
            _menhir_run54 _menhir_env (Obj.magic _menhir_stack) MenhirState76
        | LPAREN ->
            _menhir_run52 _menhir_env (Obj.magic _menhir_stack) MenhirState76
        | NEWREF ->
            _menhir_run50 _menhir_env (Obj.magic _menhir_stack) MenhirState76
        | NODE ->
            _menhir_run48 _menhir_env (Obj.magic _menhir_stack) MenhirState76
        | NULLT_QUESTION ->
            _menhir_run46 _menhir_env (Obj.magic _menhir_stack) MenhirState76
        | NULL_QUESTION ->
            _menhir_run44 _menhir_env (Obj.magic _menhir_stack) MenhirState76
        | PAIR ->
            _menhir_run42 _menhir_env (Obj.magic _menhir_stack) MenhirState76
        | PROC ->
            _menhir_run16 _menhir_env (Obj.magic _menhir_stack) MenhirState76
        | SET ->
            _menhir_run13 _menhir_env (Obj.magic _menhir_stack) MenhirState76
        | SETREF ->
            _menhir_run11 _menhir_env (Obj.magic _menhir_stack) MenhirState76
        | TL ->
            _menhir_run9 _menhir_env (Obj.magic _menhir_stack) MenhirState76
        | UNIT ->
            _menhir_run8 _menhir_env (Obj.magic _menhir_stack) MenhirState76
        | UNPAIR ->
            _menhir_run1 _menhir_env (Obj.magic _menhir_stack) MenhirState76
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState76)
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s

and _menhir_run77 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_stack = (_menhir_stack, _menhir_s) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | LPAREN ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_env = _menhir_discard _menhir_env in
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | BEGIN ->
            _menhir_run88 _menhir_env (Obj.magic _menhir_stack) MenhirState78
        | CONS ->
            _menhir_run86 _menhir_env (Obj.magic _menhir_stack) MenhirState78
        | DEBUG ->
            _menhir_run85 _menhir_env (Obj.magic _menhir_stack) MenhirState78
        | DEREF ->
            _menhir_run83 _menhir_env (Obj.magic _menhir_stack) MenhirState78
        | EMPTYLIST ->
            _menhir_run81 _menhir_env (Obj.magic _menhir_stack) MenhirState78
        | EMPTYTREE ->
            _menhir_run79 _menhir_env (Obj.magic _menhir_stack) MenhirState78
        | GETDATA ->
            _menhir_run77 _menhir_env (Obj.magic _menhir_stack) MenhirState78
        | GETLST ->
            _menhir_run75 _menhir_env (Obj.magic _menhir_stack) MenhirState78
        | GETRST ->
            _menhir_run73 _menhir_env (Obj.magic _menhir_stack) MenhirState78
        | HD ->
            _menhir_run71 _menhir_env (Obj.magic _menhir_stack) MenhirState78
        | ID _v ->
            _menhir_run70 _menhir_env (Obj.magic _menhir_stack) MenhirState78 _v
        | IF ->
            _menhir_run69 _menhir_env (Obj.magic _menhir_stack) MenhirState78
        | INT _v ->
            _menhir_run68 _menhir_env (Obj.magic _menhir_stack) MenhirState78 _v
        | ISZERO ->
            _menhir_run66 _menhir_env (Obj.magic _menhir_stack) MenhirState78
        | LET ->
            _menhir_run63 _menhir_env (Obj.magic _menhir_stack) MenhirState78
        | LETREC ->
            _menhir_run54 _menhir_env (Obj.magic _menhir_stack) MenhirState78
        | LPAREN ->
            _menhir_run52 _menhir_env (Obj.magic _menhir_stack) MenhirState78
        | NEWREF ->
            _menhir_run50 _menhir_env (Obj.magic _menhir_stack) MenhirState78
        | NODE ->
            _menhir_run48 _menhir_env (Obj.magic _menhir_stack) MenhirState78
        | NULLT_QUESTION ->
            _menhir_run46 _menhir_env (Obj.magic _menhir_stack) MenhirState78
        | NULL_QUESTION ->
            _menhir_run44 _menhir_env (Obj.magic _menhir_stack) MenhirState78
        | PAIR ->
            _menhir_run42 _menhir_env (Obj.magic _menhir_stack) MenhirState78
        | PROC ->
            _menhir_run16 _menhir_env (Obj.magic _menhir_stack) MenhirState78
        | SET ->
            _menhir_run13 _menhir_env (Obj.magic _menhir_stack) MenhirState78
        | SETREF ->
            _menhir_run11 _menhir_env (Obj.magic _menhir_stack) MenhirState78
        | TL ->
            _menhir_run9 _menhir_env (Obj.magic _menhir_stack) MenhirState78
        | UNIT ->
            _menhir_run8 _menhir_env (Obj.magic _menhir_stack) MenhirState78
        | UNPAIR ->
            _menhir_run1 _menhir_env (Obj.magic _menhir_stack) MenhirState78
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState78)
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s

and _menhir_run79 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_stack = (_menhir_stack, _menhir_s) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | BOOLTYPE ->
        _menhir_run27 _menhir_env (Obj.magic _menhir_stack) MenhirState79
    | INTTYPE ->
        _menhir_run26 _menhir_env (Obj.magic _menhir_stack) MenhirState79
    | LESS_THAN ->
        _menhir_run25 _menhir_env (Obj.magic _menhir_stack) MenhirState79
    | LISTTYPE ->
        _menhir_run24 _menhir_env (Obj.magic _menhir_stack) MenhirState79
    | LPAREN ->
        _menhir_run23 _menhir_env (Obj.magic _menhir_stack) MenhirState79
    | REFTYPE ->
        _menhir_run22 _menhir_env (Obj.magic _menhir_stack) MenhirState79
    | TREETYPE ->
        _menhir_run21 _menhir_env (Obj.magic _menhir_stack) MenhirState79
    | UNITTYPE ->
        _menhir_run20 _menhir_env (Obj.magic _menhir_stack) MenhirState79
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState79

and _menhir_run81 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_stack = (_menhir_stack, _menhir_s) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | BOOLTYPE ->
        _menhir_run27 _menhir_env (Obj.magic _menhir_stack) MenhirState81
    | INTTYPE ->
        _menhir_run26 _menhir_env (Obj.magic _menhir_stack) MenhirState81
    | LESS_THAN ->
        _menhir_run25 _menhir_env (Obj.magic _menhir_stack) MenhirState81
    | LISTTYPE ->
        _menhir_run24 _menhir_env (Obj.magic _menhir_stack) MenhirState81
    | LPAREN ->
        _menhir_run23 _menhir_env (Obj.magic _menhir_stack) MenhirState81
    | REFTYPE ->
        _menhir_run22 _menhir_env (Obj.magic _menhir_stack) MenhirState81
    | TREETYPE ->
        _menhir_run21 _menhir_env (Obj.magic _menhir_stack) MenhirState81
    | UNITTYPE ->
        _menhir_run20 _menhir_env (Obj.magic _menhir_stack) MenhirState81
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState81

and _menhir_run83 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_stack = (_menhir_stack, _menhir_s) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | LPAREN ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_env = _menhir_discard _menhir_env in
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | BEGIN ->
            _menhir_run88 _menhir_env (Obj.magic _menhir_stack) MenhirState84
        | CONS ->
            _menhir_run86 _menhir_env (Obj.magic _menhir_stack) MenhirState84
        | DEBUG ->
            _menhir_run85 _menhir_env (Obj.magic _menhir_stack) MenhirState84
        | DEREF ->
            _menhir_run83 _menhir_env (Obj.magic _menhir_stack) MenhirState84
        | EMPTYLIST ->
            _menhir_run81 _menhir_env (Obj.magic _menhir_stack) MenhirState84
        | EMPTYTREE ->
            _menhir_run79 _menhir_env (Obj.magic _menhir_stack) MenhirState84
        | GETDATA ->
            _menhir_run77 _menhir_env (Obj.magic _menhir_stack) MenhirState84
        | GETLST ->
            _menhir_run75 _menhir_env (Obj.magic _menhir_stack) MenhirState84
        | GETRST ->
            _menhir_run73 _menhir_env (Obj.magic _menhir_stack) MenhirState84
        | HD ->
            _menhir_run71 _menhir_env (Obj.magic _menhir_stack) MenhirState84
        | ID _v ->
            _menhir_run70 _menhir_env (Obj.magic _menhir_stack) MenhirState84 _v
        | IF ->
            _menhir_run69 _menhir_env (Obj.magic _menhir_stack) MenhirState84
        | INT _v ->
            _menhir_run68 _menhir_env (Obj.magic _menhir_stack) MenhirState84 _v
        | ISZERO ->
            _menhir_run66 _menhir_env (Obj.magic _menhir_stack) MenhirState84
        | LET ->
            _menhir_run63 _menhir_env (Obj.magic _menhir_stack) MenhirState84
        | LETREC ->
            _menhir_run54 _menhir_env (Obj.magic _menhir_stack) MenhirState84
        | LPAREN ->
            _menhir_run52 _menhir_env (Obj.magic _menhir_stack) MenhirState84
        | NEWREF ->
            _menhir_run50 _menhir_env (Obj.magic _menhir_stack) MenhirState84
        | NODE ->
            _menhir_run48 _menhir_env (Obj.magic _menhir_stack) MenhirState84
        | NULLT_QUESTION ->
            _menhir_run46 _menhir_env (Obj.magic _menhir_stack) MenhirState84
        | NULL_QUESTION ->
            _menhir_run44 _menhir_env (Obj.magic _menhir_stack) MenhirState84
        | PAIR ->
            _menhir_run42 _menhir_env (Obj.magic _menhir_stack) MenhirState84
        | PROC ->
            _menhir_run16 _menhir_env (Obj.magic _menhir_stack) MenhirState84
        | SET ->
            _menhir_run13 _menhir_env (Obj.magic _menhir_stack) MenhirState84
        | SETREF ->
            _menhir_run11 _menhir_env (Obj.magic _menhir_stack) MenhirState84
        | TL ->
            _menhir_run9 _menhir_env (Obj.magic _menhir_stack) MenhirState84
        | UNIT ->
            _menhir_run8 _menhir_env (Obj.magic _menhir_stack) MenhirState84
        | UNPAIR ->
            _menhir_run1 _menhir_env (Obj.magic _menhir_stack) MenhirState84
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState84)
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s

and _menhir_run85 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_env = _menhir_discard _menhir_env in
    let _menhir_stack = Obj.magic _menhir_stack in
    let _1 = () in
    let _v : (Ast.expr) = 
# 167 "parser.mly"
            ( Debug )
# 5159 "parser.ml"
     in
    _menhir_goto_expr _menhir_env _menhir_stack _menhir_s _v

and _menhir_run86 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_stack = (_menhir_stack, _menhir_s) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | LPAREN ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_env = _menhir_discard _menhir_env in
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | BEGIN ->
            _menhir_run88 _menhir_env (Obj.magic _menhir_stack) MenhirState87
        | CONS ->
            _menhir_run86 _menhir_env (Obj.magic _menhir_stack) MenhirState87
        | DEBUG ->
            _menhir_run85 _menhir_env (Obj.magic _menhir_stack) MenhirState87
        | DEREF ->
            _menhir_run83 _menhir_env (Obj.magic _menhir_stack) MenhirState87
        | EMPTYLIST ->
            _menhir_run81 _menhir_env (Obj.magic _menhir_stack) MenhirState87
        | EMPTYTREE ->
            _menhir_run79 _menhir_env (Obj.magic _menhir_stack) MenhirState87
        | GETDATA ->
            _menhir_run77 _menhir_env (Obj.magic _menhir_stack) MenhirState87
        | GETLST ->
            _menhir_run75 _menhir_env (Obj.magic _menhir_stack) MenhirState87
        | GETRST ->
            _menhir_run73 _menhir_env (Obj.magic _menhir_stack) MenhirState87
        | HD ->
            _menhir_run71 _menhir_env (Obj.magic _menhir_stack) MenhirState87
        | ID _v ->
            _menhir_run70 _menhir_env (Obj.magic _menhir_stack) MenhirState87 _v
        | IF ->
            _menhir_run69 _menhir_env (Obj.magic _menhir_stack) MenhirState87
        | INT _v ->
            _menhir_run68 _menhir_env (Obj.magic _menhir_stack) MenhirState87 _v
        | ISZERO ->
            _menhir_run66 _menhir_env (Obj.magic _menhir_stack) MenhirState87
        | LET ->
            _menhir_run63 _menhir_env (Obj.magic _menhir_stack) MenhirState87
        | LETREC ->
            _menhir_run54 _menhir_env (Obj.magic _menhir_stack) MenhirState87
        | LPAREN ->
            _menhir_run52 _menhir_env (Obj.magic _menhir_stack) MenhirState87
        | NEWREF ->
            _menhir_run50 _menhir_env (Obj.magic _menhir_stack) MenhirState87
        | NODE ->
            _menhir_run48 _menhir_env (Obj.magic _menhir_stack) MenhirState87
        | NULLT_QUESTION ->
            _menhir_run46 _menhir_env (Obj.magic _menhir_stack) MenhirState87
        | NULL_QUESTION ->
            _menhir_run44 _menhir_env (Obj.magic _menhir_stack) MenhirState87
        | PAIR ->
            _menhir_run42 _menhir_env (Obj.magic _menhir_stack) MenhirState87
        | PROC ->
            _menhir_run16 _menhir_env (Obj.magic _menhir_stack) MenhirState87
        | SET ->
            _menhir_run13 _menhir_env (Obj.magic _menhir_stack) MenhirState87
        | SETREF ->
            _menhir_run11 _menhir_env (Obj.magic _menhir_stack) MenhirState87
        | TL ->
            _menhir_run9 _menhir_env (Obj.magic _menhir_stack) MenhirState87
        | UNIT ->
            _menhir_run8 _menhir_env (Obj.magic _menhir_stack) MenhirState87
        | UNPAIR ->
            _menhir_run1 _menhir_env (Obj.magic _menhir_stack) MenhirState87
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState87)
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s

and _menhir_run88 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_stack = (_menhir_stack, _menhir_s) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | BEGIN ->
        _menhir_run88 _menhir_env (Obj.magic _menhir_stack) MenhirState88
    | CONS ->
        _menhir_run86 _menhir_env (Obj.magic _menhir_stack) MenhirState88
    | DEBUG ->
        _menhir_run85 _menhir_env (Obj.magic _menhir_stack) MenhirState88
    | DEREF ->
        _menhir_run83 _menhir_env (Obj.magic _menhir_stack) MenhirState88
    | EMPTYLIST ->
        _menhir_run81 _menhir_env (Obj.magic _menhir_stack) MenhirState88
    | EMPTYTREE ->
        _menhir_run79 _menhir_env (Obj.magic _menhir_stack) MenhirState88
    | GETDATA ->
        _menhir_run77 _menhir_env (Obj.magic _menhir_stack) MenhirState88
    | GETLST ->
        _menhir_run75 _menhir_env (Obj.magic _menhir_stack) MenhirState88
    | GETRST ->
        _menhir_run73 _menhir_env (Obj.magic _menhir_stack) MenhirState88
    | HD ->
        _menhir_run71 _menhir_env (Obj.magic _menhir_stack) MenhirState88
    | ID _v ->
        _menhir_run70 _menhir_env (Obj.magic _menhir_stack) MenhirState88 _v
    | IF ->
        _menhir_run69 _menhir_env (Obj.magic _menhir_stack) MenhirState88
    | INT _v ->
        _menhir_run68 _menhir_env (Obj.magic _menhir_stack) MenhirState88 _v
    | ISZERO ->
        _menhir_run66 _menhir_env (Obj.magic _menhir_stack) MenhirState88
    | LET ->
        _menhir_run63 _menhir_env (Obj.magic _menhir_stack) MenhirState88
    | LETREC ->
        _menhir_run54 _menhir_env (Obj.magic _menhir_stack) MenhirState88
    | LPAREN ->
        _menhir_run52 _menhir_env (Obj.magic _menhir_stack) MenhirState88
    | NEWREF ->
        _menhir_run50 _menhir_env (Obj.magic _menhir_stack) MenhirState88
    | NODE ->
        _menhir_run48 _menhir_env (Obj.magic _menhir_stack) MenhirState88
    | NULLT_QUESTION ->
        _menhir_run46 _menhir_env (Obj.magic _menhir_stack) MenhirState88
    | NULL_QUESTION ->
        _menhir_run44 _menhir_env (Obj.magic _menhir_stack) MenhirState88
    | PAIR ->
        _menhir_run42 _menhir_env (Obj.magic _menhir_stack) MenhirState88
    | PROC ->
        _menhir_run16 _menhir_env (Obj.magic _menhir_stack) MenhirState88
    | SET ->
        _menhir_run13 _menhir_env (Obj.magic _menhir_stack) MenhirState88
    | SETREF ->
        _menhir_run11 _menhir_env (Obj.magic _menhir_stack) MenhirState88
    | TL ->
        _menhir_run9 _menhir_env (Obj.magic _menhir_stack) MenhirState88
    | UNIT ->
        _menhir_run8 _menhir_env (Obj.magic _menhir_stack) MenhirState88
    | UNPAIR ->
        _menhir_run1 _menhir_env (Obj.magic _menhir_stack) MenhirState88
    | END ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_s = MenhirState88 in
        let _v : (Ast.expr list) = 
# 142 "/home/nikhil/.opam/system/lib/menhir/standard.mly"
    ( [] )
# 5309 "parser.ml"
         in
        _menhir_goto_loption_separated_nonempty_list_SEMICOLON_expr__ _menhir_env _menhir_stack _menhir_s _v
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState88

and _menhir_discard : _menhir_env -> _menhir_env =
  fun _menhir_env ->
    let lexer = _menhir_env._menhir_lexer in
    let lexbuf = _menhir_env._menhir_lexbuf in
    let _tok = lexer lexbuf in
    {
      _menhir_lexer = lexer;
      _menhir_lexbuf = lexbuf;
      _menhir_token = _tok;
      _menhir_error = false;
    }

and prog : (Lexing.lexbuf -> token) -> Lexing.lexbuf -> (
# 104 "parser.mly"
       (Ast.prog)
# 5332 "parser.ml"
) =
  fun lexer lexbuf ->
    let _menhir_env = let _tok = Obj.magic () in
    {
      _menhir_lexer = lexer;
      _menhir_lexbuf = lexbuf;
      _menhir_token = _tok;
      _menhir_error = false;
    } in
    Obj.magic (let _menhir_stack = ((), _menhir_env._menhir_lexbuf.Lexing.lex_curr_p) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | BEGIN ->
        _menhir_run88 _menhir_env (Obj.magic _menhir_stack) MenhirState0
    | CONS ->
        _menhir_run86 _menhir_env (Obj.magic _menhir_stack) MenhirState0
    | DEBUG ->
        _menhir_run85 _menhir_env (Obj.magic _menhir_stack) MenhirState0
    | DEREF ->
        _menhir_run83 _menhir_env (Obj.magic _menhir_stack) MenhirState0
    | EMPTYLIST ->
        _menhir_run81 _menhir_env (Obj.magic _menhir_stack) MenhirState0
    | EMPTYTREE ->
        _menhir_run79 _menhir_env (Obj.magic _menhir_stack) MenhirState0
    | GETDATA ->
        _menhir_run77 _menhir_env (Obj.magic _menhir_stack) MenhirState0
    | GETLST ->
        _menhir_run75 _menhir_env (Obj.magic _menhir_stack) MenhirState0
    | GETRST ->
        _menhir_run73 _menhir_env (Obj.magic _menhir_stack) MenhirState0
    | HD ->
        _menhir_run71 _menhir_env (Obj.magic _menhir_stack) MenhirState0
    | ID _v ->
        _menhir_run70 _menhir_env (Obj.magic _menhir_stack) MenhirState0 _v
    | IF ->
        _menhir_run69 _menhir_env (Obj.magic _menhir_stack) MenhirState0
    | INT _v ->
        _menhir_run68 _menhir_env (Obj.magic _menhir_stack) MenhirState0 _v
    | ISZERO ->
        _menhir_run66 _menhir_env (Obj.magic _menhir_stack) MenhirState0
    | LET ->
        _menhir_run63 _menhir_env (Obj.magic _menhir_stack) MenhirState0
    | LETREC ->
        _menhir_run54 _menhir_env (Obj.magic _menhir_stack) MenhirState0
    | LPAREN ->
        _menhir_run52 _menhir_env (Obj.magic _menhir_stack) MenhirState0
    | NEWREF ->
        _menhir_run50 _menhir_env (Obj.magic _menhir_stack) MenhirState0
    | NODE ->
        _menhir_run48 _menhir_env (Obj.magic _menhir_stack) MenhirState0
    | NULLT_QUESTION ->
        _menhir_run46 _menhir_env (Obj.magic _menhir_stack) MenhirState0
    | NULL_QUESTION ->
        _menhir_run44 _menhir_env (Obj.magic _menhir_stack) MenhirState0
    | PAIR ->
        _menhir_run42 _menhir_env (Obj.magic _menhir_stack) MenhirState0
    | PROC ->
        _menhir_run16 _menhir_env (Obj.magic _menhir_stack) MenhirState0
    | SET ->
        _menhir_run13 _menhir_env (Obj.magic _menhir_stack) MenhirState0
    | SETREF ->
        _menhir_run11 _menhir_env (Obj.magic _menhir_stack) MenhirState0
    | TL ->
        _menhir_run9 _menhir_env (Obj.magic _menhir_stack) MenhirState0
    | UNIT ->
        _menhir_run8 _menhir_env (Obj.magic _menhir_stack) MenhirState0
    | UNPAIR ->
        _menhir_run1 _menhir_env (Obj.magic _menhir_stack) MenhirState0
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState0)

# 233 "/home/nikhil/.opam/system/lib/menhir/standard.mly"
  

# 5410 "parser.ml"
