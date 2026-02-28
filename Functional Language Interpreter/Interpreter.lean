abbrev Var := String

inductive Ty : Type where
  | TInt  : Ty
  | TBool : Ty
  | TList : Ty -> Ty
  | TFun  : Ty -> Ty -> Ty
deriving Repr, DecidableEq

open Ty

inductive BinOp : Type where
  | add : BinOp
  | sub : BinOp
  | mul : BinOp
  | le  : BinOp
  | eq  : BinOp
deriving Repr, DecidableEq

open BinOp

inductive Expr : Type where
  | int    : Int -> Expr
  | bool   : Bool -> Expr
  | var    : Var -> Expr
  | binop  : BinOp -> Expr -> Expr -> Expr
  | ite    : Expr -> Expr -> Expr -> Expr
  | letE   : Var -> Expr -> Expr -> Expr
  | funE   : Var -> Ty -> Expr -> Expr
  | app    : Expr -> Expr -> Expr
  | letrec : Var -> Var -> Ty -> Ty -> Expr -> Expr -> Expr
  | nil    : Ty -> Expr
  | cons   : Expr -> Expr -> Expr
  | matchL : Expr -> Expr -> Var -> Var -> Expr -> Expr
deriving Repr, DecidableEq

open Expr

inductive Value : Type where
  | vint    : Int -> Value
  | vbool   : Bool -> Value
  | vnil    : Ty -> Value
  | vcons   : Value -> Value -> Value
  | vclos   : Var -> Ty -> Expr -> List (Var × Value) -> Value
  | vrecclos : Var -> Var -> Ty -> Ty -> Expr -> List (Var × Value) -> Value
deriving Repr

abbrev Env := List (Var × Value)

abbrev TEnv := List (Var × Ty)

def envLookup (ρ : Env) (x : Var) : Option Value :=
  match ρ with
  | [] => none
  | (y,v) :: ρ' => if x = y then some v else envLookup ρ' x

def tenvLookup (Γ : TEnv) (x : Var) : Option Ty :=
  match Γ with
  | [] => none
  | (y,t) :: Γ' => if x = y then some t else tenvLookup Γ' x

def envExtend (ρ : Env) (x : Var) (v : Value) : Env :=
  (x, v) :: ρ

def tenvExtend (Γ : TEnv) (x : Var) (t : Ty) : TEnv :=
  (x, t) :: Γ

-- Errors
inductive RuntimeError : Type where
  | unboundVar : Var -> RuntimeError
  | typeMismatch : String -> RuntimeError
  | notAFunction : RuntimeError
  | badBinop : RuntimeError
  | badMatch : RuntimeError
  | outOfFuel : RuntimeError
deriving Repr

inductive TypeError : Type where
  | unboundVar : Var -> TypeError
  | expected : Ty -> Ty -> TypeError
  | expectedFun : Ty -> TypeError
  | expectedList : Ty -> TypeError
  | badBinop : BinOp -> Ty -> Ty -> TypeError
  | branchMismatch : Ty -> Ty -> TypeError
  | matchMismatch : Ty -> Ty -> TypeError
deriving Repr

notation "IntT" => Ty.TInt
notation "BoolT" => Ty.TBool
notation "ListT[" t "]" => Ty.TList t
infixr:60 " ->' " => Ty.TFun

infixl:55 " +' " => Expr.binop BinOp.add
infixl:55 " -' " => Expr.binop BinOp.sub
infixl:60 " *' " => Expr.binop BinOp.mul
infixl:50 " <=' " => Expr.binop BinOp.le
infixl:50 " ==' " => Expr.binop BinOp.eq

def tyEq (t1 t2 : Ty) : Bool :=
  decide (t1 = t2)

def expectTy (expected got : Ty) : Except TypeError Unit :=
  if expected = got then
    pure ()
  else
    throw (TypeError.expected expected got)

def binopType (op : BinOp) (t1 t2 : Ty) : Except TypeError Ty :=
  match op with
  | BinOp.add | BinOp.sub | BinOp.mul =>
      match t1, t2 with
      | Ty.TInt, Ty.TInt => pure Ty.TInt
      | _, _ => throw (TypeError.badBinop op t1 t2)
  | BinOp.le | BinOp.eq =>
      match t1, t2 with
      | Ty.TInt, Ty.TInt => pure Ty.TBool
      | _, _ => throw (TypeError.badBinop op t1 t2)

def typeOf (Γ : TEnv) : Expr -> Except TypeError Ty
  | Expr.int _ =>
      pure Ty.TInt

  | Expr.bool _ =>
      pure Ty.TBool

  | Expr.var x =>
      match tenvLookup Γ x with
      | some t => pure t
      | none => throw (TypeError.unboundVar x)

  | Expr.binop op e1 e2 => do
      let t1 <- typeOf Γ e1
      let t2 <- typeOf Γ e2
      binopType op t1 t2

  | Expr.ite c t e => do
    let tc <- typeOf Γ c
    expectTy Ty.TBool tc
    let tt <- typeOf Γ t
    let te <- typeOf Γ e
    if tt = te then
      pure tt
    else
      throw (TypeError.branchMismatch tt te)


  | Expr.letE x e1 e2 => do
      let t1 <- typeOf Γ e1
      typeOf (tenvExtend Γ x t1) e2

  | Expr.funE x tx body => do
      let tbody <- typeOf (tenvExtend Γ x tx) body
      pure (Ty.TFun tx tbody)

  | Expr.app f arg => do
      let tf <- typeOf Γ f
      let ta <- typeOf Γ arg
      match tf with
      | Ty.TFun tx tr =>
          expectTy tx ta
          pure tr
      | _ =>
          throw (TypeError.expectedFun tf)

  | Expr.letrec f x tx tr body inE => do
    let tf : Ty := Ty.TFun tx tr
    let Γ1 := tenvExtend Γ f tf
    let Γbody := tenvExtend Γ1 x tx
    let tbody <- typeOf Γbody body
    expectTy tr tbody
    typeOf Γ1 inE

  | Expr.nil tElem =>
      pure (Ty.TList tElem)

  | Expr.cons hd tl => do
      let th <- typeOf Γ hd
      let tt <- typeOf Γ tl
      match tt with
      | Ty.TList tElem =>
          expectTy tElem th
          pure (Ty.TList tElem)
      | _ =>
          throw (TypeError.expectedList tt)

  | Expr.matchL scrut eNil x xs eCons => do
    let ts <- typeOf Γ scrut
    match ts with
    | Ty.TList tElem => do
        let tNil <- typeOf Γ eNil
        let Γ' := tenvExtend (tenvExtend Γ x tElem) xs (Ty.TList tElem)
        let tCons <- typeOf Γ' eCons
        if tNil = tCons then
          pure tNil
        else
          throw (TypeError.matchMismatch tNil tCons)
    | _ =>
        throw (TypeError.expectedList ts)

def Γ0 : TEnv := []

def ex1 : Expr := (Expr.int 3) +' (Expr.int 4)
#eval typeOf Γ0 ex1

def ex2 : Expr := Expr.ite (Expr.bool true) (Expr.int 1) (Expr.int 2)
#eval typeOf Γ0 ex2

def ex3 : Expr :=
  Expr.letE "x" (Expr.int 10)
    (Expr.letE "f" (Expr.funE "y" Ty.TInt (Expr.var "x" +' Expr.var "y"))
      (Expr.letE "x" (Expr.int 20)
        (Expr.app (Expr.var "f") (Expr.int 5))))
#eval typeOf Γ0 ex3


abbrev Fuel := Nat

def valueToString : Value -> String
  | Value.vint n => s!"{n}"
  | Value.vbool b => s!"{b}"
  | Value.vnil _ => "[]"
  | Value.vcons _ _ => "(list)"
  | Value.vclos _ _ _ _ => "<closure>"
  | Value.vrecclos _ _ _ _ _ _ => "<recclosure>"

def isIntVal : Value -> Option Int
  | Value.vint n => some n
  | _ => none

def isBoolVal : Value -> Option Bool
  | Value.vbool b => some b
  | _ => none

def isListVal (v : Value) : Option Value :=
  match v with
  | Value.vnil _ => some v
  | Value.vcons _ _ => some v
  | _ => none


def evalBinOp (op : BinOp) (v1 v2 : Value) : Except RuntimeError Value := do
  let n1? := isIntVal v1
  let n2? := isIntVal v2
  match n1?, n2? with
  | some n1, some n2 =>
      match op with
      | BinOp.add => pure (Value.vint (n1 + n2))
      | BinOp.sub => pure (Value.vint (n1 - n2))
      | BinOp.mul => pure (Value.vint (n1 * n2))
      | BinOp.le  => pure (Value.vbool (decide (n1 <= n2)))
      | BinOp.eq  => pure (Value.vbool (decide (n1 = n2)))
  | _, _ =>
      throw (RuntimeError.badBinop)

-- Main evaluator
def eval : Fuel -> Env -> Expr -> Except RuntimeError Value
  | 0, _, _ => throw RuntimeError.outOfFuel
  | fuel+1, ρ, e =>
    match e with
    | Expr.int n =>
        pure (Value.vint n)

    | Expr.bool b =>
        pure (Value.vbool b)

    | Expr.var x =>
        match envLookup ρ x with
        | some v => pure v
        | none => throw (RuntimeError.unboundVar x)

    | Expr.binop op e1 e2 => do
        let v1 <- eval fuel ρ e1
        let v2 <- eval fuel ρ e2
        evalBinOp op v1 v2

    | Expr.ite c t e => do
        let vc <- eval fuel ρ c
        match isBoolVal vc with
        | some true  => eval fuel ρ t
        | some false => eval fuel ρ e
        | none => throw (RuntimeError.typeMismatch "if condition is not Bool")

    | Expr.letE x e1 e2 => do
        let v1 <- eval fuel ρ e1
        eval fuel (envExtend ρ x v1) e2

    | Expr.funE x tx body =>
        pure (Value.vclos x tx body ρ)

    | Expr.app f arg => do
        let vf <- eval fuel ρ f
        let va <- eval fuel ρ arg
        match vf with
        | Value.vclos x _ body ρ0 =>
            eval fuel (envExtend ρ0 x va) body
        | Value.vrecclos fName x tx tr body ρ0 =>
            let self : Value := Value.vrecclos fName x tx tr body ρ0
            let ρ1 := envExtend (envExtend ρ0 fName self) x va
            eval fuel ρ1 body
        | _ =>
            throw RuntimeError.notAFunction

    | Expr.letrec f x tx tr body inE =>
        let clos : Value := Value.vrecclos f x tx tr body ρ
        eval fuel (envExtend ρ f clos) inE

    | Expr.nil tElem =>
        pure (Value.vnil tElem)

    | Expr.cons hd tl => do
        let vhd <- eval fuel ρ hd
        let vtl <- eval fuel ρ tl
        match vtl with
        | Value.vnil _ =>
            pure (Value.vcons vhd vtl)
        | Value.vcons _ _ =>
            pure (Value.vcons vhd vtl)
        | _ =>
            throw (RuntimeError.typeMismatch "cons expects a list as tail")

    | Expr.matchL scrut eNil x xs eCons => do
        let vs <- eval fuel ρ scrut
        match vs with
        | Value.vnil _ =>
            eval fuel ρ eNil
        | Value.vcons vh vt =>
            let ρ' := envExtend (envExtend ρ x vh) xs vt
            eval fuel ρ' eCons
        | _ =>
            throw (RuntimeError.badMatch)


def fuel0 : Fuel := 200
def ρ0 : Env := []

def ex_eval1 : Expr := (Expr.int 3) +' (Expr.int 4)
#eval eval fuel0 ρ0 ex_eval1

def ex_eval2 : Expr := Expr.ite (Expr.bool true) (Expr.int 1) (Expr.int 2)
#eval eval fuel0 ρ0 ex_eval2

def ex_closure : Expr :=
  Expr.letE "x" (Expr.int 10)
    (Expr.letE "f" (Expr.funE "y" Ty.TInt (Expr.var "x" +' Expr.var "y"))
      (Expr.letE "x" (Expr.int 20)
        (Expr.app (Expr.var "f") (Expr.int 5))))
#eval eval fuel0 ρ0 ex_closure


def valueHasType : Value -> Ty -> Prop
  | Value.vint _,  Ty.TInt => True
  | Value.vbool _, Ty.TBool => True
  | Value.vnil tElem, Ty.TList t => tElem = t
  | Value.vcons h t, Ty.TList te =>
      valueHasType h te ∧ valueHasType t (Ty.TList te)
  | Value.vclos _ tx _ _, Ty.TFun tArg _ =>
      tx = tArg
  | Value.vrecclos _ _ tx tr _ _, Ty.TFun tArg tRes =>
      tx = tArg ∧ tr = tRes
  | _, _ => False

inductive RunResult where
  | ok : Ty -> Value -> RunResult
  | typeErr : TypeError -> RunResult
  | runtimeErr : RuntimeError -> RunResult
deriving Repr


def run (fuel : Fuel) (e : Expr) : RunResult :=
  match typeOf [] e with
  | .error te => RunResult.typeErr te
  | .ok t =>
      match eval fuel [] e with
      | .error re => RunResult.runtimeErr re
      | .ok v => RunResult.ok t v


open Lean

declare_syntax_cat mylang

syntax num : mylang
syntax ident : mylang
syntax "true" : mylang
syntax "false" : mylang
syntax "(" mylang ")" : mylang

syntax:60 mylang " * " mylang : mylang
syntax:50 mylang " + " mylang : mylang
syntax:50 mylang " - " mylang : mylang
syntax:40 mylang " <= " mylang : mylang
syntax:40 mylang " == " mylang : mylang

syntax "if " mylang " then " mylang " else " mylang : mylang
syntax "let " ident " := " mylang " in " mylang : mylang

syntax:max "(" "mylang|" mylang ")" : term

declare_syntax_cat myty

syntax "int"  : myty
syntax "bool" : myty
syntax "(" myty ")" : myty
syntax "list" "[" myty "]" : myty
syntax:25 myty " -> " myty : myty


syntax:max "(" "mylangTy|" myty ")" : term

macro_rules
  | `((mylangTy| ($t:myty))) => `((mylangTy| $t))

  | `((mylangTy| int))  => `(Ty.TInt)
  | `((mylangTy| bool)) => `(Ty.TBool)
  | `((mylangTy| list[$t:myty])) =>
      `(Ty.TList (mylangTy| $t))
  | `((mylangTy| $t1:myty -> $t2:myty)) =>
      `(Ty.TFun (mylangTy| $t1) (mylangTy| $t2))


macro_rules
  -- parenthesis
  | `((mylang| ( $e:mylang ))) =>
      `((mylang| $e))

  -- literals
  | `((mylang| $n:num)) =>
      `(Expr.int (Int.ofNat $n))
  | `((mylang| true)) =>
      `(Expr.bool Bool.true)
  | `((mylang| false)) =>
      `(Expr.bool Bool.false)

  -- variables
  | `((mylang| $x:ident)) =>
      `(Expr.var $(Lean.quote (toString x.getId)))

  -- binary ops
  | `((mylang| $e1:mylang + $e2:mylang)) =>
      `(Expr.binop BinOp.add ((mylang| $e1)) ((mylang| $e2)))
  | `((mylang| $e1:mylang - $e2:mylang)) =>
      `(Expr.binop BinOp.sub ((mylang| $e1)) ((mylang| $e2)))
  | `((mylang| $e1:mylang * $e2:mylang)) =>
      `(Expr.binop BinOp.mul ((mylang| $e1)) ((mylang| $e2)))
  | `((mylang| $e1:mylang <= $e2:mylang)) =>
      `(Expr.binop BinOp.le ((mylang| $e1)) ((mylang| $e2)))
  | `((mylang| $e1:mylang == $e2:mylang)) =>
      `(Expr.binop BinOp.eq ((mylang| $e1)) ((mylang| $e2)))

  -- if
  | `((mylang| if $c:mylang then $t:mylang else $e:mylang)) =>
      `(Expr.ite ((mylang| $c)) ((mylang| $t)) ((mylang| $e)))

  -- let
  | `((mylang| let $x:ident := $v:mylang in $b:mylang)) =>
      `(Expr.letE $(Lean.quote (toString x.getId)) ((mylang| $v)) ((mylang| $b)))


def fuelTest : Fuel := 800

instance : OfNat Expr n where
  ofNat := Expr.int (Int.ofNat n)

def showRun (r : RunResult) : String :=
  match r with
  | RunResult.ok t v =>
      s!"OK : {reprStr t} => {valueToString v}"
  | RunResult.typeErr e =>
      s!"TYPE ERROR : {reprStr e}"
  | RunResult.runtimeErr e =>
      s!"RUNTIME ERROR : {reprStr e}"


def isIdentStart (c : Char) : Bool :=
  c.isAlpha || c = '_'

def isIdentChar (c : Char) : Bool :=
  c.isAlphanum || c = '_'

def surround (s : String) (pat : String) : String :=
  s.replace pat (" " ++ pat ++ " ")

def tokenize (s : String) : List String :=
  let s :=
    s
      |>.replace ":=" "__ASSIGN__"
      |>.replace "<=" "__LE__"
      |>.replace "==" "__EQ__"
      |>.replace "->" "__ARROW__"
      |>.replace "=>" "__FATARROW__"
      |> fun t => surround t "("
      |> fun t => surround t ")"
      |> fun t => surround t "["
      |> fun t => surround t "]"
      |> fun t => surround t ","
      |> fun t => surround t "|"
      |> fun t => surround t "+"
      |> fun t => surround t "*"
      |> fun t => surround t ":"
      |> fun t => surround t "-"
      |>.replace "__ASSIGN__" ":="
      |>.replace "__LE__" "<="
      |>.replace "__EQ__" "=="
      |>.replace "__ARROW__" "->"
      |>.replace "__FATARROW__" "=>"
      |> fun t => surround t ":="
      |> fun t => surround t "<="
      |> fun t => surround t "=="
      |> fun t => surround t "->"
      |> fun t => surround t "=>"

  (s.splitToList (·.isWhitespace)).filter (· ≠ "")

def parseIntTok (tok : String) : Option Int :=
  tok.toInt?

def expectTok (t : String) (ts : List String) : Except String (List String) :=
  match ts with
  | x :: xs =>
      if x = t then
        pure xs
      else
        throw s!"expected '{t}', got '{x}'"
  | [] =>
      throw s!"expected '{t}', got end of input"

def startsAtom : List String -> Bool
  | [] => Bool.false
  | tok :: _ =>
      match tok with
      | ")" | "]" | "," | "|" | "=>" | ":=" | ":" | "then" | "else" | "in" | "with"
      | "+" | "-" | "*" | "<=" | "==" | "->" => Bool.false
      | _ => Bool.true


mutual
  partial def parseTy (ts : List String) : Except String (Ty × List String) :=
    parseTyArrow ts

  partial def parseTyArrow (ts : List String) : Except String (Ty × List String) := do
    let (t1, ts1) <- parseTyAtom ts
    match ts1 with
    | "->" :: rest => do
        let (t2, rest2) <- parseTyArrow rest
        pure (Ty.TFun t1 t2, rest2)
    | _ =>
        pure (t1, ts1)

  partial def parseTyAtom (ts : List String) : Except String (Ty × List String) := do
    match ts with
    | [] =>
        throw "unexpected end of input in type"
    | "int" :: rest =>
        pure (Ty.TInt, rest)
    | "bool" :: rest =>
        pure (Ty.TBool, rest)
    | "list" :: "[" :: rest => do
        let (t, rest1) <- parseTy rest
        let rest2 <- expectTok "]" rest1
        pure (Ty.TList t, rest2)
    | "(" :: rest => do
        let (t, rest1) <- parseTy rest
        let rest2 <- expectTok ")" rest1
        pure (t, rest2)
    | tok :: _ =>
        throw s!"unexpected token in type: '{tok}'"
end

mutual
  partial def parseExpr (ts : List String) : Except String (Expr × List String) :=
    parseCmp ts

  partial def parseCmp (ts : List String) : Except String (Expr × List String) := do
    let (e1, ts1) <- parseAdd ts
    match ts1 with
    | "<=" :: rest => do
        let (e2, rest2) <- parseAdd rest
        pure (Expr.binop BinOp.le e1 e2, rest2)
    | "==" :: rest => do
        let (e2, rest2) <- parseAdd rest
        pure (Expr.binop BinOp.eq e1 e2, rest2)
    | _ =>
        pure (e1, ts1)

  partial def parseAdd (ts : List String) : Except String (Expr × List String) := do
    let (e1, ts1) <- parseMul ts
    let rec loop (acc : Expr) (ts : List String) : Except String (Expr × List String) :=
      match ts with
      | "+" :: rest => do
          let (e2, rest2) <- parseMul rest
          loop (Expr.binop BinOp.add acc e2) rest2
      | "-" :: rest => do
          let (e2, rest2) <- parseMul rest
          loop (Expr.binop BinOp.sub acc e2) rest2
      | _ =>
          pure (acc, ts)
    loop e1 ts1

  partial def parseMul (ts : List String) : Except String (Expr × List String) := do
    let (e1, ts1) <- parseApp ts
    let rec loop (acc : Expr) (ts : List String) : Except String (Expr × List String) :=
      match ts with
      | "*" :: rest => do
          let (e2, rest2) <- parseApp rest
          loop (Expr.binop BinOp.mul acc e2) rest2
      | _ =>
          pure (acc, ts)
    loop e1 ts1

  partial def parseApp (ts : List String) : Except String (Expr × List String) := do
    let (e1, ts1) <- parseAtom ts
    let rec loop (acc : Expr) (ts : List String) : Except String (Expr × List String) :=
      if startsAtom ts then
        do
          let (arg, rest) <- parseAtom ts
          loop (Expr.app acc arg) rest
      else
        pure (acc, ts)
    loop e1 ts1

  partial def parseAtom (ts : List String) : Except String (Expr × List String) := do
    match ts with
    | [] =>
        throw "unexpected end of input"

    | "true" :: rest =>
        pure (Expr.bool Bool.true, rest)

    | "false" :: rest =>
        pure (Expr.bool Bool.false, rest)

    | "if" :: rest => do
        let (c, rest1) <- parseExpr rest
        let rest2 <- expectTok "then" rest1
        let (t, rest3) <- parseExpr rest2
        let rest4 <- expectTok "else" rest3
        let (e, rest5) <- parseExpr rest4
        pure (Expr.ite c t e, rest5)

    | "let" :: x :: ":=" :: rest => do
        let (v, rest1) <- parseExpr rest
        let rest2 <- expectTok "in" rest1
        let (b, rest3) <- parseExpr rest2
        pure (Expr.letE x v b, rest3)

    | "fun" :: x :: ":" :: rest => do
        let (tx, rest1) <- parseTy rest
        let rest2 <- expectTok "=>" rest1
        let (body, rest3) <- parseExpr rest2
        pure (Expr.funE x tx body, rest3)

    | "letrec" :: f :: "(" :: x :: ":" :: rest => do
        let (tx, rest1) <- parseTy rest
        let rest2 <- expectTok ")" rest1
        let rest3 <- expectTok ":" rest2
        let (tr, rest4) <- parseTy rest3
        let rest5 <- expectTok ":=" rest4
        let (body, rest6) <- parseExpr rest5
        let rest7 <- expectTok "in" rest6
        let (inE, rest8) <- parseExpr rest7
        pure (Expr.letrec f x tx tr body inE, rest8)

    | "nil" :: "[" :: rest => do
        let (tElem, rest1) <- parseTy rest
        let rest2 <- expectTok "]" rest1
        pure (Expr.nil tElem, rest2)

    | "cons" :: "(" :: rest => do
        let (hd, rest1) <- parseExpr rest
        let rest2 <- expectTok "," rest1
        let (tl, rest3) <- parseExpr rest2
        let rest4 <- expectTok ")" rest3
        pure (Expr.cons hd tl, rest4)

    | "match" :: rest => do
        let (scrut, rest1) <- parseExpr rest
        let rest2 <- expectTok "with" rest1
        let rest3 <- expectTok "|" rest2
        let rest4 <- expectTok "nil" rest3
        let rest5 <- expectTok "=>" rest4
        let (eNil, rest6) <- parseExpr rest5
        let rest7 <- expectTok "|" rest6
        let rest8 <- expectTok "cons" rest7
        match rest8 with
        | x :: xs :: rest9 => do
            let rest10 <- expectTok "=>" rest9
            let (eCons, rest11) <- parseExpr rest10
            pure (Expr.matchL scrut eNil x xs eCons, rest11)
        | _ =>
            throw "match cons branch expects two variable names"

    | "(" :: rest => do
        let (e, rest1) <- parseExpr rest
        let rest2 <- expectTok ")" rest1
        pure (e, rest2)

    | "-" :: tok :: rest =>
        match parseIntTok ("-" ++ tok) with
        | some n => pure (Expr.int n, rest)
        | none => throw s!"expected number after '-', got '{tok}'"

    | tok :: rest =>
        match parseIntTok tok with
        | some n => pure (Expr.int n, rest)
        | none => pure (Expr.var tok, rest)
end

def parse (s : String) : Except String Expr := do
  let toks := tokenize s
  let (e, rest) <- parseExpr toks
  match rest with
  | [] => pure e
  | xs => throw s!"unconsumed tokens: {xs}"

def runString (fuel : Fuel) (s : String) : String :=
  match parse s with
  | .error msg => s!"PARSE ERROR: {msg}"
  | .ok e => showRun (run fuel e)


#eval runString fuelTest "2 + 3 * 4"
#eval runString fuelTest "if 1 <= 2 then 10 else 20"
#eval runString fuelTest "let x := 5 in x + 1"
#eval runString fuelTest "(fun x : int => x + 1) 5"
#eval runString fuelTest "let x := 10 in let f := fun y : int => x + y in let x := 20 in f 5"
#eval runString fuelTest "letrec fact (n : int) : int := if n <= 0 then 1 else n * fact (n - 1) in fact 5"
#eval runString fuelTest "match cons(1, cons(2, nil[int])) with | nil => 0 | cons h t => h"


def t_arith1 : Expr :=
  2 +' 3

#eval showRun (run fuelTest t_arith1)

def t_arith2 : Expr :=
  10 *' 4

#eval showRun (run fuelTest t_arith2)

def t_if1 : Expr :=
  Expr.ite (Expr.bool Bool.true) 1 2

#eval showRun (run fuelTest t_if1)

def t_if2 : Expr :=
  Expr.ite (Expr.bool Bool.false) 1 2

#eval showRun (run fuelTest t_if2)


def t_let1 : Expr :=
  Expr.letE "x" 5 ((Expr.var "x") +' 1)

#eval showRun (run fuelTest t_let1)

def t_closure1 : Expr :=
  Expr.letE "x" 10
    (Expr.letE "f"
      (Expr.funE "y" Ty.TInt ((Expr.var "x") +' (Expr.var "y")))
      (Expr.letE "x" 20
        (Expr.app (Expr.var "f") 5)))

#eval showRun (run fuelTest t_closure1)

def t_fact : Expr :=
  Expr.letrec "fact" "n" Ty.TInt Ty.TInt
    (Expr.ite
      ((Expr.var "n") <=' 0)
      1
      ((Expr.var "n")
        *' (Expr.app (Expr.var "fact")
              ((Expr.var "n") -' 1))))
    (Expr.app (Expr.var "fact") 5)

#eval showRun (run fuelTest t_fact)

def t_list1 : Expr :=
  Expr.matchL
    (Expr.cons 1 (Expr.cons 2 (Expr.nil Ty.TInt)))
    0
    "x" "xs"
    (Expr.var "x")

#eval showRun (run fuelTest t_list1)

def t_type_err1 : Expr :=
  Expr.ite 0 1 2

#eval showRun (run fuelTest t_type_err1)

def t_type_err2 : Expr :=
  Expr.app 3 4

#eval showRun (run fuelTest t_type_err2)
