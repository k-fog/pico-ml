open Pico

let assert_equal expected actual =
  if expected <> actual then
    failwith
      (Printf.sprintf "Expected %s, got %s"
         (match expected with
         | VInt n -> string_of_int n
         | VBool b -> string_of_bool b
         | VClosure _ -> "<closure>")
         (match actual with
         | VInt n -> string_of_int n
         | VBool b -> string_of_bool b
         | VClosure _ -> "<closure>"))

let test_int () =
  assert_equal (VInt 42) (eval [] (Int 42));
  assert_equal (VInt 0) (eval [] (Int 0));
  assert_equal (VInt (-10)) (eval [] (Int (-10)));
  print_endline "[OK] Int tests passed"

let test_bool () =
  assert_equal (VBool true) (eval [] (Bool true));
  assert_equal (VBool false) (eval [] (Bool false));
  print_endline "[OK] Bool tests passed"

let test_binop () =
  assert_equal (VInt 5) (eval [] (BinOp (Add, Int 2, Int 3)));
  assert_equal (VInt 1) (eval [] (BinOp (Sub, Int 5, Int 4)));
  assert_equal (VInt 6) (eval [] (BinOp (Mul, Int 2, Int 3)));
  assert_equal (VBool true) (eval [] (BinOp (Less, Int 2, Int 3)));
  assert_equal (VBool false) (eval [] (BinOp (Less, Int 5, Int 3)));
  print_endline "[OK] BinOp tests passed"

let test_if () =
  assert_equal (VInt 1) (eval [] (If (Bool true, Int 1, Int 2)));
  assert_equal (VInt 2) (eval [] (If (Bool false, Int 1, Int 2)));
  assert_equal (VInt 10)
    (eval [] (If (BinOp (Less, Int 1, Int 2), Int 10, Int 20)));
  print_endline "[OK] If tests passed"

let test_let () =
  assert_equal (VInt 5)
    (eval [] (Let ("x", Int 3, BinOp (Add, Var "x", Int 2))));
  assert_equal (VInt 6)
    (eval []
       (Let ("x", Int 2, Let ("y", Int 3, BinOp (Mul, Var "x", Var "y")))));
  print_endline "[OK] Let tests passed"

let test_fun () =
  let f = Fun ("x", BinOp (Add, Var "x", Int 1)) in
  let app = App (f, Int 5) in
  assert_equal (VInt 6) (eval [] app);
  let add = Fun ("x", Fun ("y", BinOp (Add, Var "x", Var "y"))) in
  let app1 = App (add, Int 2) in
  let app2 = App (app1, Int 3) in
  assert_equal (VInt 5) (eval [] app2);
  print_endline "[OK] Fun/App tests passed"

let () =
  test_int ();
  test_bool ();
  test_binop ();
  test_if ();
  test_let ();
  test_fun ();
  print_endline "\nAll tests passed!"
