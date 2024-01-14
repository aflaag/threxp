use "exprifier.sml";

val output1 = exprify "10 - (11 * (123 - 52 / 1 + 25) / (12 - (12 + (12)) * 5))";
val expected1 = Sub (Int 10, Div (Mul (Int 11,Add (Sub (Int 123,Div (Int 52,Int 1)),Int 25)), Sub (Int 12,Mul (Add (Int 12,Int 12),Int 5))));

val output2 = exprify "1 - (2 * (1 + 3 * 4 + 5))";
val expected2 = Sub (Int 1,Mul (Int 2,Add (Add (Int 1,Mul (Int 3,Int 4)),Int 5)));

val output3 = exprify "(12 + var) * (4 - 9)";
val expected3 = Mul (Add (Int 12,Id "var"),Sub (Int 4,Int 9));

val output4 = exprify "3 + 4 * (12 + 5)";
val expected4 = Add (Int 3, Mul (Int 4, Add(Int 12, Int 5)));

val output5 = exprify "7 + (4 * (5 + 6))";
val expected5 = Add (Int 7, Mul (Int 4, Add(Int 5, Int 6)));

val output6 = exprify "1 + 3 * test + 5";
val expected6 = Add (Add (Int 1,Mul (Int 3,Id "test")),Int 5);

val output7 = exprify "5 + (x + (ciao + 5))";
val expected7 = Add (Int 5, Add (Id "x", Add(Id "ciao", Int 5)));

val output8 = exprify "$ x = ($ y = 3 @ 2 + 3 ! + 3) @ x !";
val expected8 = Let ("x",Add (Let ("y",Int 3,Add (Int 2,Int 3)),Int 3),Id "x");

val output9 = exprify "$ x = $ x = 10 @ x + x ! + $ x =  10 @ x + x ! @ x !";
val expected9 = Let ("x", Add (Let("x", Int 10, Add(Id "x", Id "x")), Let("x", Int 10, Add(Id "x", Id "x"))), Id "x");

val output10 = exprify "2 + $ x = 2 * 3 @ x / 3 ! * 4";
val expected10 = Add(Int 2, Mul (Let("x", Mul (Int 2, Int 3), Div (Id "x", Int 3)), Int 4));

val output11 = exprify "$ x = 5 @ x !";
val expected11 = Let("x", Int 5, Id "x");

val output12 = exprify "$ x = 5 @ ($ y = 3 @ x + y!)!";
val expected12 = Let("x", Int 5, Let("y", Int 3, Add( Id "x", Id "y")));

val output13 = exprify "$ x = $ y = 3 @ 2 + 3! @ x!";
val expected13 = Let("x", Let("y", Int 3, Add(Int 2, Int 3)), Id "x");

val check1 = output1 = expected1;
val check2 = output2 = expected2;
val check3 = output3 = expected3;
val check4 = output4 = expected4;
val check5 = output5 = expected5;
val check6 = output6 = expected6;
val check7 = output7 = expected7;
val check8 = output8 = expected8;
val check9 = output9 = expected9;
val check10 = output10 = expected10;
val check11 = output11 = expected11;
val check12 = output12 = expected12;
val check13 = output13 = expected13;

val all_passed = check1 andalso
               check2 andalso
               check3 andalso
               check4 andalso
               check5 andalso
               check6 andalso
               check7 andalso
               check8 andalso
               check9 andalso
               check10 andalso
               check11 andalso
               check12 andalso
               check13;
