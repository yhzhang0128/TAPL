/* Sample Tests for Simply Typed Lambda Calculus */

lambda x:Nat. succ (succ x);

if iszero 0 then 
  pred ((lambda x:Nat. x) 25) 
else 
  12;
  /* lambda x:Nat.x; */
  /* if use this term, type of 2 branches will mismatch */

(lambda x:Nat. 
  lambda y:Nat. 
    if iszero y then 
      (succ (succ x)) 
    else 
      (pred (pred x))
) 12 1;


(lambda x:Bool->Bool. if x false then true else false) 
(lambda x:Bool. if x then false else true); 


if iszero 255 then lambda x:Nat. x else false;

/*
Result should be:

(lambda x. (succ (succ x))) : Nat -> Nat
 24    : Nat
 10     : Nat
true : Bool
/home/yhzhang/School/TAPL/project/test-fuse.f:25.1:
type of two branches in conditional mismatch

*/
