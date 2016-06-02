/* Examples for testing */

 lambda x:Top. x;
  (lambda x:Top. x) (lambda x:Top. x);
 (lambda x:Top->Top. x) (lambda x:Top. x);
 
(lambda r:{x:Top->Top}. r.x r.x) 
  {x=lambda z:Top.z, y=lambda z:Top.z}; 

{x=true, y=false}; 
{x=true, y=false}.x;
{true, false}; 
{true, false}.1; 

lambda x:Bool. x;
(lambda x:Bool->Bool. if x false then true else false) 
  (lambda x:Bool. if x then false else true); 

lambda x:Nat. succ x;
(lambda x:Nat. succ (succ x)) (succ 0); 

/* The official fullsub will allow this term to evaluate */
/* BUT this term does not pass my type checker */
/* since I do not allow type reconstruction */
if true then {x=true,y=false,a=false} else {y=false,x={},b=false};
