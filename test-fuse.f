
if iszero 0 then 
  pred ((lambda x. x) 25) 
else 
  false;


(lambda x. 
  lambda y. 
    if iszero y then 
      (succ (succ x)) 
    else 
      (pred (pred y))
) 12 0;
