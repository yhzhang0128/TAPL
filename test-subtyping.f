{x = true, y = false}.x;

(lambda r:{x:Top->Top}. r.x)
{x = true, y = false};

(lambda r:{x:Top->Top}. r.x r.x)
  {x=lambda z:Top.z, y=lambda z:Top.z}; 
