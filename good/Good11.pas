program Good11; (* and y or cortocicuitados *)

var b : bool;
   j  :  integer;

begin

   b := true or writeIntBool(1) ; (* no imprime 1 *)

   b := false or writeIntBool(2) ; (* imprime 2  *)

   b := true and writeIntBool(3) ; (* imprime 3 *)

   b := false and writeIntBool(4) ; (* no imprime 4 *) 

end.
