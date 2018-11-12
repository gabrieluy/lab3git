program Good13; (* pruebas de conversion automatica *)

var x : integer;
   y  :  real;

begin 
   x := 6 ;

   y := x + 7 ; (* suma entera y conversion en la asignacion *)

   writeReal(y) ; (* 13.0 *)

   y := x + 7.3 ; (* suma real y conversion en la variable x *)

   
   writeReal(y); (* 13.3 *)

   if y < x then (* comparacion real y conversion en la variable x *)
      writeStr("mal")
   else
      writeStr("bien");

   writeReal(x); (* conversion en el pasaje de parametros *)

end.
