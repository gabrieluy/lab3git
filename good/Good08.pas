program Good08;
(* read numbers until 0 is read, and print their average *)

var sum, num, x : integer;

begin

   sum := 0 ;
   num := 0 ;

   repeat begin
      x   := readInt() ;
      if x <> 0 then begin
	 sum := sum + x ;
	 num := num + 1 ;
      end else ;
   end
   until x = 0;
   
  writeInt(sum div num) ;

end.
