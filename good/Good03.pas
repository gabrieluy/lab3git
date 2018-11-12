program Good03;

var arg, ret, i : integer;

begin

   arg := readInt() ;
   ret := 1 ;

   i := 1 ;

   while i < arg + 1 do
   begin
      ret := i * ret ;
      i := i + 1 
   end;

   writeInt(ret)

end.
