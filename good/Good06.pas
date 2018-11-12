program Good06;

var x, d : integer;

begin

   x := readInt () ; (* 42 *)

   d := x div 2 ;

  while (d > 1) do
  begin
     if d * (x div d) = x then
	writeInt(d) 
    else
       ;

       d := d - 1;
  end;

end.
