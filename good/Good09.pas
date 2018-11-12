program Good09;

var n, i, j : integer;
   iPrime : bool;

begin
   n := readInt();
   i := 2;

   while i <= n do begin
      iPrime := true;
      j := 2;
      while (j*j <= i) and iPrime do begin
	 if (i div j) * j = i then
	    iPrime := false
	 else
	    j := j + 1;
      end;

      if iPrime and ((n div i) * i = n) then begin
	 writeInt(i);
	 n := n div i;
      end else
	 i := i + 1; 
   end;
   
end.
