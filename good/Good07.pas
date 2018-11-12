program Good07;

var i : integer;

begin

  i := readInt() ; (* 42 *)

  writeInt(i) ;   (* 42 *)

  writeInt(i) ; (* 42 *)

  i := i + 1;
   
  writeInt(i) ;  (* 43 *)

  i := i + 1;
   
  writeInt(i) ;  (* 44 *)
   
  writeInt(i) ;   (* 44 *)

end.
