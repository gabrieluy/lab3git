program Good05;

var lo, hi, mx : integer;

begin

  lo := 1 ;
  hi := lo ;


  mx := readInt () ;

  writeInt(lo) ;

  while hi < mx do
  begin
    writeInt(hi) ;
    hi := lo + hi ;
    lo := hi - lo
  end

end.
