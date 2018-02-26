string s1 := "Alice in Wonderland";
string s2 := "Gilgamesh";
string s3 := "One Thousand and One Nights";

fdef int alice () {
  return 5;
} ;

main {
  string key := "ic";  
  seq<string> books := [s1,s2,s3];

  bool found := F;
  int i := 0;
  string tmp;

  while (i < len(books)) do 
     tmp := books[i];
     if (key in tmp) then found := T; fi
     i := i + 1;
  od

  return;
};
