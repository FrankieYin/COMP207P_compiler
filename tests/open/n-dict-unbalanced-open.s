dict<int,string> d1 := (1:"a", 2:"b", 3:"c", 4:"d", 5:"e");
dict<int,string> d2 := (1:"a",2:"b",3:"c",4:"d",5:"e");
dict<top,char> d3 := (1:'a', 2:'b', 3.0:'c', [1,2]:'d'); # character tuple
dict<char,top> d4 := ('a':(1:1,2:2,3:3), 'b':("x":4,"y":5,"z":6); # dict of dict

main {
  int a := 5;
  dict<int,string> d := d1 :: d2;
  i := d[4];
  return;  
};

  
