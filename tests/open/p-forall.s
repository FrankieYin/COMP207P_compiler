main { 
    seq<int> a := [1, 2, 3];
    int n;
    forall (n in a) do
        print n * 2;
    od
} ;