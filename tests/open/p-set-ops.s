main {
    set<int> a := {0, 1, 2};
    set<int> b := {-2, -1, 0};
    set<int> c := {-2, 0, 2};

    print (a & b) | (b & c);
    print (a | b) \ (b | c);
};