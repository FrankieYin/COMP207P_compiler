main {
    function mul := |a| -> |b| -> { return a * b; };
    int x := 3;
    int y := 4;
    int z := mul(x, y);
    print z;
};