fdef int triple (int a) {
    fdef int double (int b) {
        return b * 2;
    };

    return a + double(a);
};

main {
    int n := 2;
    print triple(n);    
};
