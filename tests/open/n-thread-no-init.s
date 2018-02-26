main {
    thread t1;
    thread t2;
    t1 := {print "Hello from thread 1!";};
    t2 := {print "Hello from thread 2!";};
    wait(t1); 
    wait(t2);
    print "Multithreaded program ends.";
};