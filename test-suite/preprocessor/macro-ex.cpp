// %run ./c9 -E {}
#define debug(...) fprintf(stderr, __VA_ARGS__)
#define showlist(...) puts(#__VA_ARGS__)
#define report(test, ...) ((test) ? puts(#test) : printf(__VA_ARGS__))

// %next fprintf(stderr, "Flag");
// %next fprintf(stderr, "X = %d\n", x);
// %next puts("The first, second, and third items.");
// %next ((x>y) ? puts("x>y") : printf("x is %d but y is %d", x, y));
debug("Flag");
debug("X = %d\n", x);
showlist(The first, second, and third items.);
report(x>y, "x is %d but y is %d", x, y);


