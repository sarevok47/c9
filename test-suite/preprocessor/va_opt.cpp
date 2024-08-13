// %run ./c9 -E {}
#define F(...)           f(0 __VA_OPT__(,) __VA_ARGS__)
#define G(X, ...)        f(0, X __VA_OPT__(,) __VA_ARGS__)
#define SDEF(sname, ...) S sname __VA_OPT__(= { __VA_ARGS__ })
#define EMP

F(a, b, c)          // %next f(0 , a, b, c)
F()                 // %next f(0)
F(EMP)              // %next f(0)

G(a, b, c)          // %next f(0, a , b, c)
G(a, )              // %next f(0, a)
G(a)                // %next f(0, a)

SDEF(foo);          // %next S foo;
SDEF(bar, 1, 2);    // %next S bar = { 1, 2 };

/*
#define H1(X, ...) X __VA_OPT__(##) __VA_ARGS__ // error: ## may not appear at
                                                // the beginning of a replacement list ([cpp.concat])
*/

#define H2(X, Y, ...) __VA_OPT__(X ## Y,) __VA_ARGS__
H2(a, b, c, d)      // %next ab, c, d

#define H3(X, ...) #__VA_OPT__(X##X X##X)
H3(, 0)             // %next ""

#define H4(X, ...) __VA_OPT__(a X ## X) ## b
H4(, 1)             // %next a b

#define H5A(...) __VA_OPT__()/**/__VA_OPT__()
#define H5B(X) a ## X ## b
#define H5C(X) H5B(X)
H5C(H5A())          // %next ab
