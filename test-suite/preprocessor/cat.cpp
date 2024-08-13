// %run ./c9 -E {}
#define INTERNAL_CAT(a, b) INTERNAL_PRIMITIVE_CAT(a, b)
#define INTERNAL_PRIMITIVE_CAT(a, b) a ## b

#define EMPTY()


#define CAT_1(a, b) PRIMITIVE_CAT_1(a, b)
#define CAT_1_ID() CAT_1
#define PRIMITIVE_CAT_1(a, b) a ## b

#define CAT_2(a, b) PRIMITIVE_CAT_2(a, b)
#define CAT_2_ID() CAT_2
#define PRIMITIVE_CAT_2(a, b) a ## b

#define CAT_3(a, b) PRIMITIVE_CAT_3(a, b)
#define CAT_3_ID() CAT_3
#define PRIMITIVE_CAT_3(a, b) a ## b

#define CAT_4(a, b) PRIMITIVE_CAT_4(a, b)
#define CAT_4_ID() CAT_4
#define PRIMITIVE_CAT_4(a, b) a ## b

#define CAT TRY_1()

#define TRY_1() INTERNAL_CAT(TRY_1_, CAT_1(0, 0))()
#define TRY_2() INTERNAL_CAT(TRY_2_, CAT_2(0, 0))()
#define TRY_3() INTERNAL_CAT(TRY_3_, CAT_3(0, 0))()
#define TRY_4() INTERNAL_CAT(TRY_4_, CAT_4(0, 0))()

#define TRY_1_00 CAT_1_ID
#define TRY_2_00 CAT_2_ID
#define TRY_3_00 CAT_3_ID
#define TRY_4_00 CAT_4_ID

#define TRY_1_CAT_1(a, b) TRY_2
#define TRY_2_CAT_2(a, b) TRY_3
#define TRY_3_CAT_3(a, b) TRY_4
#define TRY_4_CAT_4(a, b) error: ran out of CAT depth! EMPTY


CAT(C, AT(C, AT(1, 2))) // %next 12

