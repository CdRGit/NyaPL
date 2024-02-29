integer literals have a problem
well, integer math has a problem

```
let a: u8 = 255;
let b: u16 = a + a;
```

here it's obvious what type b is, and by extension what value it is, 510 as a u16

the current plan is to have literals be a ux, where x is the first value where their value < 2^x

however, what are the types at all points in this expression

```
let a = (1 + 1) + (6 + 10);
```

```
a : ??
(
    1 : u1
    + : ??
    1 : u1
)
+ : ??
(
    6  : u3
    +  : ??
    10 : u4
)
```

proposal, the type is a metatype with a specific constraint, `int_constraint(value_bits, signed)`

```
a : M1 : int_constraint(4, false)
(
    1 : u1
    + : M0 : int_constraint(1, false)
    1 : u1
)
+ : M1 : int_constraint(4, false)
(
    6  : u3
    +  : M1 : int_constraint(4, false)
    10 : u4
)
```

because M0 was added to a value of M1 in here, it will also be the same as M1

```
a : M1 : int_constraint(4, false)
(
    1 : u1
    + : M0 = M1
    1 : u1
)
+ : M1 : int_constraint(4, false)
(
    6  : u3
    +  : M1 : int_constraint(4, false)
    10 : u4
)
```

now we just need a's type to be reified at some point in the future (into a type u4/i5 or bigger), and we should be fine with typechecking

the same goes for prefix `-`

```
let a = -10;
```

```
a  : ??

-  : ??
10 : u4
```

```
a  : M0 : int_constraint(4, true)

-  : M0 : int_constraint(4, true)
10 : u4
```

now a's type can only be reified into i5 or bigger, as a sign is required
