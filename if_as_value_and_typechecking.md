let's say we have a value decided by an if statement

```
let val = if condition() {
    foo
} else {
    bar
};
```

val has an unnamed (deduced) type

let's say foo is a u7, and bar is a u10
val would need to be *at least* a u10

```
val : M0
foo : u7
bar : u10
```

in this case both of these impose the same kind of constraint, an `int_constraint`

```
val : M0 : int_constraint(7, false) & int_constraint(10, false)
foo : u7
bar : u10
```

because this is the same kind of constraint twice they collapse into a combination of the two
```
val : M0 : int_constraint(10, false)
foo : u7
bar : u10
```

so M0 needs to be reified to a u10 or larger

if foo was an i7 instead it'd be

```
val : M0 : int_constraint(7, true) & int_constraint(10, false) -> int_constraint(10, true)
foo : i7
bar : u10
```

if foo was instead a struct that can't be implicitly converted into other types it'd cause a contradiction

```
val : M0 : same_as(foo_struct) & int_constraint(10, false) -> !!error, contradiction!!
foo : foo_struct
bar : u10
```
