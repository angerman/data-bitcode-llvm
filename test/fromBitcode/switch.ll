define i32 @main() {
entry:
       %ptr = alloca i32
       store i32 0, i32* %ptr
       %val = load i32, i32* %ptr

       switch i32 %val, label %otherwise [ i32 0, label %onzero
                                           i32 1, label %onone
                                           i32 2, label %ontwo ]


otherwise:
       ret i32 -1
onzero:
       ret i32 0
onone:
       ret i32 1
ontwo:
       ret i32 2
}
