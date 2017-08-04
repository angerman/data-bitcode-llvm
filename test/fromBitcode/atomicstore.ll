define i32 @main() {
       %ptr = alloca i32
       store atomic i32 0, i32* %ptr seq_cst, align 8
       %val = load i32, i32* %ptr
       ret i32 %val
}
