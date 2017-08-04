define i32 @main() {
       %ptr = alloca i32
       store i32 0, i32* %ptr
       %val = load atomic i32, i32* %ptr seq_cst, align 4
       ret i32 %val
}
