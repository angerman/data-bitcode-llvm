
define i32 @main() {
    %ptr = alloca i32
    %ptr2 = alloca i32
    store i32 1, i32* %ptr
    store i32 2, i32* %ptr2
    %val = load i32, i32* %ptr
    %val2 = load i32, i32* %ptr2
    %squared = mul i32 %val2, %val2

    cmpxchg i32* %ptr, i32 %val, i32 %squared acq_rel monotonic

    %ret = load i32, i32* %ptr
    ret i32 %ret
}
