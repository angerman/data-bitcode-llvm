define i32 @main() {
       %ptr = alloca i32
       store i32 0, i32* %ptr
       
       %old = atomicrmw add i32* %ptr, i32 1 acquire

       ret i32 %old
}
