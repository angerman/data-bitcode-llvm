
declare void @llvm.memset.i32(i8*, i8, i32, i32)
; declare void @llvm.memset.p0i8.i64(i8*, i8, i64, i32)

define i32 @main() {
       %ptr = alloca i8, i8 16, align 4
       call void @llvm.memset.i32(i8* %ptr, i8 0, i32 16, i32 4)
       ret i32 0
}
