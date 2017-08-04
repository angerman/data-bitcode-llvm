define i32 @main() {
       fence acquire
       fence syncscope("singlethread") seq_cst
       ret i32 0
}
