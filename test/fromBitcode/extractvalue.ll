
define i32 @main() {
    %ptr = alloca { i32, i1 }
    store { i32, i1 } { i32 1, i1 1 }, { i32, i1 }* %ptr
    %val = load { i32, i1 }, { i32, i1 }* %ptr

    %ret = extractvalue { i32, i1 } %val, 0 

    ret i32 %ret
}
