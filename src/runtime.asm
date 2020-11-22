.globl _enter_exec
_enter_exec:
    subq $16, %rsp
    movq $0, 0(%rsp) # function_id
    movq $0, 8(%rsp) # prev_frame
    callq *%rdi
    addq $16, %rsp
    retq
