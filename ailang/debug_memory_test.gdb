set pagination off
set logging file /tmp/memory_test_crash.log
set logging on

# Break on segfault
catch signal SIGSEGV
commands
    printf "\n=== CRASH DETECTED ===\n"
    printf "RIP: %p\n", $rip
    printf "RAX: %p\n", $rax
    printf "RDI: %p\n", $rdi
    
    printf "\nInstruction:\n"
    x/i $rip
    
    printf "\nStack:\n"
    x/20gx $rsp
    
    printf "\nBacktrace:\n"
    bt 10
    
    printf "\nDisassembly around crash:\n"
    x/20i $rip-40
    
    continue
end

run
quit
