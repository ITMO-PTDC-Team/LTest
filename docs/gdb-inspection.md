# GDB inspection

Use a Debug build and Python-enabled GDB on x86-64 Linux.
Before running the target in GDB:

```gdb
source scripts/ltest_gdb.py
set follow-fork-mode child
```

Run the program and stop at a breakpoint. Then list the logical threads,
select one, and inspect its stack and local variables:

```gdb
ltest threads
ltest thread 0
bt
info locals
```

Use `frame N` and `p expression` to inspect another frame or variable.
`thread apply all bt` shows all stacks. Stepping uses native threads.
