Each of the folders litmus_stl and litmus_stl_barrier contain an executable script that executes
the STL test cases.

If the cases were manually recompiled, change case2.s with case2_working_offset.s in litmus_stl/ and
case2_working_barrier.s in litmus_stl_barrier/.
This has to be done due to to incompability reasons of 32 bit mode as described in the paper.