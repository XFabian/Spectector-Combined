Contains the test cases for Spectre V5.

We stripped the examples from the repositories (safeside, transientfail) of all library calls that are not necessary for the vulnerability, because of the 
limitations of the translation in Spectector. These library calls were either deleted or inlined.
We replaced functions like ForceRead or cache_encode with a construct of a secretarray and an access to it. This simulates accessing secret data.

# ret2spec_call_ret_disparity
Here we had to change the inline assembly that restores the correct stack frame (UnwindStackAndSlowlyReturnTo) to manually unwinding the stack. The reason is a limitation of the
translation of Spectector.
