We sometimes added the construct
load f, secret % set up a secret
store f, 1
spbarr

to some test cases. This is doen to set up a secret for Spectector and does not influence the vulnerability. For Spectector all memory regions are high values and all registers are low.
The problem is that we cannot specify the value secret to be high. There is no option for that. So we add this snippet which means that we have a high symbolic value stored at location 1.

Use the script exec_new_comb.sh to see all the combinations in action. For each of the combinations, we created an 
example showing the additional strength.
