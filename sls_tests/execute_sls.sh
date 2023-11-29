
echo "----------------------------------- Leaky Main compiled from C -----------------------------"
spectector "leaky_main.s" $SOLVER -s --steps 1000000 -w 200  --keep-sym [secretarray] -v 6 -e [main] --parse-uns --skip-uns  | grep 'data leak\|program is'
echo "----------------------------------- with barrier -------------------------------------------"
spectector "leaky_main_barrier.s" $SOLVER -s --steps 1000000 -w 200  --keep-sym [secretarray] -v 6 -e [main] --parse-uns --skip-uns | grep 'data leak\|program is'



echo "----------------------------------- Leaky Functions compiled from C -------------------------"
spectector "leaky_functions.s" $SOLVER -s --steps 1000000 -w 200  --keep-sym [secretarray] -v 6 -e [main] --parse-uns --skip-uns | grep 'data leak\|program is'
echo "----------------------------------- with barrier --------------------------------------------"
spectector "leaky_functions_barrier.s" $SOLVER -s --steps 1000000 -w 200  --keep-sym [secretarray] -v 6 -e [main] --parse-uns --skip-uns | grep 'data leak\|program is'

echo "----------------------------------- simple muasm example ----------------------------------------"
spectector "example_sls.muasm" $SOLVER -s --steps 1000000 -w 200 -v 6  | grep 'data leak\|program is'

echo "----------------------------------- simple muasm with barrier -----------------------------------"
spectector "example_sls_barrier.muasm" $SOLVER -s --steps 1000000 -w 200 -v 6  | grep 'data leak\|program is'
