
echo "--------------------------------------------- A jump table ---------------------------------------------------"
spectector "jump_table.s" $SOLVER  --steps 1000000 -w 200 --keep-sym [secretarray] -e [main] -v 2 --skip-uns --parse-uns | grep 'data leak\|program is'
echo "--------------------------------------------- with barrier ---------------------------------------------------"
spectector "jump_table_barrier.s" $SOLVER  --steps 1000000 -w 200 --keep-sym [secretarray] -e [main] -v 2 --skip-uns --parse-uns | grep 'data leak\|program is'
echo "--------------------------------------------- with retpoline --------------------------------------------------"
spectector "retpoline_defense/jump_table.s" $SOLVER  --steps 1000000 -w 200 --keep-sym [secretarray] -e [main] -v 2 --skip-uns --parse-uns | grep 'data leak\|program is'

echo "-------------------------------------------- Different function Pointer example -----------------------------"
spectector "func_table.s" $SOLVER  --steps 1000000 -w 200 --keep-sym [secretarray] -e [main] -v 2 --skip-uns --parse-uns | grep 'data leak\|program is'
echo "-------------------------------------------- with barrier -----------------------------"
spectector "func_table_barrier.s" $SOLVER  --steps 1000000 -w 200 --keep-sym [secretarray] -e [main] -v 2 --skip-uns --parse-uns | grep 'data leak\|program is'
echo "--------------------------------------------- with retpoline ------------------------------------------------"
spectector "retpoline_defense/func_table.s" $SOLVER  --steps 1000000 -w 200 --keep-sym [secretarray] -e [main] -v 2 --skip-uns --parse-uns | grep 'data leak\|program is'

echo "----------------------------------- simple muasm example ----------------------------------------"
spectector "examplev2.muasm" $SOLVER -s --steps 1000000 -w 200 -v 2 -e [main]  | grep 'data leak\|program is'
echo "----------------------------------- simple muasm with barrier -----------------------------------"
spectector "examplev2_barrier.muasm" $SOLVER -s --steps 1000000 -w 200 -v 2 -e [main]  | grep 'data leak\|program is'
