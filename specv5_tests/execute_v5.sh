#!/bin/bash
rm -r stats/*

echo "-------------- Executing safeside example ret2spec_call_ret --------------------"
spectector "ret2spec_test/ret2spec_call_ret.s" $SOLVER -s -a noninter --steps 1000000  -c 'c([],[])' --version 5 -w 200 --skip-uns --parse-uns --keep-sym [secretarray] -e [main] --stats "stats/ret2spec_call_ret.txt" | grep 'data leak\|program is'

echo "---- Now with barrier  defense-----"
spectector "ret2spec_test/ret2spec_call_ret_barr.s" $SOLVER -s -a noninter --steps 1000000  -c 'c([],[])' --version 5 -w 200 --skip-uns --parse-uns --keep-sym [secretarray] -e [main] --stats "stats/barrier_ret2spec_call_ret.txt" | grep 'data leak\|program is'

echo "---- Now with retpoline defense-----"
spectector "ret2spec_test/ret2spec_call_ret_retpoline.s" $SOLVER -s -a noninter --steps 1000000  -c 'c([],[])' --version 5 -w 200 --skip-uns --parse-uns --keep-sym [secretarray] -e [main]  --stats "stats/retpo_ret2spec_call_ret.txt" | grep 'data leak\|program is'


echo "-------------- Executing transfail examples --------------------"

echo "-------------- Executing ca_ip --------------------"
spectector "transfail/ca_ip.s" $SOLVER -s -a noninter --steps 1000000  -c 'c([],[])' --version 5 -w 200  --skip-uns --parse-uns  -e [main]  --keep-sym [real_secret] --stats "stats/ca_ip.txt"  | grep 'data leak\|program is'


echo "-------------- Executing ca_oop --------------------"
spectector "transfail/ca_oop.s" $SOLVER -s -a noninter --steps 1000000  -c 'c([],[])' --version 5 -w 200 --skip-uns --parse-uns -e [main]  --keep-sym [secret]  --stats "stats/ca_oop.txt" | grep 'data leak\|program is'

echo "-------------- Executing sa_ip --------------------"
spectector "transfail/sa_ip.s" $SOLVER -s -a noninter --steps 1000000  -c 'c([],[])' --version 5 -w 200 --skip-uns --parse-uns  -e [main]  --keep-sym [real_secret]  --stats "stats/sa_ip.txt" | grep 'data leak\|program is'


echo "-------------- Executing sa_oop --------------------"
spectector "transfail/sa_oop_min.s" $SOLVER -s -a noninter --steps 1000000  -c 'c([],[])' --version 5 -w 200 --skip-uns --parse-uns --keep-sym [secretarray] -e [call_start] --stats "stats/sa_oop_min.txt" | grep 'data leak\|program is'

echo "---- Now with barrier  defense-----"

echo "-------------- Executing ca_ip_barr --------------------"
spectector "transfail/ca_ip_barr.s" $SOLVER -s -a noninter --steps 1000000  -c 'c([],[])' --version 5 -w 200  --skip-uns --parse-uns  -e [main]  --keep-sym [real_secret] --stats "stats/barrier_ca_ip.txt" | grep 'data leak\|program is'


echo "-------------- Executing ca_oop_barr --------------------"
spectector "transfail/ca_oop_barr.s" $SOLVER -s -a noninter --steps 1000000  -c 'c([],[])' --version 5 -w 200 --skip-uns --parse-uns -e [main]  --keep-sym [secret] --stats "stats/barrier_ca_oop.txt" | grep 'data leak\|program is'

echo "-------------- Executing sa_ip_barr --------------------"
spectector "transfail/sa_ip_barr.s" $SOLVER -s -a noninter --steps 1000000  -c 'c([],[])' --version 5 -w 200 --skip-uns --parse-uns -e [main]  --keep-sym [real_secret]  --stats "stats/barrier_sa_ip.txt"| grep 'data leak\|program is'


echo "-------------- Executing sa_oop_barr --------------------"
spectector "transfail/sa_oop_barr.s" $SOLVER -s -a noninter --steps 1000000  -c 'c([],[])' --version 5 -w 200 --skip-uns --parse-uns --keep-sym [secretarray] -e [call_start] --stats "stats/barrier_sa_oop.txt" | grep 'data leak\|program is'


echo "---- Now with retpoline defense-----"

echo "-------------- Executing ca_ip_retpoline --------------------"
spectector "transfail/ca_ip_retpoline.s" $SOLVER -s -a noninter --steps 1000000  -c 'c([],[])' --version 5 -w 200  --skip-uns --parse-uns  -e [main]  --keep-sym [real_secret] --stats "stats/retpo_ca_ip.txt" | grep 'data leak\|program is'


echo "-------------- Executing ca_oop_retpoline --------------------"
spectector "transfail/ca_oop_retpoline.s" $SOLVER -s -a noninter --steps 1000000  -c 'c([],[])' --version 5 -w 200 --skip-uns --parse-uns -e [main]  --keep-sym [secret]  --stats "stats/retpo_ca_oop.txt"| grep 'data leak\|program is'

echo "-------------- Executing sa_ip_retpoline --------------------"
spectector "transfail/sa_ip_retpoline.s" $SOLVER -s -a noninter --steps 1000000  -c 'c([],[])' --version 5 -w 200 --skip-uns --parse-uns -e [main]  --keep-sym [real_secret] --stats "stats/retpo_sa_ip.txt" | grep 'data leak\|program is'

echo "-------------- Executing sa_oop_retpoline --------------------"
spectector "transfail/sa_oop_retpoline.muasm" $SOLVER -s -a noninter --steps 1000000  -c 'c([],[])' --version 5 -w 200 --skip-uns --parse-uns  --keep-sym [secretarray] -e [call_start] --stats "stats/retpo_sa_oop.txt" | grep 'data leak\|program is'

for file in stats/*
  do
      truncate -s -1 $file
  done
