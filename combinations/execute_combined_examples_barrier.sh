#!/bin/bash
# Commands to execute all the combined example with the correct options
# Sometimes -e is used because the entry point is not at 0 but at a specifc function e.g., main.
# 
# V1 + V4
rm -r stats/*
common_args=($SOLVER -s -a noninter --steps 1000000  -c 'c([],[])' -w 200 --skip-uns -e [main] --parse-uns)
# First arg is program name without suffix and second arg is version executed under
exec_version() {
 echo "----With a barrier inserted ----"
 spectector "$1_barrier.muasm" ${common_args[@]} --version $2 --stats "stats/$1_$2_stats_barrier.txt" | grep 'data leak\|program is'

}

exec_base() {
    echo "--------------Execute $1 under Baseline --------------------"
    spectector "$1_barrier.muasm" -n -a noninter --steps 100000 -c 'c([],[])' -e [main] --parse-uns  --stats "stats/$1_base_stats_barrier.txt"| grep 'data leak\|program is'
}
 echo "--------------Executions of combined14 under each respective version--------------------"
exec_version "combined14" 14
exec_version "combined14" 4
exec_version "combined14" 1
exec_base "combined14"
echo "--------------Executions of combined45 under each respective version--------------------"
exec_version "combined45" 45
exec_version "combined45" 41
exec_version "combined45" 5
exec_base "combined45"
echo "--------------Executions of combined15 under each respective version--------------------"
exec_version "combined15" 15
exec_version "combined15" 1
exec_version "combined15" 5
exec_base "combined15"
echo "--------------Executions of combined145 under each respective version--------------------"
exec_version "combined145" 145
exec_version "combined145" 1
exec_version "combined145" 4
exec_version "combined145" 5
exec_version "combined145" 14
exec_version "combined145" 15
exec_version "combined145" 45
exec_base "combined145"
for file in stats/*
  do
      truncate -s -1 $file
  done
exit


 
