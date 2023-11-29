# Commands to execute all the combined example with the correct options
# Sometimes -e is used because the entry point is not at 0 but at a specifc function e.g., main.
# 
# V1 + V4
rm -r stats/*
common_args=($SOLVER -s -a noninter --steps 1000000  -c 'c([],[])' -w 200 --skip-uns -e [main] --parse-uns)
# First arg is program name without suffix and second arg is version executed under
exec_version() {
 echo "--------------Execute $1 under Version $2--------------------"
 spectector "$1.muasm" ${common_args[@]} --version $2 --stats "stats/$1_$2_stats.txt" | grep 'data leak\|program is'
 echo "----With a barrier inserted ----"
 spectector "$1_barrier.muasm" ${common_args[@]} --version $2 --stats "stats/$1_$2_stats_barrier.txt" | grep 'data leak\|program is'

}
 echo "--------------Executions of combined14 under each respective version--------------------"
exec_version "combined14" 14
exec_version "combined14" 41 # Corect versoin of version 4
exec_version "combined14" 1
echo "--------------Executions of combined45 under each respective version--------------------"
exec_version "combined45" 45
exec_version "combined45" 41
exec_version "combined45" 5
echo "--------------Executions of combined15 under each respective version--------------------"
exec_version "combined15" 15
exec_version "combined15" 1
exec_version "combined15" 5
echo "--------------Executions of combined145 under each respective version--------------------"
exec_version "combined145" 145
exec_version "combined145" 1
exec_version "combined145" 4
exec_version "combined145" 5
for file in stats/*
  do
      truncate -s -1 $file
  done
exit
# V4 + V5
 echo "--------------V4 + V5 --------------------"
 spectector "combined45.muasm" ${common_args[@]} --version 45 | grep 'data leak\|program is'
 echo "----With a barrier inserted ----"
 spectector "combined45_barrier.muasm" ${common_args[@]} --version 45  | grep 'data leak\|program is'
# V1 + V5
 echo "--------------V1 + V5 --------------------"
spectector "combined15.muasm" $common_args --version 15 | grep 'data leak\|program is'
echo "----With a barrier inserted ----"
spectector "combined15_barrier.muasm" $common_args --version 15 | grep 'data leak\|program is'

# V1 + V4 + V5
 echo "--------------V1 + V4 + V5 --------------------"
 spectector "combined145.muasm" $common_args --version 145 | grep 'data leak\|program is'

 echo "----With a barrier inserted ----"
spectector "combined145_barrier.muasm" $common_args --version 145 | grep 'data leak\|program is'
