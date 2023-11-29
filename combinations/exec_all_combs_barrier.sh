common_args=($SOLVER -s -a noninter --steps 1000000  -c 'c([],[])' -w 200 --skip-uns -e [main] --parse-uns)
# First arg is program name without suffix and second arg is version executed under
exec_version() {
 echo "--------------Execute $1 under Version $2--------------------"
 spectector "$1_barrier.muasm" ${common_args[@]} --version $2 --stats "stats/$1_$2_stats.txt" | grep 'data leak\|program is'
#  echo "----With a barrier inserted ----"
#  spectector "$1_barrier.muasm" ${common_args[@]} --version $2 --stats "stats/$1_$2_stats_barrier.txt" | grep 'data leak\|program is'

}
echo "---------------- Executes all the combinations on a snippet showing the combined vulenerability"
 echo "--------------Executions of all 2 combinations--------------------"
exec_version "combined15" 15
exec_version "combined14" 14
exec_version "combined45" 45
exec_version "combined16" 16
exec_version "combined46" 46
exec_version "combined12" 12
exec_version "combined24" 24
exec_version "combined25" 25
exec_version "combined26" 26
echo "--------------Executions of all 3 combinations --------------------"
exec_version "combined145" 145
exec_version "combined146" 146
exec_version "combined124" 124
exec_version "combined125" 125
exec_version "combined126" 126
exec_version "combined245" 245
exec_version "combined246" 246
echo "--------------Executions of all 4 combinations--------------------"
exec_version "combined1245" 1245
exec_version "combined1246" 1246
for file in stats/*
  do
      truncate -s -1 $file
  done
exit