#!/bin/bash
rm -r stats/*
for file in *.s
do
	echo "-------Executing $file ------------"
        spectector "$file" $SOLVER -s -a noninter --steps 1000000  --version 4 -w 200 --stats "stats/stats_$file.txt" --keep-sym [secretarray] | grep 'data leak\|program is'
done

echo "-------Executing case9.s ------------"
        spectector "case9/case9.s" $SOLVER -s -a noninter --steps 1000000  --version 4 -w 10 --stats "stats/stats_case9.s.txt" --keep-sym [secretarray] | grep 'data leak\|program is'
for file in stats/* # Stats writes an extra comma that we delete
  do
      truncate -s -1 $file
  done
