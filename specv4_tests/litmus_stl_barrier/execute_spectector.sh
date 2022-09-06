#!/bin/bash
rm -r stats/*
for file in *.s
do
	echo "-------Executing $file ------------"
        spectector "$file" $SOLVER -s -a noninter --steps 1000000 --parse-uns --skip-uns --version 4 -w 200 --stats "stats/stats_barrier_$file.txt" --keep-sym [secretarray] | grep 'data leak\|program is'
done
for file in stats/* # Stats writes an extra comma that we delete
  do
      truncate -s -1 $file
  done
