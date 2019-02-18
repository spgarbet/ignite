#!/bin/bash

for i in $(seq 1 9800)
do
    if [ ! -f "output/run-$i.Rdata"  ]; then
      echo -ne "$i,"
    fi
done
echo ""

