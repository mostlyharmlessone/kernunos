#!/bin/bash
iconv --list | sed -e 's/\/\///g' | while read encoding
do
    transcoded=$(head -n1 RAOPD.DAT | iconv -sc -f $encoding -t UTF-8)
    echo "$encoding $transcoded"
done
