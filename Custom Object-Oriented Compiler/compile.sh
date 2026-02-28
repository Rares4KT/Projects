#!/bin/bash
if [ -z "$1" ]; then
    echo "Usage: ./compile.sh <name>"
    exit 1
fi

NAME=$1
rm -f lex.yy.c $NAME.tab.c $NAME.tab.h $NAME
flex $NAME.l
bison -d $NAME.y
g++ lex.yy.c $NAME.tab.c -o $NAME -std=c++17
