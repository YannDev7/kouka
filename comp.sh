#!/bin/bash

if g++ -DDEBUG -std=gnu++20 -Wall -Wextra -Wshadow -g -fsanitize=address,undefined $1; then
    for i in *.in; do
        echo $i
        ./a.out < $i 
    done
fi