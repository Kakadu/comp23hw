#!/bin/bash

RED='\033[0;31m'
GREEN='\033[0;32m'
NC='\033[0m'

LOGO=1
DIR=1
BINDINGS="../lib/bindings.o"

while getopts "ldb:" flag; do
  case "${flag}" in
    "l") LOGO=0 ;;
    "d") DIR=0 ;;
    "b") BINDINGS=$OPTARG ;;
  esac
done

cd "./cm_build"

if [[ $DIR -eq 1 ]]
then
  echo "Executing build in $PWD..."
  echo "Compiling program..."
fi

nasm -f elf64 program.asm -o program.o 1> /dev/null 2> /dev/null
if [[ $? -ne 0 ]]
then
  echo -e "${RED}Error compiling program"
  echo -e "${RED}Failure"
  exit $?
fi

if [[ $DIR -eq 1 ]]
then
  echo "Linking program..."
fi

gcc $BINDINGS program.o -o program 1> /dev/null 2> /dev/null
if [[ $? -ne 0 ]]
then
  echo -e "${RED}Error linking stuff"
  echo -e "${RED}Failure"
  exit $?
fi

# print successful run
if [[ $LOGO -eq 1 ]]
then
  echo
  echo -e "${GREEN}   ___     ___        _____                               __  __ _"
  echo -e "${GREEN}  |   |   |   |      / ____|                             |  \/  | |"
  echo -e "${GREEN}  |___|___|___|     | |     _ __ ___  ___ _ __   ___ _ __| \  / | |"
  echo -e "${GREEN}     _|   |_        | |    | '__/ _ \/ _ \ '_ \ / _ \ '__| |\/| | |"
  echo -e "${GREEN}    |  ___  |       | |____| | |  __/  __/ |_) |  __/ |  | |  | | |____"
  echo -e "${GREEN}    |_|   |_|        \_____|_|  \___|\___| .__/ \___|_|  |_|  |_|______|"
  echo -e "${GREEN}                                         |_|"
  echo -e "👌 ${NC}Successful run"
fi