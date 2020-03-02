#!/usr/bin/env bash

CARDANOCLI=$(find . -name cardano-cli | tail -n 1)
echo $CARDANOCLI

assert_file_absent () {
    if test -f "$1" ; then
        echo "File '$1' is present but should not be."
        error=1
    fi
}

assert_file_exists () {
    if test ! -f "$1" ; then
        echo "Output file '$1' is missing."
        error=1
    fi
}

banner () {
    echo
    echo == "$*" ==
}


fail_test () {
    RESULT="FAILED [ ${testname} ]"
    exit 1
}

pass_test () {
    RESULT="PASSED [ ${testname} ]"
    exit 0
}
