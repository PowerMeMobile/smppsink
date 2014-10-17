#!/bin/bash

SCRIPT_DIR=$(dirname ${BASH_SOURCE[0]})

HOST=localhost
PORT=8002
SYSTEM_TYPE=smpp
SYSTEM_ID=test
PASSWORD=test
SRC_ADDR=375296660002
DST_ADDR=375296543210

EXIT=0

function check() {
    local command="$1"
    local delivery="$2"
    local invert="$3"
    local pattern="$4"

    case "$delivery" in
        !dlr) delivery_flag=false;;
        dlr) delivery_flag=true
    esac

    case "$invert" in
        w/o) invert_match="--invert-match";;
        with) invert_match=""
    esac

    echo -en "$command\t$delivery\t"

    echo -n "$SRC_ADDR;$DST_ADDR;$command;$delivery_flag;3" |
    $SCRIPT_DIR/smppload --host=$HOST --port=$PORT --system_type=$SYSTEM_TYPE --system_id=$SYSTEM_ID --password=$PASSWORD \
        --file - -vv | grep $invert_match "$pattern" > /dev/null

    if [[ "$?" != 0 ]]; then
        echo -e "\e[31mFAIL\e[0m"
        EXIT=1
    else
        echo -e "\e[32mOK\e[0m"
    fi
}

# try to start
$SCRIPT_DIR/../rel/smppsink/bin/smppsink start
start_ret=$?
if [[ $start_ret == 0 ]]; then
    # give time to init
    sleep 5
fi

check "submit: 0"   !dlr w/o "ERROR"
check "submit: 0x0" !dlr w/o "ERROR"
check "submit: 1"   !dlr with "ERROR: Failed with: (0x00000001)"
check "submit: 0x1" !dlr with "ERROR: Failed with: (0x00000001)"

check "submit: {status: 1}" !dlr with "ERROR: Failed with: (0x00000001)"
check "submit: {status: 1, timeout: 0}" !dlr with "ERROR: Failed with: (0x00000001)"

check "{submit: 1}" !dlr with "ERROR: Failed with: (0x00000001)"
check "{submit: {status: 1}}" !dlr with "ERROR: Failed with: (0x00000001)"
check "{submit: {status: 1, timeout: 0}}" !dlr with "ERROR: Failed with: (0x00000001)"


check "receipt: enroute" !dlr w/o "stat:ENROUTE"
check "receipt: enroute" dlr with "stat:ENROUTE"

check "receipt: {status: enroute}" dlr with "stat:ENROUTE"
check "receipt: {status: enroute, timeout: 0}" dlr with "stat:ENROUTE"

check "{receipt: enroute}" dlr with "stat:ENROUTE"
check "receipt: {status: enroute}" dlr with "stat:ENROUTE"
check "receipt: {status: enroute, timeout: 0}" dlr with "stat:ENROUTE"


check "{submit: 0, receipt: enroute}" dlr with "stat:ENROUTE"
check "{submit: {status: 0, timeout: 0}, receipt: {status: enroute, timeout: 0}}" dlr with "stat:ENROUTE"

check "{submit: 1, receipt: unknown}" dlr with "ERROR: Failed with: (0x00000001)"

# w/o spaces
check "{submit:{status:0,timeout:0},receipt:{status:enroute,timeout:0}}" dlr with "stat:ENROUTE"

# stop if wasn't running
if [[ $start_ret == 0 ]]; then
    $SCRIPT_DIR/../rel/smppsink/bin/smppsink stop > /dev/null
fi

exit $EXIT
