#!/bin/bash

HOST=localhost
PORT=2775
SYSTEM_TYPE=""
SYSTEM_ID=user
PASSWORD=password
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

    echo -n "$SRC_ADDR;$DST_ADDR;$command;$delivery_flag;3" |
    smppload --host=$HOST --port=$PORT --system_type=$SYSTEM_TYPE --system_id=$SYSTEM_ID --password=$PASSWORD \
        --file - -vv | grep $invert_match "$pattern" > /dev/null

    if [[ "$?" != 0 ]]; then
        echo -e "$command\t$delivery\t\e[31mFAIL\e[0m"
        EXIT=1
    else
        echo -e "$command\t$delivery\t\e[32mOK\e[0m"
    fi
}

check "submit: 0"   !dlr w/o "ERROR"
check "submit: 0x0" !dlr w/o "ERROR"
check "submit: 1"   !dlr with "ERROR: Failed with: (0x00000001)"
check "submit: 0x1" !dlr with "ERROR: Failed with: (0x00000001)"

check "submit: {status: 1}" !dlr with "ERROR: Failed with: (0x00000001)"
check "submit: {status: 1, timeout: 0}" !dlr with "ERROR: Failed with: (0x00000001)"

check "{submit: 1}" !dlr with "ERROR: Failed with: (0x00000001)"
check "{submit: {status: 1}}" !dlr with "ERROR: Failed with: (0x00000001)"
check "{submit: {status: 1, timeout: 0}}" !dlr with "ERROR: Failed with: (0x00000001)"


check "receipt: delivered" !dlr w/o "stat:DELIVRD"
check "receipt: delivered" dlr with "stat:DELIVRD"

check "receipt: {status: delivered}" dlr with "stat:DELIVRD"
check "receipt: {status: delivered, timeout: 0}" dlr with "stat:DELIVRD"

check "{receipt: delivered}" dlr with "stat:DELIVRD"
check "receipt: {status: delivered}" dlr with "stat:DELIVRD"
check "receipt: {status: delivered, timeout: 0}" dlr with "stat:DELIVRD"


check "{submit: 0, receipt: delivered}" dlr with "stat:DELIVRD"
check "{submit: {status: 0, timeout: 0}, receipt: {status: delivered, timeout: 0}}" dlr with "stat:DELIVRD"

check "{submit: 1, receipt: unknown}" dlr with "ERROR: Failed with: (0x00000001)"

# w/o spaces
check "{submit:{status:0,timeout:0},receipt:{status:delivered,timeout:0}}" dlr with "stat:DELIVRD"

exit $EXIT
