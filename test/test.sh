#!/bin/bash

SCRIPT_DIR=$(dirname ${BASH_SOURCE[0]})
SMPPLOAD=$(which smppload 2>/dev/null || echo $SCRIPT_DIR/smppload)

HOST=localhost
PORT=2775
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
        !dlr) dlr_flag=0;;
        dlr) dlr_flag=1
    esac

    echo -en "$command\t$delivery\t"

    $SMPPLOAD --host=$HOST --port=$PORT \
        --system_type=$SYSTEM_TYPE --system_id=$SYSTEM_ID --password=$PASSWORD \
        --source=$SRC_ADDR --destination=$DST_ADDR --body="$command" \
        --delivery=$dlr_flag --submit_timeout=5000 --delivery_timeout=5000 \
        -vv | grep "$pattern" > /dev/null

    ret=$?
    if [[ $ret == 0 && "$invert" == "with" ]]; then
        echo -e "\e[32mOK\e[0m"
    elif [[ $ret == 1 && "$invert" == "w/o" ]]; then
        echo -e "\e[32mOK\e[0m"
    else
        echo -e "\e[31mFAIL\e[0m"
        EXIT=1
    fi
}

# try to start
$SCRIPT_DIR/../rel/smppsink/bin/smppsink start > /dev/null
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


check "submit:{timeout:inf}" !dlr with "ERROR: Timeout"
check "submit:{timeout:infinity}" !dlr with "ERROR: Timeout"
check "receipt:{timeout:inf}" dlr with "ERROR: Delivery timeout"
check "receipt:{timeout:infinity}" dlr with "ERROR: Delivery timeout"


# allow receipt status to be any string and integer
check "receipt:abc" dlr with "stat:abc"
check "receipt:{status:abc}" dlr with "stat:abc"
check "receipt:123" dlr with "stat:123"
check "receipt:{status:123}" dlr with "stat:123"

#
check "submit:{status:[]}" !dlr w/o "ERROR"
check "submit:{status:[]}" dlr with "stat:DELIVRD"

check "submit:{status:{code:1,freq:1.0}}" !dlr with "ERROR: Failed with: (0x00000001)"
check "submit:{status:[{code:1,freq:1.0}]}" !dlr with "ERROR: Failed with: (0x00000001)"

# for both below
#check "submit:{status:[{code:1,freq:0.3}]} !dlr with ""
#check "submit:{status:[{code:0,freq:0.3},{code:1,freq:0.7}]}
# something like
#INFO:  Stats:
#INFO:     Send success:     70
#INFO:     Delivery success: 70
#INFO:     Send fail:        30
#INFO:     Delivery fail:    0
#INFO:     Errors:           0
#INFO:     Avg Rps:          101 mps
#INFO:  Unbound

# stop if wasn't running
if [[ $start_ret == 0 ]]; then
    $SCRIPT_DIR/../rel/smppsink/bin/smppsink stop > /dev/null
fi

exit $EXIT
