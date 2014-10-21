[![Build Status](https://travis-ci.org/PowerMeMobile/smppsink.png?branch=master)](https://travis-ci.org/PowerMeMobile/smppsink)

## Prerequisites

In order to compile and run **smppsink** you need to have [Erlang](http://www.erlang.org/) and make installed.

## Compilation

<pre>
$ git https://github.com/PowerMeMobile/smppsink.git
$ cd
$ make
</pre>

## Starting/stopping

<pre>
$ rel/smppsink/bin/smppsink start
$ rel/smppsink/bin/smppsink stop
</pre>

## Usage
In order to run the examples below you need to have [smppload](https://github.com/PowerMeMobile/smppload).

Return submit status 0x1 (invalid message length) after 1 sec
<pre>
$ smppload -P8002 -tsmpp -itest -ptest -s375296660002 -d375296543210 -b"submit:{status:1,timeout:1}" -D
INFO:  Connected to 127.0.0.1:8002
INFO:  Bound to smppsink
ERROR: Failed with: (0x00000001) Message Length is invalid.
INFO:  Stats:
INFO:     Send success:     0
INFO:     Delivery success: 0
INFO:     Send fail:        1
INFO:     Delivery fail:    0
INFO:     Errors:           0
INFO:     Avg Rps:          0 mps
INFO:  Unbound
</pre>

Return submit status 0x0 (success) after 1 sec, then send delivery receipt "DELIVERED" after 5 secs
<pre>
smppload -P8002 -tsmpp -itest -ptest -s375296660002 -d375296543210 -b"{submit:{status:0,timeout:1},receipt:{status:delivered,timeout:5}}" -D
INFO:  Connected to 127.0.0.1:8002
INFO:  Bound to smppsink
INFO:  Stats:
INFO:     Send success:     1
INFO:     Delivery success: 1
INFO:     Send fail:        0
INFO:     Delivery fail:    0
INFO:     Errors:           0
INFO:     Avg Rps:          0 mps
INFO:  Unbound
</pre>
