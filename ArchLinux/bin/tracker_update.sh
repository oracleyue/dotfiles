#!/bin/bash

DAEMON=/usr/bin/aria2c
PID_NAME=aria2c
PID=$(ps ux | awk '/aria2c/ && !/awk/ {print $2}')
CONFIG_PATH='/path/to/aria2.conf'
TRACKER_URL='https://raw.githubusercontent.com/ngosang/trackerslist/master/trackers_all_ip.txt'

#####


check_running() {
    PID=`ps ux | awk '/aria2c/ && !/awk/ {print $2}'`
    if [ -z $PID ]; then
        return 1
    else
        return 0
    fi
}

do_update() {
    list=`wget -qO- $TRACKER_URL|sed '/^$/d'|perl -pe 'chop if eof'|tr "\n" ","`
    if [ -z "`grep "bt-tracker" $CONFIG_PATH`" ]; then
        echo -e "\nbt-tracker=${list}" >> ${CONFIG_PATH}
        echo 'bt-tracker added!'
    else
        sed -i '' "s@^bt-tracker.*@bt-tracker=$list@g" ${CONFIG_PATH}
        echo 'bt-tracker updated!'
    fi
	do_restart
}

do_start() {
    if check_running; then
        echo "$PID_NAME (pid $PID) is already running..."
        return 0
    fi
    $DAEMON
    if check_running; then
        echo "Starting $PID_NAME success"
    else
        echo "Starting $PID_NAME failed"
        RET_VAL=1
    fi
}

do_stop() {
    if check_running; then
        kill -9 $PID
        echo "Stopping $PID_NAME success"
    else
        echo "$PID_NAME is stopped"
        RET_VAL=1
    fi
}

do_restart() {
    do_stop
    do_start
}

do_status() {
    check_running
    case $? in
        0)
        echo "$PID_NAME (pid $PID) is running..."
        ;;
        1)
        echo "$PID_NAME is stopped"
        RET_VAL=1
        ;;
    esac
}

case "$1" in
    start|stop|restart|status|update)
    do_$1
    ;;
    *)
    echo "Usage: $0 { start | stop | restart | status |update }"
    RET_VAL=1
    ;;
esac

exit $RET_VAL
