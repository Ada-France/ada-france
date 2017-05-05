#!/bin/sh

### BEGIN INIT INFO
# Provides:	  adafr-server
# Required-Start:    $local_fs $remote_fs $network $syslog $named $apache2
# Required-Stop:     $local_fs $remote_fs $network $syslog $named $apache2
# Default-Start:     2 3 4 5
# Default-Stop:      0 1 6
# Short-Description: starts the Ada France Web server
# Description:       starts the Ada France Web server
### END INIT INFO

NAME=adafr
ADAFR_DIR=/var/lib/$NAME
ADAFR_USER=root
ADAFR_LOG=/var/log
ADAFR_RUN=/var/run

# Read configuration variable file if it is present
[ -r /etc/default/$NAME ] && . /etc/default/$NAME

DAEMON=daemon

start_server() {

  $DAEMON --name $NAME --respawn --delay=60 --inherit \
      --pidfile $ADAFR_RUN/$NAME.pid --output $ADAFR_LOG/$NAME.log \
      --user $ADAFR_USER $ADAFR_DIR/adafr-server.sh
}

stop_server() {
  $DAEMON --name $NAME --stop --pidfile $ADAFR_LOG/$NAME.pid
}

case $1 in
  stop)
     stop_server
     ;;
  start)
     start_server
     ;;
  restart)
     stop_server
     start_server
     ;;
  *)
     echo "Usage: adafr-server.sh [start|stop|restart]"
     ;;
esac

