#!/bin/sh

set -e
CLASSPATH='.:argonaut_2.12-6.2.2.jar:junit-4.8.2.jar'

scalac -classpath $CLASSPATH com/mylife/test/*.scala
scalac -classpath $CLASSPATH AtdsTest.scala
#javadoc -quiet -Xdoclint:none -classpath $CLASSPATH -d doc/test \
#        -public com.mylife.test -quiet \
#    | grep -v "Creating destination directory"
scala  -classpath $CLASSPATH AtdsTest > scala.trace || {
    cat scala.trace
    exit 1
}
