#!/bin/sh

CLASSPATH='.:argonaut_2.12-6.2.2.jar:junit-4.8.2.jar'

scalac -classpath $CLASSPATH com/mylife/test/*.scala
#javac -classpath $CLASSPATH AtdjTest.java
#javadoc -quiet -Xdoclint:none -classpath $CLASSPATH -d doc/test \
#        -public com.mylife.test -quiet \
#    | grep -v "Creating destination directory"
#java  -classpath $CLASSPATH AtdjTest | grep -v -E '^(Time:|JUnit version)' > java.trace
