#!/bin/bash

CLASSPATH='.:json.jar:junit-4.8.2.jar'

$1 test.atd -package com.mylife.test
javac -classpath $CLASSPATH com/mylife/test/*.java
javac -classpath $CLASSPATH AtdjTest.java
javadoc -classpath $CLASSPATH -d doc/test -public com.mylife.test
java  -classpath $CLASSPATH AtdjTest
