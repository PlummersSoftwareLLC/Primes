#!/bin/bash

# Start Tomcat in the background
catalina.sh start > /dev/null

# Poll for Lucee to be ready
until curl -s http://localhost:8888/ping.cfm | grep -q "pong"; do
  sleep 1
done

# Run benchmarks, output results
curl -s http://localhost:8888/index.cfm | html2text -width 255

# Shutdown Tomcat
catalina.sh stop
