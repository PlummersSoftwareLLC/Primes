#!/bin/bash

# Start Lucee/Tomcat
export LUCEE_ENABLE_WARMUP=true
/usr/local/tomcat/bin/catalina.sh run &


# Wait for server to start
#sleep 20

# Fetch the page and convert HTML to plaintext
#curl -s http://host.docker.internal:8888/index.cfm 

# Shutdown Lucee cleanly
#/usr/local/tomcat/bin/catalina.sh stop
