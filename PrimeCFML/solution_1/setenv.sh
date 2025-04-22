

# Default memory settings if not specified in $LUCEE_JAVA_OPTS
: ${LUCEE_JAVA_OPTS:="-Xms64m -Xmx1024m"}

# Use /dev/urandom for EGD (http://wiki.apache.org/tomcat/HowTo/FasterStartUp)
CATALINA_OPTS="${LUCEE_JAVA_OPTS} -Djava.security.egd=file:/dev/./urandom";

export CATALINA_OPTS;

# Add location of Apache Tomcat native library to the library path
LD_LIBRARY_PATH="$LD_LIBRARY_PATH:/usr/lib/x86_64-linux-gnu";
export LD_LIBRARY_PATH;

(sh -c "sleep 10 && curl -s http://localhost:8888/index.cfm") &

