FROM lucee/lucee:6.2.0.321-light-tomcat10.1-jre21-temurin-jammy

COPY . /var/www

COPY ./setenv.sh /usr/local/tomcat/bin/
RUN chmod a+x /usr/local/tomcat/bin/setenv.sh

EXPOSE 8888
