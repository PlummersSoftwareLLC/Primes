FROM lucee/lucee:6.2.0.321-light-tomcat10.1-jre21-temurin-jammy
#ONBUILD RUN rm -rf /var/www/*
#WORKDIR /var/www
COPY . /var/www

#RUN chmod +x /var/www/entrypoint.sh

EXPOSE 8888

#CMD ["bash", "/var/www/entrypoint.sh"]