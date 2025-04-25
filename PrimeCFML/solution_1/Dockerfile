FROM lucee/lucee:6.2.0.321-light-tomcat10.1-jre21-temurin-jammy

RUN apt-get update && \
    apt-get install -y html2text && \
    apt-get clean && \
    rm -rf /var/lib/apt/lists/*

COPY . /var/www

WORKDIR /app

RUN mv /var/www/docker-run.sh . && \
    chmod a+x docker-run.sh

CMD ["./docker-run.sh"]
