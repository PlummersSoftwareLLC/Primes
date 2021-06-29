FROM adoptopenjdk:11-jdk-hotspot

SHELL ["/bin/bash", "-c"]
RUN apt-get update && apt-get install -y zip unzip curl bash
RUN curl -s https://get.sdkman.io | bash

RUN source /root/.sdkman/bin/sdkman-init.sh;sdk install kotlin 1.5.10

WORKDIR /opt/app

COPY primes.kt .
RUN source /root/.sdkman/bin/sdkman-init.sh;kotlinc ./ -d primes.jar


#ENTRYPOINT ["kotlin","primes.jar"]
ENTRYPOINT ["/bin/bash"]
