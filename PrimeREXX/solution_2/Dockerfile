FROM adoptopenjdk/openjdk11:jdk-11.0.11_9-alpine-slim
WORKDIR /home/netrexx
RUN apk add --no-cache curl unzip \
	&& curl -G https://www.netrexx.org/files/NetRexx-4.01-GA.zip -o ./NetRexx4.zip \
	&& unzip ./NetRexx4.zip -d . \
	&& rm NetRexx4.zip

COPY ./PrimeNetRexx.nrx .

RUN java -jar lib/NetRexxF.jar -compile PrimeNetRexx

ENTRYPOINT ["java","-cp","lib/NetRexxF.jar:.","PrimeNetRexx"]

#Print the primes <= 1000000
#ENTRYPOINT ["java","-cp","lib/NetRexxF.jar:.","PrimeNetRexx","100000","1"]
# - or -
#docker run --rm --entrypoint "java" imageName -cp lib/NetRexxF.jar:. PrimeNetRexx 1000000 1

#Print a summary of the prime counts up to 100, 1000, 10000, 100000 and 1000000
#ENTRYPOINT ["java","-cp","lib/NetRexxF.jar:.","PrimeNetRexx","100000","2"]
# - or -
#docker run --rm --entrypoint "java" imageName -cp lib/NetRexxF.jar:. PrimeNetRexx 1000000 2

#Print the primes <= 1000000 and also a summary as above
#ENTRYPOINT ["java","-cp","lib/NetRexxF.jar:.","PrimeNetRexx","100000","3"]
# - or -
#docker run --rm --entrypoint "java" imageName -cp lib/NetRexxF.jar:. PrimeNetRexx 1000000 3
