FROM swift:5.3.3-bionic

WORKDIR /opt/app

COPY . .

RUN swift build --configuration release

ENTRYPOINT ["./.build/release/PrimeSieveSwift", "-n", "1000000", "-t", "5.0"]