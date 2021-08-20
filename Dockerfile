# Copyright 2021 Andrey Abramov 
# Licensed under BSD-2-Clause https://opensource.org/licenses/BSD-2-Clause
FROM opensuse/leap

RUN zypper --non-interactive install nodejs14 nodejs14-devel docker make gcc10 gcc10-c++ && \
    zypper clean -a && \
    update-alternatives --install /usr/bin/cc cc /usr/bin/gcc-10 10 && \
    update-alternatives --install /usr/bin/g++ g++ /usr/bin/g++-10 10 && \
    mkdir /primes

WORKDIR /primes

CMD ["/bin/bash"]
