FROM silkeh/clang:19 AS ltest

RUN apt update && apt install -y git ninja-build valgrind libboost-context-dev libgflags-dev libstdc++-11-dev libclang-19-dev
RUN mv /usr/lib/gcc/x86_64-linux-gnu/12 /usr/lib/gcc/x86_64-linux-gnu/_12

FROM ltest as blocking
RUN apt install -y pkg-config libcapstone-dev && \
    git clone https://github.com/Kirillog/syscall_intercept.git &&  \
    cmake syscall_intercept -G Ninja -B syscall_intercept/build -DCMAKE_INSTALL_PREFIX=/usr -DCMAKE_BUILD_TYPE=Release -DCMAKE_C_COMPILER=clang && \
    cmake --build syscall_intercept/build --target install
FROM blocking as folly-blocking
RUN apt install -y libboost-filesystem-dev libboost-program-options-dev libboost-regex-dev \
                    libdouble-conversion-dev libfast-float-dev libevent-dev libssl-dev libfmt-dev \
                    libgoogle-glog-dev zlib1g-dev && \
    git clone https://github.com/Kirillog/folly.git && \
    cmake folly -G Ninja -B folly/build_dir -DCMAKE_BUILD_TYPE=Release
    # cmake --build folly/build_dir --target install
RUN sh -c "$(wget -O- https://github.com/deluan/zsh-in-docker/releases/download/v1.2.1/zsh-in-docker.sh)" -- \
       -p git
CMD [ "zsh" ]
