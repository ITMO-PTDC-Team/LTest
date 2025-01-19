FROM silkeh/clang:19 AS ltest

RUN apt update && apt install -y git ninja-build valgrind libgflags-dev libstdc++-11-dev libclang-19-dev
RUN mv /usr/lib/gcc/x86_64-linux-gnu/12 /usr/lib/gcc/x86_64-linux-gnu/_12

FROM ltest as blocking
RUN apt install -y pkg-config libcapstone-dev libboost-context-dev && \
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
FROM ltest as userver-blocking
# userver conflicts with default libboost-context-dev (1.74) version, 1.81 required
RUN apt install -y python3-dev python3-venv \
        libboost-context1.81-dev libboost-filesystem1.81-dev libboost-program-options1.81-dev libboost-regex1.81-dev 
        libboost-stacktrace1.81-dev libboost-locale1.81-dev \
        libzstd-dev libyaml-cpp-dev libcrypto++-dev libnghttp2-dev libev-dev
RUN sh -c "$(wget -O- https://github.com/deluan/zsh-in-docker/releases/download/v1.2.1/zsh-in-docker.sh)" -- \
       -p git
CMD [ "zsh" ]
