#!/bin/bash
# 通过将宿主机的目录挂载到这个位置，可以将每次 quicklisp 下载的包缓存起来。
QUICKLISP_DIRECTORY='/root/.roswell/lisp/quicklisp'
QUICKLISP_INSTALLATION_DIRECTORY="${QUICKLISP_DIRECTORY}/dists/quicklisp/software/"
QUICKLISP_LOCAL_PROJECT_DIRECTORIES="${QUICKLISP_DIRECTORY}/local-projects/"

# --rm 使得容器退出后被自动删除，避免占用过多磁盘空间。
# --load 使得 SBCL 在执行脚本前仍然可以加载配置文件，以便使用 quicklisp。
# --quit 让 SBCL 在运行完脚本 main.lisp 后退出进程，而不是启动 REPL。
docker run -it \
--rm \
-p 4006:4006 \
-v "/Users/liutos/Data/quicklisp_for_docker:${QUICKLISP_INSTALLATION_DIRECTORY}" \
-v `pwd`:"${QUICKLISP_LOCAL_PROJECT_DIRECTORIES}/cl-accounting" \
fukamachi/sbcl:latest-ubuntu \
--load "${QUICKLISP_LOCAL_PROJECT_DIRECTORIES}/cl-accounting/main.lisp"
