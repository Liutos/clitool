FROM fukamachi/sbcl:latest-ubuntu

# 参考[这里](https://learnku.com/articles/26066)的办法，将软件源地址替换为阿里云镜像以便加速。
# 安装访问 MySQL 需要用到的依赖。
RUN sed -i s@/deb.debian.org/@/mirrors.aliyun.com/@g /etc/apt/sources.list \
  && apt-get clean \
  && apt-get update \
  && apt-get install -y libmysqlclient-dev \
  && apt-get install -y build-essential

# 安装常用的第三方库。
RUN sbcl --eval "(ql:quickload '(cl-dbi hunchentoot local-time swank yason))"

# COPY . /app/
# # 将这个软件的目录做一个软链放到 quicklisp 的 local project 目录下。
# WORKDIR /app/
# RUN ln -s '/app/cl-accounting.asd' '/root/.roswell/lisp/quicklisp/local-projects/cl-accounting.asd'

EXPOSE 4006 4242

# ENTRYPOINT ["sbcl", "--load", "/app/main.lisp"]
ENTRYPOINT ["bash", "/app/main.sh"]
