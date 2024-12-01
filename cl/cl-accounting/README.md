# 记账

## 用腾讯云下载镜像

```shell
docker pull fukamachi/sbcl:latest-ubuntu
```

然后将其导出为文件

```shell
sudo docker save 51ac51e2643e > /tmp/51ac51e2643e.tar
```

再下载到本地

```shell
scp 'ubuntu@*.*.*.*:/tmp/51ac51e2643e.tar' .
```

最后安装到本地的 Docker 镜像中

```shell
docker load < ./51ac51e2643e.tar
```

将其重命名为稍后要使用的格式

```shell
docker tag 51ac51e2643e fukamachi/sbcl:latest-ubuntu
```

## 用 dockerproxy 下载镜像

```shell
docker pull dockerproxy.net/fukamachi/sbcl:latest-ubuntu
```

## 如何构建？

```shell
bash ./build.sh
```

## 如何启动？

```shell
bash run.sh
```
