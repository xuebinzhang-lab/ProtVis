# 国内可正常拉取的 R 基础镜像
FROM r-base:latest

WORKDIR /app

COPY . .

# 安装 shiny 必需包
RUN R -e "install.packages(c('shiny'), repos='https://mirrors.tuna.tsinghua.edu.cn/CRAN/')"

EXPOSE 3838

CMD ["R", "-e", "shiny::runApp('/app', host='0.0.0.0', port=3838)"]