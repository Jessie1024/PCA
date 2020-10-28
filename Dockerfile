FROM rocker/verse
MAINTAINER Qianhui Yang <jessy1024qh@gmail.com>
RUN apt update -y && apt install -y python3-pip
RUN pip3 install jupyter jupyterlab
RUN pip3 install numpy pandas sklearn plotnine matplotlib pandasql bokeh
