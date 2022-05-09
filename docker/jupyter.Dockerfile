FROM jupyter/datascience-notebook

EXPOSE 8888

USER root

RUN apt-get update
RUN apt-get install -y mc 
