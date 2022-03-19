FROM rust:latest

RUN apt-get update && apt-get -y upgrade && apt-get -y install gdb
