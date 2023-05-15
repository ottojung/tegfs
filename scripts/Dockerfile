# syntax=docker/dockerfile:1
   
FROM debian:bookworm

RUN apt-get update
RUN apt-get install -y guile-3.0 make
RUN apt-get install -y wget file fzf rsync openssh-server swi-prolog ffmpeg imagemagick entr pup

WORKDIR /home/tegfs
COPY . .

RUN make clean && make install && tegfs version

CMD ["bash", "scripts/run-docker.sh"]
EXPOSE 8081
