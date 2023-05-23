FROM ubuntu:latest

RUN apt-get update && apt-get install -y open-cobol

CMD ["/bin/bash"]
