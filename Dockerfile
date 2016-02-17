FROM haskell:7.10

WORKDIR /opt/server

RUN stack update

# Add and Install Application Code
COPY . /opt/server
RUN stack install

CMD ["easync"]
