FROM haskell:7.10

WORKDIR /opt/server

# Installing Redis and supervisor
RUN apt-get update && apt-get install -y redis-server supervisor
RUN mkdir -p /var/log/supervisor
COPY Docker/supervisord.conf /etc/supervisor/conf.d/supervisord.conf

# Updating stack
RUN stack update
# Add and Install Application Code
COPY . /opt/server
RUN stack install

RUN mkdir -p /opt/server/uploads
EXPOSE 3000
CMD ["/usr/bin/supervisord"]
