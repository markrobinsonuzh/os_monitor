apt-get update && apt-get install -y \
        bzip2 \
        ca-certificates \
        ca-certificates-java \
        curl \
        default-jre-headless \
        git \
        java-common \
        jq \
        libcairo2-dev \
        libcurl4-gnutls-dev \
        libfontconfig \
        libssl-dev \
        libssh2-1-dev \
        libxml2 \
        libxml2-dev \
        libxt-dev \
        openjdk-11-jre-headless \
        odbc-postgresql \
        pandoc \
        pandoc-citeproc \
        sudo \
        unixodbc \
        unixodbc-dev \
        vim \
        && mkdir /tmp/phantomjs \
        && curl -L https://bitbucket.org/ariya/phantomjs/downloads/phantomjs-2.1.1-linux-x86_64.tar.bz2 \
                      | tar -xj --strip-components=1 -C /tmp/phantomjs \
        && cd /tmp/phantomjs \
        && mv bin/phantomjs /usr/local/bin \
        && cd \
        && apt-get clean \
        && rm -rf /tmp/* /var/lib/apt/lists/*
