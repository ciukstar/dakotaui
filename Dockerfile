FROM ubuntu:22.04
RUN mkdir -p /opt/dakotaui \
        && apt-get update \
        && apt-get install -y --no-install-recommends build-essential zlib1g-dev libpq-dev libicu-dev \
        && apt-get clean \
        && rm -rf /var/lib/apt/lists/*

ARG YESOD_DEMO_LANG=EN

WORKDIR /opt/dakotaui
COPY dakotaui /opt/dakotaui
COPY static /opt/dakotaui/static
COPY config /opt/dakotaui/config

ENV YESOD_PORT=8080
ENV DEMO_LANG=${YESOD_DEMO_LANG}

EXPOSE 8080
CMD ["./dakotaui"]
