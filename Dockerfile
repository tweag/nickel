# This Dockerfile is used for our release builds, because we distribute
# docker images that provide Nickel binaries.

FROM scratch
LABEL org.opencontainers.image.source="https://github.com/tweag/nickel" \
      org.opencontainers.image.description="Nickel: better configuration for less" \
      org.opencontainers.image.licenses="MIT";

COPY ./nickel /bin/nickel
COPY ./nls /bin/nls

ENTRYPOINT ["/bin/nickel"]
