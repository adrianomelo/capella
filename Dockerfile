FROM haskell:9.4.4-slim-buster as builder

WORKDIR /app

COPY . .

RUN apt-get update \
    && apt-get install libghc-pcre-light-dev -y \
    && cabal update

RUN cabal install --installdir=/app --install-method=copy

FROM debian:buster-slim

WORKDIR /app

CMD ["/app/capella-exe"]

EXPOSE 8000

RUN apt-get update \
    && apt-get install libgmp-dev netbase libstdc++6 ca-certificates libc-bin libghc-pcre-light-dev -y

COPY --from=builder /app/capella-exe /app/capella-exe
COPY --from=builder /app/small.gif /app/small.gif
