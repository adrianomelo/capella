FROM haskell:8 as builder

WORKDIR /app

COPY . .

RUN apt-get update && apt-get install && stack setup && stack install && cp `stack path --local-bin`/capella-exe /app/capella-exe


FROM debian:buster-slim

WORKDIR /app

CMD ["/app/capella-exe"]

EXPOSE 8000

RUN apt-get update && apt-get install libgmp-dev netbase libstdc++6 ca-certificates libc-bin -y

COPY . .

COPY --from=builder /app/capella-exe /app/capella-exe

