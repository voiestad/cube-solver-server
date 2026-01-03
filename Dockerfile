FROM haskell:9.6.7

WORKDIR /app
COPY cabal.project cube-solver-server.cabal /app/

RUN cabal update
RUN cabal build --only-dependencies

COPY . /app

RUN cabal build

EXPOSE 8080

CMD ["cabal", "run"]
