FROM haskell:9.12.2

COPY . .

RUN cabal update
RUN cabal build

EXPOSE 8080

CMD ["cabal", "run"]
