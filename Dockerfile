FROM haskell:9.6.7

COPY . .

RUN cabal update
RUN cabal build

EXPOSE 8080

CMD ["cabal", "run"]
