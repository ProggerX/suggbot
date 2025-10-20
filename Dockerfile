FROM haskell:latest

RUN cabal update
COPY . .
RUN cabal install --installdir=.

CMD ["./suggbot"]
