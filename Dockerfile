FROM haskell:latest

RUN cabal update;
RUN cabal install --installdir=.;

CMD ["./suggbot"]
