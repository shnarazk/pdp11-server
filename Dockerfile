FROM haskell:8.4.3

RUN mkdir -p /app/user
RUN mkdir -p /app/user/PDP11
WORKDIR /app/user
COPY PDP11 ./PDP11
COPY stack.yaml pdp11-server.cabal ./
RUN export PATH=$(stack path --local-bin --no-nix):$PATH
RUN stack build --dependencies-only --no-nix

COPY . /app/user
RUN stack install --no-nix

CMD pdp11-server
