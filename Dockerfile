FROM haskell:8.4.3

RUN mkdir -p /app/user
WORKDIR /app/user
COPY stack.yaml *.cabal ./

RUN export PATH=$(stack path --local-bin):$PATH
RUN stack build --dependencies-only --no-nix

COPY . /app/user
RUN stack install --no-nix
