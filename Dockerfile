FROM haskell:8.4.3

RUN mkdir -p /app/user
WORKDIR /app/user
COPY . /app/user
RUN export PATH=$(stack path --local-bin):$PATH
RUN stack install --no-nix
CMD pdp11-server
