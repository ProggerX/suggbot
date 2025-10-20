# Dockerfile

FROM ubuntu:latest

WORKDIR /usr/share/bot

COPY . .

RUN apt update -y
RUN apt install curl -y
RUN curl --proto '=https' --tlsv1.2 -sSf -L https://install.determinate.systems/nix | sh -s -- install linux \
  --extra-conf "sandbox = false" \
  --init none \
  --no-confirm
ENV PATH="${PATH}:/nix/var/nix/profiles/default/bin"

RUN nix build .

CMD ["nix", "run", ".", "--impure"]
