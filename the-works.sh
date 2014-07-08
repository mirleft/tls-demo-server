#!/bin/bash

set -e

PAQ=(vim mosh tmux screen htop git opam ocaml-nox ocaml daemontools daemontools-run m4 zlib1g-dev libssl-dev libgmp-dev)
OPAQ=(mirage-clock-unix mirage-entropy-unix tcpip mirage-http yojson)

dd if=/dev/zero bs=1M count=2K of=/swap
mkswap /swap
swapon /swap

aptitude update
aptitude upgrade -y
aptitude -y install ${PAQ[@]}

yes|opam init
eval $(opam config env)
opam pin tls git://github.com/mirleft/ocaml-tls.git#demo-random

echo -e "\n* opam will now think for a little while..."
yes|opam install ssl mirage tls

echo -e "\n* opam will now consider more options..."
yes|opam install ${OPAQ[@]}
yes|opam install irmin

git clone https://github.com/mirleft/tls-demo-server.git
tar xvf tls-demo-server/daemontools-svc-dir.tar.xz -C .
tar xvf cert.tar.xz -C .

( cd tls-demo-server && mirage configure && make depend && mirage build )

update-service --add $(pwd)/demo-server-service
sleep 3

( cd tls-demo-server && ./deploy )

echo
echo -e '\033[32m' "yup." "\033[0m"

tail -f demo-server-service/log/main/current |tai64nlocal
