Proyecto 2 - Lenguajes de Programación 2020                                                                                                                                                                             
Grupo #2
Integrantes:
DANIEL SANTIAGO SALGADO PINTO
JAIRO ALEJANDRO SIERRA FLORES
MIGUEL ANGEL FLORES RAMOS
RENATO DAVID LIZARDO VARELA

Para la parte grafica se necesita:
sudo apt-get
sudo apt-get install -y libgirepository1.0-dev libwebkit2gtk-4.0-dev libgtksourceview-3.0-dev
Para las dependencias se necesita:
cabal v2-update
cabal v2-configure --disable-optimization --write-ghc-environment-files=always -j2
cabal v2-build --only-dependencies
Para correr el build se necesita:
cabal v2-build
cabal v2-install all:exes --overwrite-policy=always
