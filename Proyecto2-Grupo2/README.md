Proyecto 2 - Lenguajes de Programación 2020<br />                                                                                                                                                                        
Grupo #2<br />
Integrantes:<br />
DANIEL SANTIAGO SALGADO PINTO<br />
JAIRO ALEJANDRO SIERRA FLORES<br />
MIGUEL ANGEL FLORES RAMOS<br />
RENATO DAVID LIZARDO VARELA<br />

Para la parte grafica se necesita:<br />
sudo apt-get<br />
sudo apt-get install -y libgirepository1.0-dev libwebkit2gtk-4.0-dev libgtksourceview-3.0-dev<br />
Para las dependencias se necesita:<br />
cabal v2-update<br />
cabal v2-configure --disable-optimization --write-ghc-environment-files=always -j2<br />
cabal v2-build --only-dependencies<br />
Para correr el build se necesita:<br />
cabal v2-build<br />
cabal v2-install all:exes --overwrite-policy=always<br />
