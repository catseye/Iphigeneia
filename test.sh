#!/bin/sh

cd src && make iphi && cd ..
falderal test tests/Iphigeneia.markdown && echo "All tests passed!"
rm -f foo.txt
#cd src && make clean && cd ..
