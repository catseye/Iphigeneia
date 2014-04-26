#!/bin/sh

if [ -z `which ghc` -a -z x`which runhugs` ]; then
    echo "Neither ghc nor runhugs found on search path."
    exit 1
fi

mkdir -p bin

if [ -z `which ghc` -o ! -z $USE_HUGS ]; then
    # create script to run with Hugs
    cat >bin/iphi <<'EOF'
#!/bin/sh
THIS=`realpath $0`
DIR=`dirname $THIS`/../src
runhugs $DIR/Main.hs $*
EOF
    chmod 755 bin/iphi
else
    cd src && ghc --make Main.hs -o ../bin/iphi
fi
