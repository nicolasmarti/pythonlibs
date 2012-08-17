#!/bin/bash
mydir=`pwd`
currdir=`dirname $0`
finaldir="$mydir/$currdir"

echo "finaldir = $finaldir"

export PATH="$finaldir/ocaml/:$PATH"

export PATH="$finaldir/ocaml/_build/:$PATH"

export PYTHONPATH="$finaldir/misc/:$PYTHONPATH"
export PYTHONPATH="$finaldir/gui/:$PYTHONPATH"
export PYTHONPATH="$finaldir/finance/misc/:$PYTHONPATH"
export PYTHONPATH="$finaldir/finance/ibbroker/client/:$PYTHONPATH"
export PYTHONPATH="$finaldir/finance/ibbroker/server/:$PYTHONPATH"