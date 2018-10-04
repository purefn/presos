#!/usr/bin/env nix-shell
#! nix-shell -i bash -p scala_2_11

set -ue

PRESO="mtl.txt"

TMPFILE=$(mktemp)
trap "rm -f $TMPFILE; exit" INT TERM EXIT

echo "val replesent = REPLesent(input=\"$PRESO\", intp=\$intp);import replesent._;import scalaz._;import Scalaz._;h;" >> $TMPFILE

scala -Xplugin:kind-projector_2.11-0.7.1.jar -classpath scalaz-core_2.11-7.1.7.jar -Dscala.color -language:_ -nowarn -i REPLesent.scala -i $TMPFILE

