#!/usr/bin/env bash

set -ue

PRESO="mtl.txt"

TMPFILE=$(mktemp --suffix=$PRESO)
trap "rm -f $TMPFILE; exit" INT TERM EXIT

echo "val replesent = REPLesent(input=\"$PRESO\", intp=\$intp);import replesent._;import scalaz._;import Scalaz._;h;" >> $TMPFILE

scala -Xplugin:kind-projector_2.11-0.7.1.jar -classpath scalaz-core_2.11-7.1.7.jar -Dscala.color -language:_ -nowarn -i REPLesent.scala -i $TMPFILE

