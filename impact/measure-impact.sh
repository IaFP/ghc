#!/bin/bash

## EDIT ME!
GHC_PATH="~/PTC/ghc"

# keywords and patterns
ANY="(\w|,|\(|\s|\n|\#|\)|\@)"
INSTANCE="^instance"
CLASS="^class"
TERM="^(\w+\s+::)"
AT=" \@ "
TOTAL="Total"
WDT="WDT"

function measure2 () {
  PATTERN="$2$ANY+?$3"
  pcregrep --include "\.hs$" -Mrc "$PATTERN" "GHC_PATH/$1" | grep -v ":0" > "$4"
}

## --------------------------------------------------------------------------------
## Counting annotations in compiler/GHC/
## --------------------------------------------------------------------------------

measure2 compiler/GHC "$INSTANCE" "$WDT" ./compiler/instances/wdt.raw
measure2 compiler/GHC "$INSTANCE" "$TOTAL" ./compiler/instances/total.raw
measure2 compiler/GHC "$INSTANCE" "$AT" ./compiler/instances/at.raw

measure2 compiler/GHC "$CLASS" "$WDT" ./compiler/classes/wdt.raw
measure2 compiler/GHC "$CLASS" "$TOTAL" ./compiler/classes/total.raw
measure2 compiler/GHC "$CLASS" "$AT" ./compiler/classes/at.raw

measure2 compiler/GHC "$TERM" "$WDT" ./compiler/terms/wdt.raw
measure2 compiler/GHC "$TERM" "$TOTAL" ./compiler/terms/total.raw
measure2 compiler/GHC "$TERM" "$AT" ./compiler/terms/at.raw

## --------------------------------------------------------------------------------
## Count annotations in libraries/
## --------------------------------------------------------------------------------


measure2 libraries "$INSTANCE" "$WDT" ./libraries/instances/wdt.raw
measure2 libraries "$INSTANCE" "$TOTAL" ./libraries/instances/total.raw
measure2 libraries "$INSTANCE" "$AT" ./libraries/instances/at.raw

measure2 libraries "$CLASS" "$WDT" ./libraries/classes/wdt.raw
measure2 libraries "$CLASS" "$TOTAL" ./libraries/classes/total.raw
measure2 libraries "$CLASS" "$AT" ./libraries/classes/at.raw

measure2 libraries "$TERM" "$WDT" ./libraries/terms/wdt.raw
measure2 libraries "$TERM" "$TOTAL" ./libraries/terms/total.raw
measure2 libraries "$TERM" "$AT" ./libraries/terms/at.raw


## --------------------------------------------------------------------------------
