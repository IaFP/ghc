#!/bin/bash

ANY="(\w|,|\(|\s|\n|\#|\))"
INSTANCE="^instance"
CLASS="^class"
TERM="^(\w+\s+::)"

function measure () {
  PATTERN="$2$ANY+#if MIN_VERSION_base\(4,16,0\)(.|\n)+?#endif"
  echo $PATTERN;
  pcregrep --include "\.hs$" -Mr "$PATTERN" ~/PTC/ghc/$1 > "$3"
}

AT=" \@ "
TOTAL="Total"
WDT="WDT"

function measure2 () {
  PATTERN="$2$ANY+?$3"
  echo $PATTERN;
  pcregrep --include "\.hs$" -Mrc "$PATTERN" ~/PTC/ghc/$1 | grep -v ":0" > "$4"
}

## --------------------------------------------------------------------------------
## TODO: this should be abstracted.
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


# measure2 compiler/GHC/ "$INSTANCE" ./compiler/instances.raw
# measure2 compiler/GHC/ "$CLASS"    ./compiler/classes.raw

# pcregrep --include "\.hs$" -Mcr "instance (\w|,|\(|\s|\n)*#if MIN_VERSION_base\(4,16,0\)(.|\n)+?#endif" ./ | grep -v ':0$' > ./impact/instances.impact_log;
# pcregrep --include "\.hs$" -Mcr "class (\w|,|\(|\s|\n)*#if\s+MIN_VERSION_base\(4,16,0\)(.|\n)+?#endif" ./ | grep -v ':0$' > ./impact/classes.impact_log;
# pcregrep --include "\.hs$" -Mcr "^(\w+\s+::)(\w|,|\(|\s|\n)*#if\s+MIN_VERSION_base\(4,16,0\)(.|\n)+?#endif" ./ | grep -v ':0$' > ./impact/top_level_terms.impact_log;
# pcregrep --include "\.hs$" -Mcr "(\w+\s+::)(\w|,|\(|\s|\n)*#if\s+MIN_VERSION_base\(4,16,0\)(.|\n)+?#endif" ./ | grep -v  ':0$' > ./impact/all_terms.impact_log;
# 
# cd ./impact;
# python tocsv.py;
# 
