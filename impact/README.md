## How-to
1. Be on Linux / Mac. You'll need
   [pcregrep](https://www.pcre.org/original/doc/html/pcregrep.html) and python3.
1. Modify `GHC_PATH` in `measure-impact.sh` to point to (an absolute path of)
   your GHC dir.
1. Running `./measure-impact.sh` will build `.raw` files with counts per file,
   indexed by type and library. `python3 tocsv.py` will aggregate these files
   into one `./report.csv`. Make sure to run this script from **this** directory.


## Counting total instances/classes/terms

Haskell is very nice to us in that all instance, class, and term
declarations must/should be at start of line. So grepping for totals
is fairly easy.

```bash
# instances
pcregrep --include "\.hs$" -r "^instance" /path/to/ghc/compiler/GHC | wc -l 
pcregrep --include "\.hs$" -r "^instance" /path/to/ghc/libraries/ | wc -l 

pcregrep --include "\.hs$" -r "^class" /path/to/ghc/compiler/GHC | wc -l 
pcregrep --include "\.hs$" -r "^class" /path/to/ghc/libraries/ | wc -l 

pcregrep --include "\.hs$" -r "^(\w+\s+::)" /path/to/ghc/compiler/GHC | wc -l 
pcregrep --include "\.hs$" -r "^(\w+\s+::)" /path/to/ghc/libraries/ | wc -l 
```
