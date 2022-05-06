pcregrep --include "\.hs$" -Mcr "instance (\w|,|\(|\s|\n)*#if MIN_VERSION_base\(4,16,0\)(.|\n)+?#endif" ./ | grep -v ':0$' > ./impact/instances.impact_log;
pcregrep --include "\.hs$" -Mcr "class (\w|,|\(|\s|\n)*#if\s+MIN_VERSION_base\(4,16,0\)(.|\n)+?#endif" ./ | grep -v ':0$' > ./impact/classes.impact_log;
pcregrep --include "\.hs$" -Mcr "^(\w+\s+::)(\w|,|\(|\s|\n)*#if\s+MIN_VERSION_base\(4,16,0\)(.|\n)+?#endif" ./ | grep -v ':0$' > ./impact/top_level_terms.impact_log;
pcregrep --include "\.hs$" -Mcr "(\w+\s+::)(\w|,|\(|\s|\n)*#if\s+MIN_VERSION_base\(4,16,0\)(.|\n)+?#endif" ./ | grep -v  ':0$' > ./impact/all_terms.impact_log;

cd ./impact;
python tocsv.py;
