pcregrep --include "\.hs$" -Mrc "^instance(\w|,|\(|\s|\n)*#if\s+MIN_VERSION_base\(4,16,0\)(.|\n)+?#endif" ./ > ./impact/instances.impact_log;
pcregrep --include "\.hs$" -Mrc "^class(\w|,|\(|\s|\n)*#if\s+MIN_VERSION_base\(4,16,0\)(.|\n)+?#endif" ./ > ./impact/classes.impact_log;
pcregrep --include "\.hs$" -Mrc "^(\w+\s+::)(\w|,|\(|\s|\n)*#if\s+MIN_VERSION_base\(4,16,0\)(.|\n)+?#endif" ./ > ./impact/top_level_terms.impact_log;
pcregrep --include "\.hs$" -Mrc "(\w+\s+::)(\w|,|\(|\s|\n)*#if\s+MIN_VERSION_base\(4,16,0\)(.|\n)+?#endif" ./ > ./impact/all_terms.impact_log;
