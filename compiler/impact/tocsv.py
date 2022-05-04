import csv

for f in ['all_terms', 'classes', 'instances', 'top_level_terms']:
    with open("./{}.impact_log".format(f), "r") as log, open("./{}.csv".format(f), "w") as outCsv:
        lines = log.readlines()
        writer = csv.DictWriter(outCsv, fieldnames=['full_path', 'package', 'module',  'count'])
        writer.writeheader()
        
        for line in lines:
            line = line.replace(".//","")
            full_path, count = line.split(":")
            # coerce to int for better formatting
            count = int(count)

            # Infer package + module
            slugs   = full_path.split("/")
            package = slugs[0]
            module  = slugs[-1]
            row     = {
                'full_path': full_path,
                'count'    : count,
                'package'  : package,
                'module'   : module
            }
            
            writer.writerow(row)
