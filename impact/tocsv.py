import csv

def parse_line(p):
    line = p.replace("/home/alex/PTC/ghc/", "")    
    full_path, count = line.split(":")
    return (full_path, int(count))

with open("./report.csv", "w") as report:
    writer = csv.DictWriter(report, fieldnames=['full_path', 'package', 'module', 'type', 'ats', 'totals', 'WDTs'])
    writer.writeheader()

    interm = {}
    
    for d in ['compiler','libraries']:
        for f in ['classes', 'instances', 'terms']:
            # Multiline-with statements made available in python 3.10. U+1F643 upsidedownsmile U+1F643
            with open("./{}/{}/at.raw".format(d,f), "r") as ats, open("./{}/{}/total.raw".format(d,f), "r") as totals, open("./{}/{}/wdt.raw".format(d,f), "r") as wdts:
                for name, log in {"ats": ats, "totals": totals, "WDTs": wdts}.items():
                    lines = log.readlines()
                    for line in lines:
                        full_path, count = parse_line(line)
                        
                        # Inferwence
                        slugs   = full_path.split("/")
                        package = slugs[1]
                        module  = slugs[-1]

                        if not full_path in interm:
                            counts = {
                                "ats": 0,
                                "totals": 0,
                                "WDTs"  : 0
                                }
                            
                            interm[full_path] = {
                                'full_path': full_path,
                                'package'  : package,
                                'module'   : module,
                                'classes'  : {
                                    "ats": 0,
                                    "totals": 0,
                                    "WDTs"  : 0
                                },
                                'instances': {
                                    "ats": 0,
                                    "totals": 0,
                                    "WDTs"  : 0
                                },
                                'terms'    : {
                                    "ats": 0,
                                    "totals": 0,
                                    "WDTs"  : 0
                                }
                            }
                        
                        interm[full_path][f][name] = count

    for _, fp in interm.items():
        for ugh in ['classes', 'instances', 'terms']:
            row = {
                'full_path': fp['full_path'],
                'package'  : fp['package'],
                'module'   : fp['module'],
                'type'     : ugh,
                'ats'      : fp[ugh]['ats'],
                'totals'   : fp[ugh]['totals'],
                'WDTs'     : fp[ugh]['WDTs'],
            }
            writer.writerow(row)
