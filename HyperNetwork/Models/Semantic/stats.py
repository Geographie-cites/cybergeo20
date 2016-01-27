# -*- coding: utf-8 -*-
import utils

# export from base for stats


def export_ref_info():
    data = get_data('SELECT id,year,language FROM refdesc;','mysql')
    export_matrix_csv(data,'stats/ref_info',False)



def export_matrix_csv(m,fileprefix,withDate):
    datestr = ''
    if withDate : datestr = str(datetime.datetime.now())
    outfile=open(fileprefix+datestr+'.csv','w')
    for r in m :
        for c in range(len(r)) :
            outfile.write(r[c])
            if c < len(r)-1 : outfile.write(";")
        outfile.write(k+'\n')
