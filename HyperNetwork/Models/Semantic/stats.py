# -*- coding: utf-8 -*-
import utils

# export from base for stats


def export_ref_info():
    data = utils.get_data('SELECT refs.id,refs.year,language FROM refdesc JOIN refs ON refs.id=refdesc.id;','mysql')
    export_matrix_csv(data,'stats/ref_info.csv',False)



def export_matrix_csv(m,fileprefix,withDate):
    datestr = ''
    if withDate : datestr = str(datetime.datetime.now())
    outfile=open(fileprefix+datestr+'.csv','w')
    for r in m :
        for c in range(len(r)) :
            outfile.write(str(r[c]))
            if c < len(r)-1 : outfile.write(";")
        outfile.write(k+'\n')