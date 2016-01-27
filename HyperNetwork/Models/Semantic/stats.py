# -*- coding: utf-8 -*-
import utils

# export from base for stats


def export_ref_info():
    data = utils.get_data('SELECT refs.id,refs.year,language FROM refdesc INNER JOIN refs ON refs.id=refdesc.id;','mysql')
    #for r in data : print(r)
    export_matrix_csv(data,'stats/ref_info',False)


##
#  export infos for refs whose od is obtain from a primary request (parameter)
def export_secondaryref_info(request,outfile):
    ids = utils.get_data(request,'mysql')
    res=[]
    # iterate on ids - slow ?
    for i in ids :
        print(i[0])
        ref = utils.get_data('SELECT refdesc.id,year,language,keywords FROM refdesc INNER JOIN refs ON refs.id=refdesc.id WHERE refdesc.id='+i[0]+';','mysql')
        res.append(ref[0])
    export_matrix_csv(data,outfile,False)


export_secondaryref_info('SELECT citing FROM links INNER JOIN cybergeo on cybergeo.id=links.cited;','stats/citing_info')
export_secondaryref_info('SELECT cited FROM links INNER JOIN cybergeo on cybergeo.id=links.citing;','stats/cited_info')


def export_matrix_csv(m,fileprefix,withDate):
    datestr = ''
    if withDate : datestr = str(datetime.datetime.now())
    outfile=open(fileprefix+datestr+'.csv','w')
    for r in m :
        #print(len(r))
        for c in range(len(r)) :
            print(str(r[c]))
	    outfile.write(str(r[c]))
            if c < len(r)-1 : outfile.write(";")
        outfile.write('\n')
