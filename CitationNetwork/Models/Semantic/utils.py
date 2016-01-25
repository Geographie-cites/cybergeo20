# -*- coding: utf-8 -*-

# utils Functions

import MySQLdb,sqlite3

# read a conf file under the format key:value
# , returns a dictionary
def read_conf(file):
    conf = open(file,'r')
    res=dict()
    currentLine = conf.readline().replace('\n','')
    while currentLine != '' :
        t=str.split(currentLine,':')
        if len(t) != 2 : raise Exception('error in conf file')
        res[t[0]]=t[1]
        currentLine = conf.readline().replace('\n','')
    return(res)

# return the mysql connection
def configure_sql():
    # conf mysql
    conf=read_conf('conf/mysql.conf')
    user = conf['user']
    password = conf['password']
    conn = MySQLdb.connect("localhost",user,password,"cybergeo",charset="utf8")
    return(conn)

# returns sqlite connection
def configure_sqlite(database):
    return(sqlite3.connect(database))



def get_data(query,source):
    if source=='mysql' :
        conn = configure_sql()
    else :
        conn = configure_sqlite(source)
    cursor = conn.cursor()
    cursor.execute(query)
    data=cursor.fetchall()
    return(data)

##
# usage : [ref_kw_dico,kw_ref_dico] = import_kw_dico()
def import_kw_dico(source):
    # import extracted keywords from database
    data = get_data('SELECT id,abstract_keywords FROM refdesc WHERE abstract_keywords IS NOT NULL;',source)

    ref_kw_dico = dict() # dictionnary refid -> keywords as list
    kw_ref_dico = dict() # dictionnary keywords -> refs as list

    for row in data :
        ref_id = row[0].encode('ascii','ignore')
        #print(ref_id)
        keywords_raw = row[1].encode('ascii','ignore').split(';')
        keywords = [keywords_raw[i] for i in range(len(keywords_raw)-1)]
        # pb with last delimiter in
        ref_kw_dico[ref_id] = keywords
        for kw in keywords :
            if kw not in kw_ref_dico : kw_ref_dico[kw] = []
            kw_ref_dico[kw].append(kw)

    return([ref_kw_dico,kw_ref_dico])


##
# corpus as (id,...)
def extract_sub_dicos(corpus,occurence_dicos) :
    ref_kw_dico_all = occurence_dicos[0]
    kw_ref_dico_all = occurence_dicos[1]

    ref_kw_dico = dict()
    kw_ref_dico = dict()

    for ref in corpus :
        ref_id = ref[0].encode('ascii','ignore')
        keywords = []
        if ref_id in ref_kw_dico_all :
            keywords = ref_kw_dico_all[ref_id]
            ref_kw_dico[ref_id] = keywords
            for k in keywords :
                if k not in kw_ref_dico : kw_ref_dico[k] = []
                kw_ref_dico[k].append(ref_id)

    return([ref_kw_dico,kw_ref_dico])
