# -*- coding: utf-8 -*-

# db management

import utils
import pymongo


def sqlite_to_mongo(sqlitedb,mongodb):
    client=pymongo.MongoClient()
    db=client[mongodb]
    relevant = utils.get_data('SELECT * FROM relevant;',sqlitedb)
    col=db['relevant']
    for row in relevant:
        col.insert_one({'keyword':row[0],'cumtermhood':row[1],'ids':row[2].split(';')})
    # add index for query efficiency
    col.create_index('keyword')
    dico = utils.get_data('SELECT * FROM dico;',sqlitedb)
    col=db['dico']
    for row in dico:
        col.insert_one({'id':row[0],'keywords':row[1].split(';')})
    col.create_index('id')

sqlite_to_mongo('bootstrap/run_kw1000_csize5000_b20/bootstrap.sqlite3','cyb_kw1000_csize5000_b20')
