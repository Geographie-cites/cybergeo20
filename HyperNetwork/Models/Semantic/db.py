# db script
import utils



# dirty for purpose
def read_csv(file):
    conf = open(file,'r')
    res=dict()
    currentLine = conf.readline().replace('\n','')
    while currentLine != '' :
        t=str.split(currentLine,',')
        res[t[1].replace("\"","")]=t[0]
        currentLine = conf.readline().replace('\n','')
    return(res)

def readfile(file):
    f = open(file,'r')
    res=""
    currentLine = conf.readline().replace('\n',' ')
    while currentLine != '' :
        res = res+currentLine
        currentLine = conf.readline().replace('\n',' ')
    return(res)

def update_cyb_abstracts():
    # read csv for correspondance id,schid
    ids = read_csv('../../../Data/raw/cybergeo.csv')

    for i in ids.keys():
        abstract=readfile('../../../Data/raw/texts/'+i+'_abstract.txt').replace('\'','')
        query = 'UPDATE refdesc SET abstract=\''+abstract+'\' WHERE id=\''+ids[i]+'\';'
        print(query)
        # query_mysql(query)


update_cyb_abstracts()
