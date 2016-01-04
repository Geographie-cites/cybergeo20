import nltk,MySQLdb,time,locale


def run :
    run_kw_extraction()




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




def run_kw_extraction() :
    start = time.time()
    conf=read_conf('conf/mysql.conf')
    user = conf['user']
    password = conf['password']
    while True :
        # get data from mysql
        conn = MySQLdb.connect("localhost",user,password,"cybergeo")
        cursor = conn.cursor()
        cursor.execute('SELECT id,abstract FROM refdesc WHERE abstract_keywords IS NULL;')
        data=cursor.fetchall()

        for ref in data:
            keywords = extract_keywords(ref[0],ref[1])
            # insert
            cursor.execute("INSERT INTO refdesc (abstract_keywords) VALUES (\'"+reduce(lambda s1,s2 : s1+';'+s2,keywords)+"\') ON DUPLICATE KEY UPDATE abstract_keywords = VALUES(abstract_keywords);")
            conn.commit()

        conn.close()


def potential_multi_term(tagged) :
    res = True
    for tag in tagged :
        res = res and (tag[1]=='NN' or tag[1]=='NNP' or tag[1] == 'VBG' or tag[1] =='NNS'or tag[1] =='JJ' or tag[1] =='JJR')
    return res


def extract_keywords(raw_text,id):

    print("Extracting keywords for "+id)

    stemmer = nltk.PorterStemmer()

    # Construct text

    # Tokens
    tokens = nltk.word_tokenize(raw_text)
    # filter undesirable words and format
    words = [w.replace('\'','') for w in tokens if len(w)>=3]
    text = nltk.Text(words)

    tagged_text = nltk.pos_tag(text)
    #nouns = [tg[0] for tg in tagged_text if tg[1]=='NN' or tg[1]=='NNP' ]
    #print(nouns)

    # multi-term
    multiterms = []
    for i in range(len(tagged_text)) :
        # max length 4 for multi-terms
        for l in range(1,5) :
            if i+l < len(tagged_text) :
                tags = [tagged_text[k] for k in range(i,i+l)]
                if potential_multi_term(tags) :
                    multistem = [str.lower(stemmer.stem(tagged_text[k][0]).encode('ascii','ignore')) for k in range(i,i+l)]
                    multistem.sort(key=str.lower)
                    multiterms.append(multistem)

    return multiterms



def main():

    # import utils
    # execfile('../Utils/utils.py')

    start = time.time()

    run()

    print('Ellapsed Time : '+str(time.time() - start))


main()
