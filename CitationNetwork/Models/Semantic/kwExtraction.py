# -*- coding: utf-8 -*-
import nltk,MySQLdb,time,locale,sys
from treetagger import TreeTagger


def run():
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
        conn = MySQLdb.connect("localhost",user,password,"cybergeo",charset="utf8")
        cursor = conn.cursor()
        cursor.execute('SELECT id,abstract FROM refdesc WHERE abstract_keywords IS NULL;')
        data=cursor.fetchall()

        for ref in data:
            language = get_language(ref[1])
	    keywords = extract_keywords(ref[1],ref[0],language)
            #print(language)
	    #print(keywords)
	    # insert
	    #kwtext = reduce(lambda l1,l2 : reduce(lambda s1,s2 : s1+' '+s2,l1)+';'+reduce(lambda s1,s2 : s1+' '+s2,l2),keywords)
            kwtext = ""
	    for multistem in keywords:
		kwtext=kwtext+reduce(lambda s1,s2 : s1+' '+s2,multistem)+";"
	    print(kwtext)
	    cursor.execute("INSERT INTO refdesc (id,language,abstract_keywords) VALUES (\'"+ref[0].encode('utf8')+"\',\'"+language.encode('utf8')+"\',\'"+kwtext+"\') ON DUPLICATE KEY UPDATE language = VALUES(language),abstract_keywords=VALUES(abstract_keywords);")
            conn.commit()

        conn.close()


def potential_multi_term(tagged,language):
    res = True
    for tag in tagged :
	if len(tag)>=2 :
            if language=='english':
	        res = res and (tag[1]=="NN" or tag[1]=="NNP" or tag[1] == "VBG" or tag[1] =="NNS" or tag[1] =="JJ" or tag[1] =="JJR")
            else:
                if language=='french' :
	            res = res and (tag[1]=='NOM' or tag[1]=='ADJ') and len(tag[0]) >= 3 #and tag[2]!="<unknown>"
                else :
	            res = False
	else :
	    res=False
    return res


STOPWORDS_DICT = dict()
for lang in nltk.corpus.stopwords.fileids():
    STOPWORDS_DICT[lang] =  set(nltk.corpus.stopwords.words(lang))

def get_language(text):
    words = set(nltk.wordpunct_tokenize(text.lower()))
    return max(((lang, len(words & stopwords)) for lang, stopwords in STOPWORDS_DICT.items()), key = lambda x: x[1])[0]


def extract_keywords(raw_text,id,language):

    print("Extracting keywords for "+id)
  #  print(raw_text)
 #   print(get_language(raw_text))

    stemmer = nltk.PorterStemmer()

    # Construct text


    # Tokens
    
    if language == 'english': 
        tokens = nltk.word_tokenize(raw_text)
    # filter undesirable words and format
        words = [w.replace('\'','') for w in tokens if len(w)>=3]
        text = nltk.Text(words)

        tagged_text = nltk.pos_tag(text)
    
    else:
       tt = TreeTagger(encoding='utf-8',language='french')
       tagged_text =tt.tag(raw_text.replace('\'',' ').replace(u'\u2019',' ').replace(u'\xab',' ').replace(u'\xbb',' '))

    print(tagged_text)
    
    # detect language using stop words, adapt filtering/stemming technique in function

    #nouns = [tg[0] for tg in tagged_text if tg[1]=='NN' or tg[1]=='NNP' ]
    #print(nouns)

    # multi-term
    multiterms = []
    for i in range(len(tagged_text)) :
  #      # max length 4 for multi-terms
        for l in range(1,5) :
            if i+l < len(tagged_text) :
                tags = [tagged_text[k] for k in range(i,i+l)]
                #if language == 'english':
                #        print(tags)
		#	print(potential_multi_term(tags,language))
		if potential_multi_term(tags,language) :
                    multistem = [] 
		    if language == 'english':
			#print(tags)
			#for k in range(i,i+l):
		        #    print(tagged_text[k][0])
			#    print(stemmer.stem(tagged_text[k][0]))
			#    print(stemmer.stem(tagged_text[k][0]).encode('ascii','ignore'))

			multistem = [str.lower(stemmer.stem(tagged_text[k][0]).encode('utf8','ignore')) for k in range(i,i+l)]
                    else :#in case of french or other language, terms are already stemmed by TreeTagger
			multistem=[]
			for k in range(i,i+l):
			    if tagged_text[k][2]!="<unknown>":
			        stem = tagged_text[k][2]
			    else :
			        stem = tagged_text[k][0]
			    multistem.append(str.lower(stem.encode('utf8','ignore')))
		    #multistem.sort(key=str.lower)
                    multiterms.append(multistem)

    return multiterms



def main():

    # import utils
    # execfile('../Utils/utils.py')

    #sys.setdefaultencoding('utf8')
    # deprecated in python 2.6

    start = time.time()

    run()

    print('Ellapsed Time : '+str(time.time() - start))


main()
