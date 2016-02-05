
# kw extraction for cybergeo corpus alone
import utils,kwFunctions


def extract_relevant_cybergeo (kwLimit) :
    corpus = utils.get_data('SELECT cybergeo.id FROM refdesc INNER JOIN cybergeo ON cybergeo.id=refdesc.id WHERE abstract_keywords IS NOT NULL;','../../Data/dumps/20160204_cybergeo.sqlite3')
    print(corpus)
    occurence_dicos = utils.import_kw_dico_req('../../Data/dumps/20160204_cybergeo.sqlite3','SELECT cybergeo.id,abstract_keywords FROM refdesc INNER JOIN cybergeo ON cybergeo.id=refdesc.id WHERE abstract_keywords IS NOT NULL;')
    print(occurence_dicos)
    [relevantkw,relevant_dico] = kwFunctions.extract_relevant_keywords(corpus,kwLimit,occurence_dicos)
    utils.export_dico_csv(relevant_dico,'res/cybergeo/relevantDico_kwLimit'+str(kwLimit),False)
    utils.export_dico_num_csv(relevantkw,'res/cybergeo/kw_'+str(kwLimit),False)
