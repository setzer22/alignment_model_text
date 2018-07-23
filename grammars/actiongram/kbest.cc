
#include <iostream>

#include "freeling.h"
#include "freeling/morfo/traces.h"

using namespace std;
using namespace freeling;

// predeclarations
void ProcessResults(const list<sentence> &);
void PrintMorfo(const sentence &, int);
void PrintTree(const sentence &, int);

int KBEST;

// analyzers
tokenizer *tk;
splitter *sp;
maco *morfo;
hmm_tagger *tagger;
chart_parser *parser;

/////////   MAIN SAMPLE PROGRAM  -- begin

int main (int argc, char **argv) {

  /// set locale to an UTF8 comaptible locale
  util::init_locale(L"default");

  if (argc < 2) {
    wcerr << L"Usage " << argv[0] << L" nbest [FL-install-path]" << endl;
    exit(1);
  }
  // number of kbest PoS tag sequences to consider
  KBEST = std::stoi(argv[1]);
  // FreeLing installation path, to locate default data files.
  wstring ipath;
  if (argc < 3) ipath=L"/usr/local";
  else ipath=util::string2wstring(argv[2]);

  wstring path=ipath+L"/share/freeling/en/";

  // if FreeLing was compiled with --enable-traces, you can activate
  // the required trace verbosity for the desired modules.
  //traces::TraceLevel=4;
  //traces::TraceModule=CHART_TRACE|GRAMMAR_TRACE;
  
  // create analyzers
  tk = new tokenizer(path+L"tokenizer.dat"); 
  sp = new splitter(path+L"splitter.dat");
  
  // morphological analysis has a lot of options, and for simplicity they are packed up
  // in a maco_options object. First, create the maco_options object with default values.
  maco_options opt(L"es");  
  // and provide files for morphological submodules. Note that it is not necessary
  // to set opt.QuantitiesFile, since Quantities module was deactivated.
  opt.UserMapFile=L"";
  opt.LocutionsFile=path+L"locucions.dat"; opt.AffixFile=path+L"afixos.dat";
  opt.ProbabilityFile=path+L"probabilitats.dat"; opt.DictionaryFile=path+L"dicc.src";
  opt.NPdataFile=path+L"np.dat"; opt.PunctuationFile=path+L"../common/punct.dat";
  opt.CompoundFile=path+L"compounds.dat"; opt.QuantitiesFile=path+L"quantities.dat";

  // create the analyzer with the just build set of maco_options
  morfo = new maco(opt); 
  // then, set required options on/off  
  morfo->set_active_options (false,// UserMap
                             true, // NumbersDetection,
                             true, //  PunctuationDetection,
                             true, //  DatesDetection,
                             true, //  DictionarySearch,
                             true, //  AffixAnalysis,
                             true, //  CompoundAnalysis,
                             true, //  RetokContractions,
                             true, //  MultiwordsDetection,
                             true, //  NERecognition,
                             true, //  QuantitiesDetection,
                             true);  //  ProbabilityAssignment

  // create a hmm tagger for spanish (with retokenization ability, forced 
  // to choose only one tag per word, producing KBEST best sequences)
  tagger = new hmm_tagger(path+L"tagger.dat", true, FORCE_TAGGER, KBEST); 
  // create chunker
  parser = new chart_parser(L"./actions.gram");

  // get plain text input lines while not EOF.
  wstring text;
  int n=1;
  while (getline(wcin,text)) {
    // remove final dot, if any (the grammar doesn't expect it)
    int p;
    for (p=text.length()-1; text[p]==' ' or text[p]=='.'; --p);
    if (p < text.length()-1) text = text.substr(0,p+1);
    
    // tokenize input line into a list of words
    list<word> lw = tk->tokenize(text);
    // lowercase the capitalized words in the sentence (except acronyms)
    for (list<word>::iterator w=lw.begin(); w!=lw.end(); ++w)
      if (util::capitalization(w->get_form())==UPPER_1ST)
        w->set_form(util::lowercase(w->get_form()));
    // split sentences in text (probably unneeded)
    list<sentence> ls = sp->split(lw);
    
    // Analyze sentences 
    morfo->analyze(ls);   // morphological analysis
    tagger->analyze(ls);  // PoS tagging 
    parser->analyze(ls);  // shallow parser (chunker)

    wcout << L"======= Line " << n++ << L" ========" << endl;
    ProcessResults(ls);

    // clear temporary lists;
    lw.clear(); ls.clear();    
  }
  
  delete tk;
  delete sp;
  delete morfo;
  delete tagger;
  delete parser;
}

/////////   MAIN SAMPLE PROGRAM  -- end


//----------------------------------
/// Result processing functions
//----------------------------------

void ProcessResults(const list<sentence> &ls) {

  for (list<sentence>::const_iterator is=ls.begin(); is!=ls.end(); ++is) {

    bool found = false;
    // for each of the k best sequences proposed by the tagger
    for (int k=0; k<is->num_kbest() and not found; k++) {
      wstring lab = is->get_parse_tree(k).begin()->get_label();
      if (lab == L"action" or lab == L"noun-action" or lab == L"double-action") {
        wcout << L"Found match at best sequence #" << k;
        wcout << L" [";
        for (sentence::const_iterator w=is->begin(); w!=is->end(); ++w) 
          wcout << L" " << w->get_form() << L"/" << w->get_tag(k);
        wcout << L" ]" << endl;
        found = true;
        PrintTree(*is,k);
      }
    }

    if (not found) {
        wcout<<L"No match found. Available trees were:"<<endl;
        for (int k=0; k<is->num_kbest(); k++) {
          wcout << L"------- Tree #" << k;
          wcout << L" [";
          for (sentence::const_iterator w=is->begin(); w!=is->end(); ++w) 
            wcout << L" " << w->get_form() << L"/" << w->get_tag(k);
          wcout << L" ]" << endl;
          PrintTree(*is,k);
        }
    }
    wcout <<endl;    
  }

}


//---------------------------------------------
// print syntactic tree
//---------------------------------------------

void rec_PrintTree(parse_tree::const_iterator n, int k, int depth) {
  parse_tree::const_sibling_iterator d;
  
  wcout<<wstring(depth*3,' '); 
  if (n.num_children()==0) { 
    const word & w=n->get_word();
    wcout<<(n->is_head()? L"+" : L"")<< L"("<<w.get_form()<<L" "<<w.get_lemma(k)<<L" "<<w.get_tag(k)<<")"<<endl;
  }
  else { 
    wcout<<(n->is_head()? L"+" : L"")<<n->get_label()<<L"_["<<endl;
    for (d=n.sibling_begin(); d!=n.sibling_end(); d++)
      rec_PrintTree(d, k, depth+1);

    wcout<<wstring(depth*3,' ')<<L"]"<<endl;
  }
}

// - - - - - - - - - - - - - - - - - - - - - - -
void PrintTree(const sentence &s, int k) {
  rec_PrintTree(s.get_parse_tree(k).begin(), k, 0);
}



