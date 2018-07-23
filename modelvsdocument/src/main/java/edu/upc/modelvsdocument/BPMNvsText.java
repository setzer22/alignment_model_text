package edu.upc.modelvsdocument;

import clojure.java.api.Clojure;
import clojure.lang.IFn;

import java.io.File;
import java.io.FileNotFoundException;
import java.util.Scanner;
import java.util.List;

public class BPMNvsText
{
    public static Result compare(String bpmn_path, String text, String credentials_path) {
        IFn set_creds_path = Clojure.var("edu.upc.modelvsdocument.config", "set-creds-path!");
        set_creds_path.invoke(credentials_path);
        IFn main = Clojure.var("edu.upc.modelvsdocument.core", "main");
        return (Result) main.invoke(bpmn_path, text);
    }

    public static void executeOptimizer(String config_path, String credentials_path) {
        require("edu.upc.modelvsdocument.repl.weight-optimization");
        require("edu.upc.modelvsdocument.repl.comparison-queries");
        IFn set_creds_path = Clojure.var("edu.upc.modelvsdocument.config", "set-creds-path!");
        set_creds_path.invoke(credentials_path);
        IFn main_optimize = Clojure.var("edu.upc.modelvsdocument.repl.weight-optimization", "main-optimize");
        System.out.println("config_path="+config_path);
        main_optimize.invoke(config_path);
    }

    public static void setValueInConfig(String key, Object value) {
        IFn set_value_in_config = Clojure.var("edu.upc.modelvsdocument.config", "set-value-in-config");
        set_value_in_config.invoke(key, value);
    }

    public static Object getValueInConfig(String key) {
        IFn get_value_in_config = Clojure.var("edu.upc.modelvsdocument.config", "get-value-in-config");
        return get_value_in_config.invoke(key);
    }

    public static void readDictionaries(String wordnet_path) {
        IFn read_dictionaries = Clojure.var("edu.upc.modelvsdocument.wordnet", "read-wordnet-dictionaries");
        read_dictionaries.invoke(wordnet_path);
    }

    public static void loadConfigFromFile(String path) {
        IFn load_config_from_file = Clojure.var("edu.upc.modelvsdocument.config", "load-config-from-file");
        load_config_from_file.invoke(path);
    }

    public static void require (String namespace) {
		IFn require = Clojure.var("clojure.core", "require");
		require.invoke(Clojure.read(namespace));
    }

    public static void load () {
        require("edu.upc.modelvsdocument.core");
        require("edu.upc.modelvsdocument.config");
        require("edu.upc.modelvsdocument.wordnet");
    }

    public static void main( String[] args ) throws FileNotFoundException {
        System.out.println("BPMN vs TEXT");

        // Millor cridar-lo al principi
        load();

        executeOptimizer(args[0], args[1]);

        /*
        // Definicio dels path
        String bpmn_path = "/home/josep/Repositories/NLP4BPM_project/modelvsdocument/benchmarks/mini-benchmark/Dispatch-of-goods.bpmn";
        String text_path = "/home/josep/Repositories/NLP4BPM_project/modelvsdocument/benchmarks/mini-benchmark/Dispatch-of-goods.txt";
        String text = new Scanner(new File(text_path)).useDelimiter("\\Z").next();
        String credentials_path = "/home/josep/.textserver-credentials";

        // Carregar fitxer de configuració. Cal passar un path amb el fitxer de configuració
        // Si no es carrega cap fitxer es queden els valors per defecte.
        //loadConfigFromFile("/home/josep/.bpmnvstext.config");

        // Modificar valors individuals
        setValueInConfig("hiperonimy-chain-length", 2);
        setValueInConfig("similarity-function", "jaccard");

        // IMPORTANT: Llegir els diccionaris de wordnet abans d'executar l'algoritme per primer cop.
        //            Cal passar un path amb els diccionaris de wordnet.
        readDictionaries("/home/josep/Repositories/NLP4BPM_project/modelvsdocument/wordnet/");

        // Cridem a l'algoritme
        Result res = compare(bpmn_path, text, credentials_path);

        // Recorrem els camps del resultat
        //System.out.println(res.getLog()); // El log, amb tota la informació detallada (no cal mostrar-lo).

        // La puntuació de similaritat, s'ha de mostrar en algun lloc en gran.
        System.out.println(res.getTotalScore());

        // La informació per cadascun dels match
        System.out.println(res.getMatches());
        for(Match s : res.getMatches()) {
            System.out.println("Match:");
            String id = s.getTaskId(); // Id de la tasca al BPMN
            System.out.println(id);
            int idx = s.getSentenceIndex(); // Index de la frase a la llista de frases
            System.out.println(idx);
            String sentence = res.getSentences().get(idx); // Text de la frase.
            System.out.println(sentence);
            System.out.println("Task Name:");
            String task_text = s.getTaskName();
            System.out.println(task_text);
            boolean good = s.isGood(); // Si el match no és bo, no cal mostrar-lo. Tampoc s'hauria de colorejar.
            List<String> common = s.getCommonFeatures(); // Features comunes (text a imprimir) de la parella frase-tasca
            System.out.println(common);
        }

        // Obtenir l'informació d'un match concret a partir de l'índex de la frase i els ids del text.
        int sid = res.getMatches().get(0).getSentenceIndex();
        String tid = res.getMatches().get(0).getTaskId();
        res.getMatch(sid,tid);

        System.out.println(res.getTasksOfSentence());
        for(SentenceIdsPair s : res.getTasksOfSentence()) {
            System.out.println("Sentence ids pair");
            int idx = s.getSentenceIndex(); // Index de la frase
            System.out.println(idx);
            List<String> taskIds = s.getTaskIds(); // Llista de Ids que tenen el mateix color que la frase
            System.out.println(taskIds);
        }
        System.out.println("DONE");
        System.exit(0);
        */

        System.out.println("DONE");
        System.exit(0);
    }
}
