package edu.upc.modelvsdocument;

//import edu.upc.freeling.*;
import org.activiti.bpmn.model.*;
import org.activiti.bpmn.model.Process;

import clojure.java.api.Clojure;
import clojure.lang.IFn;

public class App 
{

    /*public static String base_path = "/home/josep/BPMN/";
    public static String bpmn_path = "model.xml";
    public static String text_path = "model.txt";
*/
    public static void main( String[] args ) throws Exception {
		IFn require = Clojure.var("clojure.core", "require");
		require.invoke(Clojure.read("edu.upc.modelvsdocument.clojure"));
        IFn do_stuff = Clojure.var("edu.upc.modelvsdocument.clojure", "do-stuff");
        do_stuff.invoke();
    }
	
	


}
