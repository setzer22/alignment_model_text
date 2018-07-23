package edu.upc.nlp4bpm_commons;

import java.util.List;
import java.util.Map;
import java.util.HashMap;

import clojure.java.api.Clojure;
import clojure.lang.IFn;
import clojure.lang.Keyword;

public class ClojureRepl {

    public static void launchRepl() {
        launchRepl("127.0.0.1", 4006);
    }

    public static void launchRepl(String ip, int port) {
        IFn require = Clojure.var("clojure.core", "require");
        require.invoke(Clojure.read("clojure.tools.nrepl.server"));

        IFn start_server = Clojure.var("clojure.tools.nrepl.server", "start-server");


        start_server.invoke(Keyword.intern("port"), port);
    }

    public static void stopRepl() {
        //IFn stop_nrepl = Clojure.var("nrepl.embed", "stop-nrepl!");
        //stop_nrepl.invoke();
    }

}
