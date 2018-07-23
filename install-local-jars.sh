#!/bin/bash

mvn install:install-file -Dfile=./local-jars/jbpt-0.2.393.jar -DgroupId=org.jbpt -DartifactId=jbpt -Dversion=0.2.393 -Dpackaging=jar -DgeneratePom=true
mvn install:install-file -Dfile=./local-jars/nlp4bpm_commons-0.1.0.jar -DgroupId=edu.upc -DartifactId=nlp4bpm_commons -Dversion=0.1.0 -Dpackaging=jar -DgeneratePom=true
mvn install:install-file -Dfile=./local-jars/OpenXES-20160212.jar -DgroupId=org.deckfour -DartifactId=xes -Dversion=0.1.0 -Dpackaging=jar -DgeneratePom=true
