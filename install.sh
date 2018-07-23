#!/bin/bash


# Please set the following environment variables
export FREELING_LIB=/usr/local/lib
export FREELING_HOME=/usr/local/share/freeling/
export GUROBI_HOME=$HOME/gurobi801/linux64

# ---------------------------------------------------
# Create src file with the necessary path definitions
# ---------------------------------------------------

rm modelvsdocument.src # Delete old file if found
touch modelvsdocument.src

echo 'export PATH="${PATH:+$PATH:}'$GUROBI_HOME'/bin"' >> modelvsdocument.src
echo 'export LD_LIBRARY_PATH="${LD_LIBRARY_PATH:+$LD_LIBRARY_PATH:}'$GUROBI_HOME'/lib:'$FREELING_LIB':'$PWD'"' >> modelvsdocument.src



# -----------------------------------
# Install extra FreeLing config files
# -----------------------------------

sudo /bin/bash install-freeling-json-conf.sh

# ----------------------------------------------------------
# Create necessary files in the home folder config directory
# ----------------------------------------------------------

mkdir $HOME/BPMN
cp -r grammars $HOME/BPMN
mkdir $HOME/BPMN/tmp


exit 0

# --------------------------------------
# Install jars in local maven repository
# --------------------------------------

/bin/bash/ install-local-jars.sh

# -----------------
# Build the project
# -----------------

cd nlp4bpm_commons 
mvn install

cd ../modelvsdocument
mvn compile

