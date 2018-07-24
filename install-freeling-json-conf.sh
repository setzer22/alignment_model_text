# This script will install a json configuration file for all supported languages in your freeling directory.
export FREELING_HOME=/usr/local/share/freeling

for i in $FREELING_HOME/??
do 
    sudo echo -e "<Type>\njson\n</Type>\n<Options>\nAllAnalysis true\nAllSenses true\n</Options>\n<TagsetFile>\ntagset.dat\n</TagsetFile>" > $i/json.cfg
done
