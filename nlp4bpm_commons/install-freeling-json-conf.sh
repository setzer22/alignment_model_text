# This script will install a json configuration file for all supported languages in your freeling directory.
for i in /usr/local/share/freeling/??
do 
    sudo echo -e "<Type>\njson\n</Type>\n<Options>\nAllAnalysis true\nAllSenses true\n</Options>\n<TagsetFile>\ntagset.dat\n</TagsetFile>" > $i/json.cfg
done
