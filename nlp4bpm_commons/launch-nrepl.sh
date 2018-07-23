#!/bin/bash

export LD_LIBRARY_PATH="${LD_LIBRARY_PATH:+$LD_LIBRARY_PATH:}/usr/local/lib/:/home/josep/Repositories/freeling/build/APIs/java/"
# ldconfig -N -v $(sed 's/:/ /' <<< $LD_LIBRARY_PATH) | grep freeling # <- If this prints nothing, then LD_LIBRARY_PATH is not set correctly
mvn clojure:nrepl -Dclojure.nrepl.handler=cider.nrepl/cider-nrepl-handler

