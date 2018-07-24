# Model to Text Alignment Tool

This tool can be used to compute an alignment between two different representations of process models: BPMN and Text files in natural language. The tool uses NLP techniques to extract linguistic features from both text and model, which are then compared. Finally, the alignment is converted to an ILP optimization problem.

# Installation Guide

## Step 1. Dependencies

Make sure the following dependencies are installed in your system:

- JDK :: `sudo apt-get install default-jdk`
- Maven :: `sudo apt-get install maven`. Java 8 or higher must be installed in the system.
- CMake 5.8 or higher :: `sudo apt-get install cmake`. **NOTE:** On some distributions you will need to download a newer version from the CMake website.

## Setp 2. Build and Install FreeLing

For further reference, see the installation guide at [https://talp-upc.gitbooks.io/freeling-4-1-user-manual/content/installation/installation-source.html]. This guide provides a summary of the necessary steps.

First, install the following dependencies:

- `libicu`
- `libboost-regex`
- `libboost-system`
- `libboost-thread`
- `libboost-program-options`
- `zlib`

Then, execute the following commands on a shell:

```
cd /some/repository/folder
wget https://github.com/TALP-UPC/FreeLing/archive/4.1.tar.gz
tar -zxvf 4.1.tar.gz
cd freeling
mkdir build
cd build
cmake -DJAVA_API=on -DWARNINGS=off ..
make -j4 # NOTE: Replace 4 with the number of threads in your machine
sudo make install 
```

If a non-default install directory was chosen during installation (using -DCMAKE\_INSTALL\_PREFIX), it will need to be specified in ModelVsDocument's config as described later in this document. 

Once the install process is complete, two files located at `$FREELING_REPOSITORY/build/APIs/java` must be moved. Place the Jfreeling.jar file in the `local-jars` folder of this repository and the `libjfreeling.so` at the root of the repository.

## Step 3. Install the Gurobi ILP Solver

First, downlad the latest version of gurobi at the *Downloads* section in [http://www.gurobi.com/]. Note that this requires creating an account. At the time of writing this guide, the latest version is 8.0.1. You should replace `gurobi801` with the appropiate version number where necessary. 

To use gurobi, you need a license. Free licenses for academic purposes can be requested at [http://www.gurobi.com/academia/for-universities].

Download the gurobi8.0.1_linux64.tar.gz and extract it to your preferred location. Next, open a shell session and execute the following commands to activate the license:

```
cd /gurobi/extract/path/linux64/bin
./grbgetkey YOUR_LICENSE_KEY

```

## Step 3. Application Install

After all the installation prerequisites are met, you can configure the necessary variables in the `install.sh` script to point to your gurobi and FreeLing installations. Additionally, if FreeLing was installed to a non-default location, the right environment variable must also be set at the top of the install-freeling-json-conf.sh file. These scripts are located on the project root folder.

The `install.sh` script will perform the steps of installation process. The script has been tested on Ubuntu 16.04 LTS and should work on all Linux distributions. If your user doesn't have permission to write to the FreeLing's installation folder (this happens by default), you will need to run the `install.sh` script with privileges (e.g. `sudo`). 

Once the script completes, you have successfully built ModelVsDocument.

## Step 4. Configuration

The main entry point of ModelVsDocument is the StandaloneMain class located at `./modelvsdocument/src/main/java/edu/upc/modelvsdocument/StandaloneMain.java`. There are some configuration options that can must set in-code to change the behaviour of the tool. Please make sure that all paths defined point to existing locations in your filesystem.

# Running the software

The software supports two running modes, *execution* and *evaluation*. In execution mode, the program will receive a BPMN XML file and a text file, and will output a log with a detailed explanation of the computed alignment. In evalutaion mode, a third file in .json format must be provided. This file is the groundtruth file that will be used for comparing the alignment tool's results. The json format is described in the complementary dataset available at the datasets folder of this repository.

To run the tool in execution model, execute the following commands:

```
source modelvsdocument.src
cd modelvsdocument 
mvn exec:java -Dexec.mainClass="edu.upc.modelvsdocument.StandaloneMain" -Dexec.args="execution /path/to/bpmn/file.bpmn /path/to/text/file.txt"
```

The tool in evaluation mode requires an additional json file as argument:

```
source modelvsdocument.src
cd modelvsdocument 
mvn exec:java -Dexec.mainClass="edu.upc.modelvsdocument.StandaloneMain" -Dexec.args="execution /path/to/bpmn/file.bpmn /path/to/text/file.txt /path/to/groundtruth/file.json"
```
