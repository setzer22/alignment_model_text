# Install

## Step 1. Dependencies

Make sure the following dependencies are installed in your system:

- Maven :: `sudo apt-get install maven` Java 8 or higher must be installed in the system.
- FreeLing 4.1 :: See the installation guide at [http://nlp.lsi.upc.edu/freeling/index.php/node/8]. If a non-default install directory is used during installation, it must be specified in ModelVsDocument's config as described later in this document.
- Gurobi ILP Solver :: See the *Downloads* at [http://www.gurobi.com/]. Free licenses for academic purposes can be requested at [http://www.gurobi.com/academia/for-universities].

## Step 2. Application Install

After cloning or downloading this repository, set the according environment variables in the `install.sh` script on the project root folder.

The `install.sh` script will perform the steps of installation process. The script has been tested on Ubuntu 16.04 LTS and should work on all Linux distributions. For other operating systems, please consult the script's inline documentation to manually replicate the necessary steps for your platform.

## Step 3. Configuration

The main entry point of ModelVsDocument is the StandaloneMain class located at `./modelvsdocument/src/main/java/edu/upc/modelvsdocument/StandaloneMain.java`. There are some configuration options that can be set in-code to change the behaviour of the tool. Please make sure that all paths point to existing locations in your filesystem.

# Running the software

To run the software ...

