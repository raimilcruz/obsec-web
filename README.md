
[![Build Status](https://travis-ci.com/raimilcruz/obsec-web.svg?token=jhJHNfzf8dVnKKPhxozx&branch=master)](https://travis-ci.com/raimilcruz/obsec-web.svg?token=jhJHNfzf8dVnKKPhxozx&branch=master)


# ObSec Pad:

**ObSec** is a simple object-oriented language that supports type-based declassification

We provide an **online [ObSec Pad](https://pleiad.cl/obsec/)** that is ready to use, with predefined examples and it is always up-to-date

## Local execution of the ObSec Pad

To run the ObSec Pad locally, all you need is Java 1.8. Also check that you have Java 1.8 in the path.

The following instructions are Unix valid commands. If you have Windows installed in your machine, you need to perform the corresponding commands.

 - Unzip the file. 
    ```sh
    unzip obsec-web.zip
    ```
 - Go to the "obsec-web" folder:
    ```sh
    cd obsec-web
    ```
 - Add run permissions to the binary file:
    ```sh
    chmod +x bin/obsec-web
    ```
 - And then run:
    ```sh
    ./bin/obsec-web
    ```
    This should start a server in the port 9000. To open the ObSec Pad, go to [http://localhost:9000/obsec/](http://localhost:9000/obsec/)
 - In case the port 9000 is already taken, run:
    ```sh
    ./bin/obsec-web -Dhttp.port=`<available-port-number>
    ```

# Deploy instructions
To generate a deployable .zip file follow these instructions.
1.  Run the script file `make` of the "node" folder:
    ```sh
    ./node/make
    ```
2. Run
    ```sh
    sbt dist
    ```
   The deployable file will be generated in the folder 
   `target/universal`

## Acknowledgments:
This project is a fork of the project [https://github.com/pleiad/intrinsifymysoul](https://github.com/pleiad/intrinsifymysoul)
