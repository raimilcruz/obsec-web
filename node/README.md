
# Gradual Security:

## How to run this application:

To run the application all you need is Java8 (you need Java8 in the PATH).
Unzip the file.
```sh
unzip intrinsify-0.0.1.zip
```
go to the folder:
```sh
cd intrinsify-0.0.1
```
add run permissions to the binary file:
```sh
chmod +x bin/intrinsify
```
and then run
```sh
./bin/intrinsify
```
This should start a server in the port 9000.
To open the application go to http://localhost:9000


- To add a lambda symbol just insert a "\" symbol in the program (it'll be transformed automatically).

- To annotate function types and it latent effects use for example Int_? ->^H_L Int_? where H is the latent effect and L the security level of the function.
