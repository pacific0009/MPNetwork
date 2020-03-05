# MPNetwork
The idea behind this project is create a local wireless network in which thae master node will keep track of state other nodes and give them instruction. 

HC12 wireless transiver module was used for this project
![alt text](https://github.com/pacific0009/MPNetwork/blob/master/MPNetwork.png)

The Master node Or Controller is written in Erlang programming language and can be run in Raspbery Pi or any computer.
You need to connect HC12 transiver module with UART pin at raspbery pi or connect HC12 with a USB to TTL and connect with computer.

The Other slave node is implemented in c using Arduino framework here is the link https://github.com/pacific0009/HC12MPN

Upload the Example sketch in arduino device and then connect HC12 with UART




