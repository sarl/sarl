Basically, the networking feature of the Janus SRE enables you to launch instances of the Janus kernel on different
computers, and exchange events between these instances.

![General Networking Architecture](./janusnetworkextension.png)

In order to implement this feature, the [Hazelcast](http://www.hazelcast.com) library is used.
Hazelcast is an open source in-memory data grid based on Java. In a Hazelcast grid, data is
evenly distributed among the nodes of a computer cluster, allowing for horizontal scaling of
processing and available storage. Backups are also distributed among nodes to protect against
failure of any single node.

In addition to the data-sharing and efficient communication means, Hazelcast provides the ability
to build the node network automatically. Each Hazelcast node that is associated to a given cluster
name is *connected automatically* to the other nodes in the same cluster on the local network.
It means that you do not need to provide a network configuration to the Janus SRE.

