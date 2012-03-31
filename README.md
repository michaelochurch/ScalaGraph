... an open source Graph library in Scala. 

Michael O. Church
last updated 31. March 2012

The goal of this library is to provide a high-quality, open-source,
general-purpose Graph library in Scala. 

1. CORE TERMS

A GRAPH is a set of NODES (vertices) which are first-class entities,
along with a set of EDGES that represent relationships between
them. Each Edge has a source and dest Node. (For now, all edges are
directed.) The Graph is a general purpose data structure, but it also
allows us to perform structured queries. For example, a search might
look like this:

Human impulse: "I live in New York, and I want a cheap flight to a
warm place."

As instructions: "Give me all flights from New York (node / entry
point) under $400 (edge property) to places where the average winter
temperature is at least 70 F (node property)."

As graph query (pseudocode, because I haven't decided on a Q. language
yet):

FindNodes(T == "city" & name == "new york") |-> 
    FollowEdges(T == "flight" & cost < 400) |-> 
    FilterNodes(T == "city" & "winterTemp" > 70)

2. GRAPH COMPONENTS

Graphs come in a variety of sizes and types. Some graphs are small and
can be visualized. Others are so large they must be distributed. Some
large graphs will be stored mainly on disk, while others need to be
in-memory at all times, for performance reasons. Some are immutable,
others are append-only, meaning that information can be added but not
removed, and others are fully mutable.

Graphs are _parameterized_ in their Node and Edge types. 

A NAME is a type intended for uniqueness. Names should be globally
unique. Each Node and Edge has one. Currently, Names are implemented
using 128-bit UUIDs.

A PAYLOAD is an optionally labelled record. Currently, both field
names and field values are String, and only the binary ordering (for
comparison) is used. Case-insensitive comparisons and numeric field
values I will support in the future. I18n (variations in conventions of
alphabetical order) may be supported in the future.

STYLE NOTE: Untyped payloads shouldn't be used in real-world cases. 

The reason for using the labelled-record type is that it's
unreasonable to impose "bondage-and-discipline" static typing on all
systems (many distributed) for which this library might be used. At
least for now, it's up to the user to enforce type/schema safety. The
type field indicates the schema (on the Map[String, String] data) that
should apply.

For testing purposes, where one only cares about the shape of the
graph, untyped or empty payloads will be used.

A NODE has a unique id. Almost all Node types will have a Payload
attached.

The reference implementation, BaseNode, has an id (Name) and a Payload
only.

An EDGE has three Name fields-- id, source, and dest. That is, the
structural type of the abstract Edge is:

{val id: Name, val source: Name, val dest:Name}.

The reference implementation, BaseEdge, also has a Payload. 

Future expansions may allow Nodes and Edges to have append-only
versioning. Also, "time graphs" (append-only graphs in which changes
are signified by writing new edges, and in which queries pull the
'best' edge, which may change over time) are something I'd like to
support in the future.

3. GRAPH TYPES

For a start, I'm going to focus on a couple core graph types. I intend
for these graphs to be parameterized over NodeT <: Node and EdgeT <:
Edge-- this means that they can be specialized to specific types of
nodes and edges. For now, the only NodeT that exists is BaseNode and
the only EdgeT is BaseEdge.

3A. ResultGraph: a small, immutable graph. 

The purpose of the ResultGraph is to be returned as a result of
computations over much larger graphs. Generally, ResultGraphs should
be small and human-usable. Future expansions may include:

* Visualization for ResultGraphs. (p = 0.7/3 months)
* Path-finding within ResultGraphs (since they're immutable and
  generally small). (p = 0.9/3 months)
* Support for AI/ML algorithms over ResultGraphs. (p = 0.3/3 months)

ResultGraphs do not have indexes, except over the Name of each Node
and Edge. 

3B. MutableInMemoryGraph: the first large graph. 

This graph's purpose is to hold larger amounts of data. It will
support search operations and indexes. 

4. DATA FLOW OF SEARCHES

The Data Flow of searches is going to look like the following.

4A. Query: LargeGraph => ResultGraph

"LargeGraph" represents a graph type that may be distributed,
disk-backed, append-only or fully mutable. Currently, the only large
graph I'm working on is MutableInMemoryGraph.

Queries will return a smaller, immutable graph representing the result
of the query at that time. The reason for returning a ResultGraph
(without further processing) is that we wish to limit interaction with
"LargeGraph" types, ideally, to one call. This is because these graph
types may involve network communication or disk reads/writes for each
operation.

4B. Using the ResultGraph. 

For some queries, a graph is an appropriate. For example, the
simplest searches ("find all nodes where <X>") will return a set of
matching nodes (with no edges).

Other queries may desire post-processing such as path-finding,
especially if ResultGraphs are large. That will can be done "client
side" to the ResultGraph. 