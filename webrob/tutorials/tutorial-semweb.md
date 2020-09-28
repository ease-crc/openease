
-----------------------------------------------------------------
Semantic Web -- Introduction	
-----------------------------------------------------------------

This course is concerned with symbolic knowledge representation
using standardized languages of the semantic web.
The semantic web has a layered architecture.

Its basis is XML syntax for content structure within documents.
The next layer is the Resource Description Framework (RDF),
which is a simple language for expressing data models.

<div align="center">
<img src="https://upload.wikimedia.org/wikipedia/commons/thumb/f/f7/Semantic_web_stack.svg/405px-Semantic_web_stack.svg.png" alt="Smiley face" width="320">
</div><br>

RDF Schema extends RDF and is a vocabulary
for describing properties and classes of RDF-based resources.
The OWL layer adds more expressive elements to describe
classes and their relationships, e.g.,
existential (there must be some) and
universal (there can only be)
restrictions on class properties.
We will have a closer look at the RDFS and OWL layer during this course.

It may seem tedious to deal with these formal languages.
And in fact, it is non trivial to master them.
So what do we gain by employing semantic web technologies?

One gain is being able to perform automated reasoning.
This is possible because the languages are formally defined
and computationally well understood.
It is also useful to represent domain terminologies on which
all involved parties agree upon.
This is important to avoid confusion about what the
specific meaning of a term is.
Interdisciplinary projects, such as EASE, further need
a foundational basis for combining
data coming from different sub-projects.
This is to clarify foundational questions such as:

  * What is a `Situation`, `Event`, `Process`, `Motion`,`Action`?
  * Is `Reaching` an `Action` or a `Motion`?

-----------------------------------------------------------------
Semantic Web -- Resource Description Framework (RDF)
-----------------------------------------------------------------

The RDF data model is similar to classical conceptual modeling approaches such as entity-relationship or class diagrams,
as it is based upon the idea of making statements about resources (in particular web resources) in the form of subject-predicate-object expressions.
These expressions are known as triples in RDF terminology.
The subject denotes the resource we want to make a statement about,
and the predicate denotes traits or aspects of the subject
and expresses a relationship between the subject and the object. 

In RDF terminology, subjects, objects, and also predicates
are resources.
Each resource is identified by a unique name, the *IRI*
(Internationalized Resource Identifier) which is
a URI pointing to the definition of the resource.
An example IRI is the following one:

<pre>
http://knowrob.org/kb/knowrob.owl#Dishwasher
</pre>

In Prolog, IRIs are represented as atom, i.e. `'http://knowrob.org/kb/PR2.owl#PR2Robot1'`.
To avoid writing the full IRI, namespace prefixes are used
which can be dynamically registered in case some external ontology is used
for which the prefix was not registered in advance.
You can use `rdf_current_prefix/2` to list existing namespaces:

    rdf_current_prefix(knowrob,_).

I.e. we can also write `knowrob:'Dishwasher'`.

Unfortunately, the automated term expansion won't work for the rules
you write during the fall school.
The KB `RDF` declares a rule `expand/2` that needs to be used
for arguments using the prefix notation,
because rdf predicates expect IRI atoms, and not terms.

The core of the SWI-Prolog package `semweb` is an efficient
main-memory RDF store that is tightly integrated with Prolog.
It provides a predicate `rdf/3` to query the RDF store:

    rdf(knowrob:'Dishwasher',Predicate,Object).

The "object" of triples may also be a data value instead of a resource.
Depending on the Prolog version, these are repesented either
as term `literal(type(Type,Value))` or, in a newer version,
as `Value^^Type`.

Triples can be dynamically asserted into the triple store.

    rdf_assert(knowrob:'TransAIR', rdf:type, owl:'Class'),
    rdf_assert(knowrob:'TransAIR', rdfs:subClassOf, knowrob:'Event').

Here we state that the `knowrob:'TransAIR'` is a class resource,
and that all fall schools are also "events".
Note that this statement has no semantics in the RDF layer,
the meaning of `rdfs:subClassOf` is handled in the RDFS layer above.

The semantic web supports distributed class definitions.
Meaning that different aspects of some class can be defined
in separate modules.
We can, for example, state that dish washers are also physical
devices by asserting the corresponding triple:

    rdf_assert(knowrob:'Dishwasher',rdfs:subClassOf,knowrob:'PhysicalDevice').

This is possible even though the class was defined in some
external ontology.

SWI Prolog offers some predicates to query the RDF triple store
with RDFS semantics.
These are `rdfs_individual_of/2`, `rdfs_subclass_of/2`,
and `rdfs_subproperty_of/2`.

    rdfs_subclass_of(knowrob:'Dishwasher', knowrob:'PhysicalDevice').

These predicates exploit transitivity of `subClassOf` and
`subPropertyOf`, and thus also yield sub-classes of sub-classes.

    rdfs_subclass_of(knowrob:'TransAIR', knowrob:'Event'),
    rdfs_subclass_of(knowrob:'TransAIR', knowrob:'Situation'),
    rdfs_subclass_of(knowrob:'TransAIR', knowrob:'TemporalThing').

*Note* There is also a variant of `rdf/3` that takes into account the class and property hierarchy:
`rdf_has/3`.

`Task 1:` Write a recursive rule `subclass_path(Parent,Child,Path)` that yields
all paths from `Parent` to `Child` following the sub-class relation.

Test your declaration with following queries
(Prolog should answer *true* for all of them):

`a)`

    subclass_path(knowrob:'PhysicalDevice',knowrob:'Dishwasher',
      ['http://knowrob.org/kb/knowrob.owl#PhysicalDevice',
       'http://knowrob.org/kb/knowrob.owl#CleaningDevice',
       'http://knowrob.org/kb/knowrob.owl#Dishwasher']).

`b)`

    subclass_path(knowrob:'PhysicalDevice',knowrob:'Dishwasher',
      ['http://knowrob.org/kb/knowrob.owl#PhysicalDevice',
       'http://knowrob.org/kb/knowrob.owl#HouseholdAppliance',
       'http://knowrob.org/kb/knowrob.owl#ElectricalHouseholdAppliance',
       'http://knowrob.org/kb/knowrob.owl#Dishwasher']).

`c)`

    subclass_path(X,Y,
      ['http://knowrob.org/kb/knowrob.owl#PhysicalDevice',
       'http://knowrob.org/kb/knowrob.owl#CleaningDevice',
       'http://knowrob.org/kb/knowrob.owl#Dishwasher']),
    rdf_equal(X,knowrob:'PhysicalDevice'),
    rdf_equal(Y,knowrob:'Dishwasher').

`Task 2:` Write a predicate `shortest_path/3` that only yields the shortest path between
class resources.

Test your declaration with following queries
(Prolog should answer *true* for all of them):

`a)`

    shortest_path(knowrob:'Artifact',knowrob:'Dishwasher',
      ['http://knowrob.org/kb/knowrob.owl#Artifact',
       'http://knowrob.org/kb/knowrob.owl#PhysicalDevice',
       'http://knowrob.org/kb/knowrob.owl#Dishwasher']).

`b)`

    \\+ shortest_path(knowrob:'Artifact',knowrob:'Dishwasher',
      ['http://knowrob.org/kb/knowrob.owl#Artifact',
       'http://knowrob.org/kb/knowrob.owl#FurniturePiece',
       'http://knowrob.org/kb/knowrob.owl#ElectricalHouseholdAppliance',
       'http://knowrob.org/kb/knowrob.owl#Dishwasher']).

-----------------------------------------------------------------
Semantic Web -- Web Ontology Language (OWL)	
-----------------------------------------------------------------

The OWL Web Ontology Language is designed for use
by applications that need to process the content
of information instead of just presenting information to humans.
OWL facilitates greater machine interpretability of Web content
than that supported by XML, RDF, and RDF Schema (RDFS)
by providing additional vocabulary along with a formal semantics.

The Semantic Web is a vision for the future of the Web
in which information is given explicit meaning,
making it easier for machines to automatically process and
integrate information available on the Web.
The Semantic Web will build on XML's ability to define
customized tagging schemes and RDF's flexible approach to representing data.
The first level above RDF required for the Semantic Web is
an ontology language that can formally describe the
meaning of terminology used in documents.
If machines are expected to perform useful reasoning
tasks on these documents, the language must
go beyond the basic semantics of RDF Schema.

Providing a thorough overview is beyond the scope of this course.
The goal here is to gain an intuition of OWL as representation language.
And to collect first experiences working with OWL ontologies.

To get a better intuition, let's have a look at a class definition
(written in Manchester notation):

<div align="center">
<img src="https://ai2-s2-public.s3.amazonaws.com/figures/2017-08-08/2db3f919b6bd432a0380b47cca558be4e2624885/6-Figure5-1.png" alt="Smiley face" width="600">
</div><br>

This defines that a vegetarian pizza is a type of pizza
which has no toppings which are fish or meat.
The keyword `some` is used for existential property restrictions,
i.e. `hasTopping some FishTopping` means that there must be 
at least one topping which is a fish.
In OWL terms, `hasTopping some FishTopping` is an anonymous class!
Namely the class of all things in the world that have
fish toppings.
The keyword `not` is used to represent complement casses.
This example should make clear that OWL has some relation
to *set theory*, and that class definitions are
essentially definitions of sets whose elements all fullfill
some dedicated criterion (e.g., having a fish topping).
This property makes OWL usefull for classification tasks.

OWL ontologies can be stored in different formats,
one of them is *RDF/XML*.
Such files can be loaded by using the `owl_parse/1` predicate.
The `owl_parse` predicate handles
paths in the filesystem as well as URLs `http://...` and
`package://...`.
The latter are a special kind of URL used in the ROS
ecosystem that refer to files by their local path
w.r.t. a ROS package.

In this course we are going to use an ontology in which
one of our robots, the PR2, is described.

    tripledb_load('package://knowrob_srdl/owl/PR2.owl'),
    tripledb_load('/home/ros/user_data/fall_school/owl/PR2Poses.owl'),
    show(agent(pr2:'PR2Robot1')).

The ontology defines the kinematic structure of the robot in terms of
links and joints, and groups them into semantic components such
as arms or legs.
The semantic components of a robot are denoted by `srdl2comp:subComponent`.

    rdf_has(pr2:'PR2Robot1',srdl2comp:subComponent,Component).

Each comonent has a `srdl2comp:baseLinkOfComposition` and a
`srdl2comp:endLinkOfComposition` between which a path exists
along the `srdl2comp:succeedingLink` and `srdl2comp:succeedingJoint`
predicates.
All links are indiviudal of `srdl2comp:'UrdfLink'`, and
all joints are indiviudal `srdl2comp:'UrdfJoint'`.

Every semantic component is a `srdl2comp:'ComponentComposition'`,
but components are often further classified, e.g. as arm or hand.

    rdf_has(pr2:'PR2Robot1',srdl2comp:subComponent,Component),
    rdfs_individual_of(Component,knowrob:'Hand').

Links can be highlighted in the canvas. The IRIs of links must be wrapped in `object_without_children` terms. E.g.

    rdf_has(pr2:'pr2_left_arm',srdl2comp:baseLinkOfComposition,Link),
    highlight([object_without_children(Link)]).

This yields all components of the robot which are classified as `knowrob:'Hand'`
and highlights them in the canvas.

`Task 1:` Write a predicate `component_links(Component,Links)` that yields all links that belong to a component,
and a predicate `highlight_component(Component)` that highlights all links belonging to a component in the canvas.

Test your declaration with following queries
(Prolog should answer *true* for all of them):

`a)`

    component_links(pr2:pr2_left_arm, Links).

`b)`

    highlight_component(pr2:pr2_left_arm).

`c)`

    highlight_component(X).

`Task 2:` Write a predicate `bbox(Components,size(Width,Height,Depth),position(X,Y,Z))` that relates a list of components to the minimal bounding box that contains all links in them.
Write a second predicate `show_bbox(Components)` that visualizes a cube in the canvas matching the position and size inferred by the `bbox` predicate.

Test your declaration with following queries
(Prolog should answer *true* for all of them):

`a)`

    bbox(Components,
        size(Width,Height,Depth),
        position(X,Y,Z))).

`b)`

    show_bbox([pr2:pr2_left_arm]).

`c)`

    show_bbox([pr2:pr2_left_arm,pr2_right_arm]).

