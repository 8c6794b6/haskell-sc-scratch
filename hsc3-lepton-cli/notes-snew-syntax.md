Syntax for adding new node
==========================

Motivation
----------

While making an interactive repl to send OSC message for scsynth, had a chance to
consider how we can add new node to existing synth tree. Considering the syntax
for 

* What kind of node are we adding?
* Where to add the new node?
* How can we specify above?

Types of node
-------------

There are two types of node: Group and Synth. Each nodes has add action and
target id. 

Arguments for Group nodes are:

* Id for new node (required)
* add action (required) 
* target node id (required)
    
Would like to enable adding multiple group node at once.

Arguments for Synth nodes are:

* Id for new node (required)
* add action (required)
* target node id (required)
* synthdef name (required)
* list of pair of parameter name and parameter value (optional)
     
Add action and target node id are required in OSC message. Could take default
value from current zipper environment. Currently the default add
action is AddToTail, target node id is taken from current focused node.

From above, with looking whether the input has synthdef name or not,  we could
decide whether adding new group or new synth.

Proposed syntax: No.1
--------------------

### Adding new groups

Some possible example for adding new groups.

Add group with id 10 with addAction with default add action and default target id:

    > new 10                        
    
Add new groups with id 10, 11, 12 with default add action and default target id:

    > new 10 11 12
    
Add new group with id 10, add action is AddAfter, target node id is 20:

    > new 10 after 20

Adding 2 nodes. One with new group with id 10, add action AddBefore, target node
id 21, and another with id 11, add action AddAfter, and target node id 22.

    > new 10 before 21 11 after 22
    
Adding 5 nodes, new group with id 10, new group with id 11, new group with id 22
adding before id 21, new group with id 12, and new group with id 13.
    
    > new 10 11 22 before 21 12 13

When we apply this syntax, can we add a synthdef named 'after', 'before', or any
other that matching to name of addaction? 

Yes, we could, when we made the target node id coming after addaction as
requirement.  When digit values come after add action string, it's a group. This
means, we need to pass target group id when explicitly passing add action other
than default.

### Adding new synths

Some example for adding new synths.

Adding new synth with id 1000, synthdef name is "foo":

    > new 1000 foo
    
Adding new synth with id 1000, synthdef name "foo", setting "freq" value as 440
and mapping "amp" value from control bus 100

    > new 1000 foo freq=440 amp=c100
    
Adding new synth with id 1000, synthdef name "foo", add action is AddAfter,
target node id is 12, param values are same as above.

    > new 1000 foo after 12 freq=440 amp=c100

Proposed syntax: No.2
---------------------

### Making it shorter

In above syntax, when we add multiple groups with specifying add action and
target id, it becomes a bit difficult to read due to using space as common
separater for separating new node ids, addactions, and target ids.

Since add actions and target ids are combined data, and they are
written prior to new group id, how about combining new node id,
addactoin, target id without space? When we apply this rule, the third
sample shown in syntax No.1 would become:

    > new 10a20
    
and fourth:

    > new 10b21 11a22
    
and fifth:

    > new 10 11 22b21 12 13

### Too dense?
    
Seems bit better to me. Though, some might hold a point of view that this syntax
is too dense. How about:

    > new 10after20 
    > new 10before 21 11after22
    > new 10 11 22before21 12 13

... Not feeling so much difference to me.

Propsed syntax: No.3
--------------------

### Changing separator

Confusion made to our eyes might be due to the use of same separater, the space.
How about using different separator when adding multiple group nodes? Rewriting
examples shown in Proposal No.1:

    > new 10
    > new 10,11,12
    > new 10 before 21,11 after 22
    > new 10,11,22 before 21,12,13

Hm... not bad, but still, it might seem dense. How it will look like when using syntax
in Proposal#2?

    > new 10b21,11a22
    > new 10,11,22b21,12,13

Hm... bit better. But prefer those shown in Proposal#2 than this.

Proposed syntax: No.4
---------------------
In syntax for adding new synthnode in syntax No.2, node ids are coming before
synthdef names. Since we can decide whether the new nodes are group or synth
with whether the input has synthdef name or not, why not we type the synthdef
name as the first argument?

When we rewrite example in syntax No.1:

    > new foo 1000
    > new foo 1000 freq=440 amp=c100
    > new foo 1000a12 freq=440 amp=c100

This change will enable us to show synthdef names as canditates soon after when
the user typed 'new', and stop showing when digits were entered instead of
synthdef name, assuming that none of synthdef will have a name starting with
digits.

Conclusion
----------
At the moment, the best syntax I can imagine is: Proposal 4, with separating
snew and gnew toplevel name.

When we separate toplevel, it becomes easier to complete with tab, write the
parser, etc.

For node id, using the form 'NAM', where:

* N : sequence of digit for new node id
* A : add action. Need to be one of:
    * a : AddAfter
    * b : AddBefore
    * h : AddToHead
    * t : AddToTail
* M : target node id
    
A and M are optional, and when specifying them, both A and M must to be given.

We can use this rule for synth nodes also.

    > snew foo 1001b1002 freq=440 amp=c100

Will add synth with not id 1001 after node with id 1002, def name is 'foo' and
parameter 'freq' set to 440, 'amp' mapped from control bus 100.
