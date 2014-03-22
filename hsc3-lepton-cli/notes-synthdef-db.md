Schema for synthdef database
============================

Motivation
----------

Want to try [kyotocabinet](http://http://fallabs.com/kyotocabinet/) out, let's
make a simple database of synthdef files from supercollider.

Functionalitly
--------------

* Insert synthdef file to database.

* Query synthdef by:
   * synthdef name.
   * parameter name.
   * ugen name.

* List summary of each matching result.

* Show detail of specified single synthdef.

* Show statistics ... What can we count?

* Deleting synthdef?

Schema
------

Kyotocabinet is simple key value database. What schema could support above
features? 

### Plan A

Three types of keys: 

* `def:<SYNTHDEF_NAME>`, with its value as synthdef contents.

* `para:<PARAMETER_NAME>`, with its value as list of synthdef names containing
  this parameter name in synth definition graph.
  
* `ugen:<UGEN_NAME>`, with its value as list of synthdef names containing this
  ugen in synth definition graph.
    
For instance, when inserting new synthdef called 'foo', operations would be:

* Insert new record with `def:foo`, value is synthdef contents.

* Check parameter name of foo. For each parameter, if the parameter in foo
  already exists in db, add 'foo' in db record's value. If not, insert new
  record. For instance, when parameter name was `freq`, and no record exist in
  db yet, insert a new record with key `param:freq` and value 'foo'.
  
* Check ugen of foo. For each ugen, if the ugen in foo already exists in db, add
  'foo' in db record's value. If not, insert new record. For instance, when ugen
  `pv_magFreeze` was used in foo and not found in db, insert a new record with
  key `ugen:pv_magFreeze`, and value 'foo'.

Hm... `para:<PARAMETER_NAME>` and `ugen:<UGEN_NAME>` are optional. Query for
ugen and param names could be done with visiting synthdef records only, though
everytime we need to traverse all records. It sound like matter of where to
spend more time, whether at the time of inserting or querying.

So .....

### Plan B

Insert synthdef with its name as key, definition contents as value.

Look for parameters and ugens at the time of query. When this turned out to be
too slow, try Plan A.

OK, then lets move on with Plan B
----------------------------------
Did it.

Noticed that parsing synthdef data representation in haskell everytime
from bytestring is not a best way to do. It will be show better
performance when somewhat intermediate data were stored in database.

So, currently there's three types of keys:

1. Synthdef name, which stores the contents of file in bytestring format.

2. Snthdef name plus ".ugens", ugens appearing in the synthdef,
   separated with space.
  
3. Synthdef name plus ".params", parameter names in synthdef, again,
   separated with space.

key type 2 and 3 help when we query with UGen or parameter name. 

Also, wrote a simple statistic for ugens and parameters. Counting each
occurance of parameter names and parameters.

What else would be interesting?
-------------------------------

