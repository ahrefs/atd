Atdcpp
====

Atdcpp is a program that generates a C++ interface from type definitions.
In particular, given a set of ATD type definitions,
this tool generates a set of C++ classes representing those types

The primary benefits of using the generated interface, over using a 
JSON library and writing case classes directly, are safety and ease of use
when the same interface is needed in other languages.

Don't repeat yourself when defining a JSON interface in multiple languages. 
Instead generate compatible interfaces for all languages from a single definition.

Even if you're only using C++, you may find the ATD definitions more concise
and less boilerplaity than the equivalent Scala types. (Especially for sum types)
