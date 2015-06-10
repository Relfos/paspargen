PASPARGEN
============

_PASPARGEN_ is a parser generator written in Object Pascal, capable of generating parsers also in Objective Pascal.
It takes input grammars in extended Backus–Naur Form and produces a Unicode capable lexer and a recursive descent parser that automatically constructs an abstract sintax tree.

Motivation
----------------
When I decided to start this project I was writing a Object Pascal parser using Yacc and Lex.
Why was it necessary to spend so much time writing boiler-plate code and manually assembling a AST, I asked.
Could it be possible to generate a complete parser just from a grammar description?
The answer is yes, as long as we're ok with cutting some corners.

How it works
----------------
Paspargen requires just a single grammar file as input.
A grammar is just composed of a big list of nodes, and Paspargen automatically deduces what parts of the grammar should be handled to the lexer, and what should be handled by the parser.
All nodes are then sorted into a abstract sintax tree and checked for redundancy and circular references.

Node types supported
* Literals/Keywords
* Regular expressions
* Class hierarchies

Warning
----------------
The current code is a mess, this was a experiment done in a week. Will be cleaned later, if I have the time.
However is already in a functional state, capable of generating parsers for complex languages.