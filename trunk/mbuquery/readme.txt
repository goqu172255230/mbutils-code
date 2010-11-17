  Short introduction to mbuquery utility.

  Description
  -----------

  The mbuquery utility is a console program which can perform various
Modbus queries. It can be used as debug tool, as part of data accusing
system and even to control Modbus devices - thanks to support of 'write'
operations.


  Usage
  -----

  It is recommended to run program with '-h' key to get a list of supported
switches and commands. Here is an example run to read four holding registers
starting from register 201 (by default, slave address is 1):

	mbuquery -d com4 rdhold 201 4

  Multiple commands could be specified in one run:

	mbuquery -d com4 rdhold 201 4 rdhold 96 1

  If '-n' option are given when multiple commands present, then an entire
set of commands will be repeated specified number of times.

  It is possible to perform write operations. In the following example
we'll write 3 values starting from register number 5:

	mbuquery -d com4 wrhold 5 3 1 0x22 128


  More functionality
  ------------------

  Due to flexible design of both mbulib and mbuquery, it is relatively easy to
add support for new functions. If you miss particular query, then try to
implement it by copying and modifying the existing code. Or, leave a request
with explanation of your needs.
