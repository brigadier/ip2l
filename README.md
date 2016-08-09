## Erlang OTP application for handling IP2Location http://ip2location.com/ .BIN files.

#### Features:
* Supports all 24 types of IP2Location databases;
* IPv4 only;
* Multiple pools, each with its' own file and settings;
* Start/stop pools dynamically or from the application `env`;
* Access to each opened file by atom;
* Allows reloading files and replacing files in each pool without inerrupting or slowing down requests stream;
* Safe from binary reference leakage, binary parts are getting copied.



#### Note:
* Uses [Simplepool](https://github.com/brigadier/simplepool) pools. You might not like it as
`simplepool` uses quite unconventional thing - it compiles pool proc names and other data in a RAM beam module.
* If you create pools with unique names too often you eventually would excaust atoms table. This isn't really
a problem though as in most real use cases just one pool is used.
* The app does not read the whole file in RAM, as some databases reach 1G of size. In order to allow replacing
files without interrupting request flow the app does this: as soon as the pool is started or `reload_database`
function gets called, the pool creates hardlink with unique name in a subdir of the pool working dir, deletes
the file and works with the hardlink from now on. Then when you call `reload_database` again, it checks
the directory for the new file, if there's one - do hardlink, open hardlink, delete old hardlink, delete
the file. So don't call `reload_database` and don't start pools while file is still copied into the directory
as the app would use incomplete file. Don't use multiple pools with the same directory. And don't use
working directories for anything else but the DB files.
* The app accepts IPs in `{B3:8, B2:8, B1:8, B0:8}` format.
* You must specify unique directory for each pool. While it is not enforced by the app itself, you still should
start pools with unique directories only, otherwise there will be some nasty races.

Build
-----

    $ rebar3 compile



#### Example:



```erlang
ip2l:start().
ok = ip2l:start_pool(pool1, [{size, 32}, {sup_flags, {one_for_all, 1, 5}}], "priv").
#ip2l{country_short = <<"AU">>, country_long = <<"Australia">>} = ip2l:lookup(pool1, {1, 10, 10, 10}).
ok = ip2l:reload_base(pool1, "/tmp/otherdir/").
```

See more examples in tests


Tests
-----

    $ rebar3 ct

Tests use some demo databases downloaded from the IP2Location site. The databases are incomplete,
could and should be used for testing purposes only. The license for the app does not cover usage of these files.

#### TODO

* Add IPv6 support


