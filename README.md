tinybot
=======

Tinybot is a toy implementation of semi-naive bottom up logic
programming; the several implementations all allow visual stepping
through the process of proof search. There are three implementations:

 * [tinybot-simple.cm][1] -- Persistent, McAllester-style semi-naive
   evaluation.

 * [tinybot-trackdepth.cm][2] -- Semi-naive evalutation that does some
   dependency tracking to make sure that each fact is proved with the
   minimum-depth proof, a change that is important to some algorithms
   like Meld that do truth management.

 * [tinybot-lin.cm][3] -- A very simple example of using the indexing
   structures from semi-naive evaluation to do linear forward chaining
   in the style of Ollibot, Lollimon, or Linear Logical Algorithms.

The test/demo/example file [test.sml][4] is loaded by all the
configuration files; the tests in that file can be run like this:

```
$ git clone git://github.com/robsimmons/tinybot.git
$ cd tinybot
$ sml -m tinybot-simple.cm # or one of the other two
- Test.test1 ();
```

The test/demo/example file for linearity [test-lin.sml][5] is loaded
only by tinybot-lin.cm; it can be run like this:

```
$ git clone git://github.com/robsimmons/tinybot.git
$ cd tinybot
$ sml -m tinybot-lin.cm
- LinTest.test0 ();
```

[1]: https://github.com/robsimmons/tinybot/blob/master/tinybot-simple.cm
[2]: https://github.com/robsimmons/tinybot/blob/master/tinybot-trackdepth.cm
[3]: https://github.com/robsimmons/tinybot/blob/master/tinybot-lin.cm
[4]: https://github.com/robsimmons/tinybot/blob/master/test.sml
[5]: https://github.com/robsimmons/tinybot/blob/master/test-lin.sml