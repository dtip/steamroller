🚂 Steamroller
==============

![Build Badge](https://github.com/old-reliable/steamroller/workflows/build/badge.svg)

Steamroller is an opinionated Erlang code formatter. Uniformity is inevitable.

![Austin Powers Steamroller Gif](https://thumbs.gfycat.com/MassiveGlossyAmericantoad-size_restricted.gif)

Upset your colleagues by autoformatting their code.

This rebar3 plugin is early stage but could be used in production if you're brave. The Erlang
abstract syntax tree is checked before and after formatting to make sure that the formatted file
is equivalent to the original.

Steamroller should be used as part of your CI to enforce a consistent style across your codebase.

Use
---

Add steamroller to your rebar config:

    {plugins, [
        {steamroller, "0.2.0"}
    ]}.

Then ask it to steamroll your code directly in an existing application:

    $ rebar3 steamroll
    ===> Fetching steamroller
    ===> Compiling steamroller
    <Steamroller Output>

CI
---

To check that code is properly formatted as part of your CI:

    $ rebar3 steamroll --check

The exit code will be non-zero if the code has not been formatted before being committed.

Build
-----

    $ rebar3 compile

Test
----

    $ rebar3 test

Dialyzer
--------

Dialyzer is Erlang's static analysis tool.

    $ rebar3 dialyzer

Local Development
-----------------

In order to use steamroller locally on itself you need to symlink the repo into the \_checkouts
directory:

    cd $local_steamroller_repo
    mkdir _checkouts
    ln -s $PWD ./_checkouts/steamroller

Rebar3 will use the version of steamroller in the \_checkouts directory with higher priority than
the version specified in the rebar.config. There's a line in the rebar.config (see `overrides`)
which prevents infinite plugin loops when using steamroller locally like this.
