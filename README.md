🚂 Steamroller
==============

![](https://github.com/old-reliable/steamroller/workflows/build/badge.svg)

Upset your colleagues by autoformatting their Erlang.

This plugin is early stage and not yet suitable for use in production. In particular, it has not
yet been testing on records or maps.

Use
---

Add steamroller to your rebar config:

    {plugins, [
        {steamroller, {git, "https://github.com/old-reliable/steamroller.git", {branch, "master"}}}
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
