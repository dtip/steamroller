🚂 Steamroller
=====

Upset your colleagues by autoformatting their Erlang code.

Build
-----

    $ rebar3 compile

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

    rebar3 steamroll --check


The exit code will be non-zero if the code has not been formatted before being committed.
