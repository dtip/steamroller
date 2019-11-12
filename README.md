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
