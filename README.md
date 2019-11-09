🚂 Steamroll
=====

Upset your colleagues by autoformatting their Erlang code.

Build
-----

    $ rebar3 compile

Use
---

Add steamroll to your rebar config:

    {plugins, [
        {steamroll, {git, "https://github.com/old-reliable/steamroll.git", {branch, "master"}}}
    ]}.

Then call it directly in an existing application:

    $ rebar3 steamroll
    ===> Fetching steamroll
    ===> Compiling steamroll
    <Steamroll Output>
