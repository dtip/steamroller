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

## Use

Add steamroller to your rebar config:
```erlang
{plugins, [steamroller]}.
```

Then ask it to steamroll your code directly in an existing application:
```sh
$ rebar3 steamroll
===> Fetching steamroller
===> Compiling steamroller
<Steamroller Output>
```

## Editor Integration

### Vim

You can use steamroller from vim with [Neoformat](https://github.com/sbdchd/neoformat).

Install Neoformat and then paste the below into your vimrc.

```viml
" Erlang autoformat
let g:neoformat_erlang_steamroller = {
  \ 'exe': 'rebar3',
  \ 'args': ['steamroll -f'],
  \ 'stdin': 0,
  \ 'replace': 1,
  \ }
let g:neoformat_enabled_erlang = ['steamroller']
autocmd BufWritePre rebar.config,*.[he]rl,*.app.src Neoformat steamroller
```

### Visual Studio Code

You can use steamroller from Visual Studio Code with the
[Erlang Formatter](https://marketplace.visualstudio.com/items?itemName=szTheory.erlang-formatter)
extension created by [szTheory](https://github.com/szTheory).

## CI

To check that code is properly formatted as part of your CI:

    $ rebar3 steamroll --check

The exit code will be non-zero if the code has not been formatted before being committed.

## Build & Run

```sh
$ git clone https://github.com/old-reliable/steamroller.git
$ cd steamroller
$ make
```

## Build

```sh
$ make compile
```

## Run

```sh
$ make run
```

## Clean project

```sh
$ make clean
```

## Test

```sh
$ make test
```

## Dialyzer

Dialyzer is Erlang's static analysis tool.
```sh
$ make dialyzer
```

## Generate documentation

```sh
$ make doc
```

## Hex publish

```sh
$ make publish
```

## Local Development

In order to use steamroller locally on itself you need to symlink the repo into the \_checkouts
directory:
```sh
cd $local_steamroller_repo
mkdir _checkouts
ln -s $PWD ./_checkouts/steamroller
```

Rebar3 will use the version of steamroller in the \_checkouts directory with higher priority than
the version specified in the rebar.config. There's a line in the rebar.config (see `overrides`)
which prevents infinite plugin loops when using steamroller locally like this.
