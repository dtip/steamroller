#!/usr/bin/env bash

# Abort on error
set -e

mkdir -p ~/.config/rebar3

# What we really want to do here is provide the unencrypted api_key directly.
# That would avoid the faff below with the local password.
# If https://github.com/erlang/rebar3/pull/2182 gets merged we can delete this nonsense.
echo '#{<<"hexpm">> => #{username => <<"'$HEXPM_USER'">>, read_key => <<"'$HEXPM_API_KEY'">>, write_key => '$HEXPM_WRITE_KEY'}}.' > ~/.config/rebar3/hex.config

# Debug info - might come in handy.
rebar3 --version

# Ordinarily the way to do this would probably be to use a heredoc. Something like:
#
# ```
# rebar3 hex publish <<EOF
# y
# $LOCAL_PASSWORD
# $LOCAL_PASSWORD
# ```
#
# However there's something funny around (I think) prompts and spawning processes going on,
# where rebar3 will crash if we send it the local password too quickly.
#
mkfifo pipe
rebar3 hex publish < pipe &

# We do this so that we don't send a premature EOF. Otherwise we kill Erlang.
exec 3>pipe

# Tell rebar3 yes, we want to publish.
echo y > pipe

# the mark of a quality piece of code.
sleep 1s

# Give our local password
echo $HEXPM_LOCAL_PASSWORD > pipe

# revenge of the sleep
sleep 30s

# Send the EOF
exec 3>&-

# Tidy up
rm pipe

# Now do it all again for docs
mkfifo pipe
rebar3 hex docs < pipe &
exec 3>pipe
sleep 1s
echo $HEXPM_LOCAL_PASSWORD > pipe
sleep 30s
exec 3>&-
rm pipe
