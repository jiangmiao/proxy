#!/bin/sh

erlc proxy.erl && erl -noshell -detached -s proxy $@
