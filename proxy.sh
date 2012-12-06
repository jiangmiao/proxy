#!/bin/sh

erlc proxy.erl && erl -noshell -s proxy $@
