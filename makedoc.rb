#! /usr/bin/env ruby

puts `erl -noinput -pa ebin/ -eval "doc:make_doc()" -s init stop`


