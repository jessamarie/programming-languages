#!/bin/bash

erl -compile main
erl -sname example4@localhost -setcookie thecookiemonster -pa ./main.erl -run main -run init stop -noshell


