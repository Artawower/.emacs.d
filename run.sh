#!/bin/sh

cp ./config.org ~/.emacs.d/
emacs -q -l ./init.el
