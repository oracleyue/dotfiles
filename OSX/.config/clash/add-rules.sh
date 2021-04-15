#!/bin/bash
# -*- coding: utf-8 -*-
#
# Add additional rules to clashX config for AmyTelecom.
# Usage:
# - add your rules in "rules.my"
# - run this script to generate the config "my.yaml"
#
# Last modified on 15 Apr 2021

SED="/usr/local/bin/gsed"
prepath="$HOME/.config/clash"

rules="$prepath/rules.my"
in="$prepath/AmyTelecom.yaml"
out="$prepath/my.yaml"
temp="$prepath/rules.temp"

# add rules
$SED 's/^/  - /' $rules > $temp
$SED "/rules:/r $temp" $in > $out

# remove temp files
rm $temp
