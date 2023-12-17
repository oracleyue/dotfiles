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
out="$prepath/myAmy.yaml"
temp="$prepath/rules.temp"

# add rules
$SED 's/^/  - /' $rules > $temp
$SED "/rules:/r $temp" $in > $out

# allow ipv6 when proxy is on
$SED -i 's/^ipv6: false/ipv6: true/' $out

# remove temp files
rm $temp
