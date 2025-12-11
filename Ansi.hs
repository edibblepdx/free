-- Ansi colors

module Ansi where

reset :: String
reset = "\x1b[0m"

black :: String
black = "\x1b[0;30m"

red :: String
red = "\x1b[0;31m"

green :: String
green = "\x1b[0;32m"

yellow :: String
yellow = "\x1b[0;33m"

blue :: String
blue = "\x1b[0;34m"

purple :: String
purple = "\x1b[0;35m"

cyan :: String
cyan = "\x1b[0;36m"

white :: String
white = "\x1b[0;37m"
