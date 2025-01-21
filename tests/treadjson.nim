import steamworksgen

import unittest, os, osproc
import macros

expandMacros:
  generateSteamAPI("steam_api.json")

suite "gen":
  test "main":
    discard
