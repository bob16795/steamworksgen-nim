# steamworksgen

## Usage

Use the `generateSteamworksAPI(jsonPath: string)` proc to read in an export the api calls from steams sdk see [the steamworks nimble package](https://github.com/treeform/steamworks) for a more general API setup overview.

## Differences from the flat API

### Callbacks

```nim
# The api is init as needed so you dont need to worry about initializing anything

# Interface accessors are wrapped up in simple procs so no need for parenthesis
let userId = steamUser.getSteamID()

# When you call a function that returns a APICallback type, itll return a distinct handle.
# You can then use this handle to add a callback for when the call completes

steamUserStats
  .requestUserStats(userId)
  .onDone do (failed: bool, recieved: ptr UserStatsRecieved) {.stdcall.} =
    discard
    # handle user stats get

# callbacks are handled per frame when you need
steamUtils.runFrame()
```
