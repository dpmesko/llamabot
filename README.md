# llamabot

llamabot helps you recognize achievements and give thanks to your Slack workspace buddies in a fun way!

Give a shout-out to someone in any channel that llamabot is in, and include some :llama: emojis.

```
@stevejobs thanks for the sick phone! :llama :llama :llama
```

Just like that, I awarded @stevejobs 3 llamas. Every user has a limited number to give each day, and the app keeps a leaderboard that can be accessed by users and used to do special shoutouts, automated weekly summaries, and maybe more in the future!

## Configuring the app in Slack

### App Management Dashboard

### Your Slack Workspace


## Running llamabot
llamabot is written in Haskell and compiled with `stack` (using `cabal`, `hpack`, and `GHC`). 

1) Run `stack build`. This will build all dependencies and source packages and compile an executable which it usually puts in `~/llamabot/.stack-work/install/<yourOS>/<someVersionNumber>/bin/llamabot`. You can see how mine looks in a little helper script I wrote called `build.sh`. Later I'll make a real build script that finds it, wherever it goes.

2) Run `llamabot` with your app's OAuth token
```
./llamabot --token=yourToken
```
