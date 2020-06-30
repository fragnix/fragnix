# fragnix-browse Frontend
A code fragment (slice) editor written in Elm.
This folder contains the frontend, which communicates with the backend-server in `fragnix-browse.hs`.

Normally, there should be a compiled Javascript and an index.html in `/dist`, these will be statically included into the `fragnix-browse` binary when you `cabal install` the fragnix package.

The types in `Slice.Elm` and `LocalSlice.elm` mirror the types defined in the corresponding Haskell modules and need to be adjusted manually if there are changes.

If you want to change something, I recommend editing the Modules in `src` and use the `./optimize.sh` helper script (or use it as an instruction if you are on Windows).

To compile the code, `elm` is needed, to make it smaller and faster, `uglifyjs` is [recommended](https://github.com/elm/compiler/blob/9d97114702bf6846cab622a2203f60c2d4ebedf2/hints/optimize.md).

## Requirements
 - elm: `yarn global add elm` or `npm i -g elm`
 - uglifyjs: `yarn global add uglify-js` or `npm i -g uglify-js`

## Build instructions
Compile elm code with `./optimize.sh`

<!--
## Elm-UI
A small example why elm ui is helpful:
https://ellie-app.com/6GcbZwngp4Da1
-->
