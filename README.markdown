# cl-temporal

super WIP. Using this in any production environment would be foolish.

## Usage

- don't!


## Installation

- good luck!
- requirements
  - [qlot](https://github.com/fukamachi/qlot)
  - [grpc dev libraries](https://pkgs.org/search/?q=grpc)
  - [protobuf dev libraries](https://pkgs.org/search/?q=protobuf)
  - SBCL
    - I'm using SBCL but CCL will maybe work. I have no idea about other implementations

- build the grpc.so file
  - `make -C .bundle-libs/software/grpc-*`
- load it into your lisp as normal

## TODO

- [x] macro for defining workflows
- [x] poll workflow task queue
- [x] execute workflow when one is pulled from task queue
- [ ] WRITE TESTS AND ENSURE THEY WORK
- [ ] CI/CD
- [ ] tell temporal when workflow execution starts and finishes (successfully or not)
- [ ] poll activity task queue
- [ ] execute an activity
- [ ] tell temporal when activity execution starts and finishes (successfully or not)
- [ ] timers
- [ ] Codec server?
- [ ] interceptor?
- [ ] ~~adapations of temporals courses (kidding too much work)~~
- [ ] PR changes to [cl-grpc](https://github.com/qitab/grpc) and [cl-protobufs](https://github.com/qitab/cl-protobufs)
  - [x] ~~[deadline support in cl-grpc](https://github.com/qitab/grpc/issues/68)~~ [here](https://github.com/qitab/grpc/pull/69)
  - [x] ~~figure out [how to make the full import path work in cl-protobufs](https://github.com/qitab/cl-protobufs/issues/431) (my current fix is just deleting the `then` clause from the `if` expression lol)~~ [here](https://github.com/qitab/cl-protobufs/pull/437)
  - [x] ~~[use updated protobufs versions](https://github.com/qitab/cl-protobufs/issues/402). can continue on top of [this](https://github.com/qitab/cl-protobufs/pull/434)~~ [here](https://github.com/qitab/cl-protobufs/pull/436)
  - [ ] more will probably show up
- [ ] use http api instead of grpc to save myself a cubic buttload of headaches

## Thanks

- qitab for maintaining libraries for gRPC with common lisp
  - google for buying ITA and not having an issue with them continuing to use common lisp
  
- coinbase for having their own, much easier to follow sdk
  - lots of this codebase is based on [coinbase/temporal-ruby](https://github.com/coinbase/temporal-ruby)
  
- sbcl developers for constantly and consistently iterating on it
  
## Pains

- both the protobuf and grpc common lisp implementations seem to be incomplete in some way. My best guess with how they work is that they were made for a specific purpose and then open sourced, so some features are missing or probably don't work properly
  - The issues are pretty easy to work around so it's not the end of the world. It's room for contributions, after all
  - I might take a crack at using [autowrap](https://github.com/rpav/cl-autowrap) for grpc. It'll probably make things easier to just directly use the API instead of letting it get in my way (unfortunately)
    - In my opinion, this is just what should be used for the cl-grpc anyway, with some helper CL-based helper functions on top of it
- [the sole existence of grpc](https://youtu.be/3t6L-FlfeaI)
  - it definitely solves the problems it was designed to solve, and it's probably beneficial over just HTTP for temporal, it's just less ubiquitous than standard http and that's a pain
  - I'm _pretty sure_ I can do protobuf over bare http (or json over bare http) so if it pisses me off enough that's probably what I'll switch to
- reverse engineering the sequence of messages to send to temporal
  - some of it is pretty straightforward conceptually ([like this diagram](https://docs.temporal.io/workflows#event-loop)) but that still doesn't tell me the client-server request-response order.
  - This is OK and it could be worse, but it's going to be a pain from start to finish

## why the hell would you do this

- what if someone needs to connect their common lisp service to temporal
  - there are dozens of us, im telling you
- if I don't write it, who's gonna?
- My bosses won't let me write common lisp at work so I need to get my fix somewhere else
- the thing that's really holding common lisp back is that there isn't a temporal client library. Once that's implemented, it's going to take off.

## License

BSD-3
