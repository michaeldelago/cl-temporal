# cl-temporal

super WIP. Using this in any production environment would be foolish.

## Usage

- don't!

## Installation

- good luck!
- use qlot

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
- [ ] adapations of temporals courses
- [ ] PR changes to [cl-grpc](https://github.com/qitab/grpc) and [cl-protobufs](https://github.com/qitab/cl-protobufs)
  - [ ] [deadline support in cl-grpc](https://github.com/qitab/grpc/issues/68)
  - [ ] figure out [how to make the full import path work in cl-protobufs](https://github.com/qitab/cl-protobufs/issues/431) (my current fix is just deleting the `then` clause from the `if` expression lol)
  - [ ] more will probably show up
- [ ] use http api instead of grpc to save myself a cubic buttload of headaches

## Thanks

- @qitab for maintaining libraries for gRPC with common lisp
  - google for buying ITA and not having an issue with them continuing to use common lisp
  
- @coinbase for having their own, much easier to follow sdk
  - lots of this codebase is based on [coinbase/temporal-ruby](https://github.com/coinbase/temporal-ruby)
  
- sbcl developers for constantly and consistently iterating on it
  
## Pains

- both the protobuf and grpc common lisp implementations seem to be incomplete in some way. My best guess with how they work is that they were made for a specific purpose and then open sourced, so some features are missing or probably don't work properly
  - The issues are pretty easy to work around so it's not the end of the world. It's room for contributions, after all
- [the sole existence of grpc](https://youtu.be/3t6L-FlfeaI)
- reverse engineering the sequence of messages to send to temporal

## why the hell would you do this

- what if someone needs to connect their common lisp service to temporal
  - there are dozens of us, im telling you
- if I don't write it, who's gonna?
- My bosses won't let me write common lisp at work so I need to get my fix somewhere else


## License

BSD-3
