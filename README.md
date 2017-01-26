# slackobot
An ocaml slackbot


## Features

Not much at the moment:
  * `slackobot message` will post some text to slack.
  * `slackobot mesos-capacity` will query a Mesos cluster for capacity
    information and post it to slack.


## Getting set up

You will need:
  * A clone of this repo.
  * `ocaml` (4.02 or later) and `opam`.

Set up an environment:
  * Create a fresh opam switch:
    `opam switch -A system slackobot; eval $(opam config env)`
  * Pin and install `slacko` from github (we need slack attachment support):
    `opam pin add slacko git://github.com/Leonidas-from-XIV/slacko.git`
  * Pin and install your local slackobot clone:
    `opam pin add slackobot .`

Create a slack test token at https://api.slack.com/docs/oauth-test-tokens and,
if necessary, build a [Mesos](https://mesos.apache.org/)
or [DC/OS](https://dcos.io/) cluster so you have something to query.

Run your newly installed `slackobot`:
```
export SLACKOBOT_API_TOKEN='xoxp-sooper-seekrit-token'
export SLACKOBOT_USERNAME='slackobot'
export SLACKOBOT_CHANNEL='#slackobot'
slackobot message 'Hello humans, I am here to help.'
slackobot mesos-capacity http://cluster.example.com:5050/ --cluster-name ayaks
```

## Hacking

If you want to hack on slackobot:
  * You probably don't want it installed:
    `opam uninstall slackobot`
  * You probably want test dependencies:
    `opam install ounit qcheck`
  * Configure, build, run the tests:
    `./configure --enable-tests && make && make test`

Now run your freshly built version:
```
export SLACKOBOT_API_TOKEN='xoxp-sooper-seekrit-token'
export SLACKOBOT_USERNAME='slackobot'
export SLACKOBOT_CHANNEL='#slackobot'
./slackobot.native message 'What exciting new features might I have?'
./slackobot.native mesos-capacity http://localhost:5050/ --cluster-name snowbird
```
