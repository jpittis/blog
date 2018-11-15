# Introducing Toxiproxy Haskell

**Mar 10, 2018**

With the release of [toxiproxy-haskell](https://github.com/jpittis/toxiproxy-haskell) on
[Hackage](https://hackage.haskell.org/package/toxiproxy-haskell), I'd like to introduce
the Haskell community to failure testing using Toxiproxy so that they can improve the
resilience of their network connected applications.

## Toxiproxy

Toxiproxy is a cross platform TCP proxy that lets you simulate latency, timeouts,
rejections and other network conditions in your test and development environments.

I think the best way to explain how Toxiproxy works is through an example. Let's create a
proxy to simulate latency between a redis client and server.

We start by running `toxiproxy-server` from a terminal. Toxiproxy exposes an HTTP API on
port 8474.

```bash
$ toxiproxy-server
INFO[0000] API HTTP server starting                      host=localhost port=8474 version=git-fe6bf4f
```

We then create a proxy using `toxiproxy-cli`. We're going to call our proxy `redis` and
instruct Toxiproxy to listen for TCP requests on port 4444 and forward them to the default
redis port 6379.

```bash
$ toxiproxy-cli create redis --listen 127.0.0.1:4444  --upstream 127.0.0.1:6379
Created new proxy redis
```

Finally, let's add a toxic to our proxy. Toxics are used to inject network failures into
a proxy. In this case, we're adding a latency toxic which will add 1 second of latency to
data flowing from the redis server back to the redis client.

```bash
$ toxiproxy-cli toxic add redis --type latency --attribute latency=1000 --attribute jitter=0
Added downstream latency toxic 'latency_downstream' on proxy 'redis'
```

Now we can test it out.

Start a redis server in a second terminal.

```bash
$ redis-server
58968:M 04 Mar 21:13:39.957 * Ready to accept connections
```

When using the redis client, we must specify the port of our proxy rather than the default
redis port. A simple `GET KEY` command will prove that the request takes 1 second to
respond rather than the usual immediate response.

```bash
$ redis-cli -p 4444
127.0.0.1:4444> GET KEY
(nil)
(1.00s)
```

Toxiproxy supports more kinds of network failures than just added latency. The command
line interface has provided us with a simple example to demonstrate how Toxiproxy works.
The rest of this post is going to explain how you can use
[toxiproxy-haskell](https://github.com/jpittis/toxiproxy-haskell) to write failure tests
for your Haskell applications and hopefully make them more resilient to external failures.

## Toxiproxy Haskell

[toxiproxy-haskell](https://github.com/jpittis/toxiproxy-haskell) exposes a simple
[servant](https://hackage.haskell.org/package/servant) HTTP API. Though you're welcome to
use this low level API, I've written three higher level helpers to make working with
Toxiproxy a little easier.

Let's recreate our redis latency example from above using
[hedis](https://hackage.haskell.org/package/hedis) and
[toxiproxy-haskell](https://github.com/jpittis/toxiproxy-haskell).

Here's a function that measures the duration of a set and get to redis.

```haskell
{-# LANGUAGE OverloadedStrings #-}

import qualified Database.Redis as Redis
import qualified Data.Time.Clock.POSIX as Time
import qualified Data.Map.Strict as Map

timeSetGet port = do
  before <- Time.getPOSIXTime

  conn <- Redis.checkedConnect Redis.defaultConnectInfo { Redis.connectPort = port }
  Redis.runRedis conn $ do
    Redis.set "haskell" "is awesome"
    Redis.get "haskell"

  after <- Time.getPOSIXTime
  return $ after - before

timeSetGetNormal = timeSetGet (Redis.PortNumber 6379)
```

We can run it from the repl. Don't forget to have `toxiproxy-server` and `redis-server`
running in the background!

```bash
$ stack repl example.hs
> timeSetGetNormal
0.001758s
```

Now to add some latency with Toxiproxy.

```haskell
import Toxiproxy

timeSetGetWithLatency = do
  let proxy = Proxy
        { proxyName     = "redis"
        , proxyListen   = "127.0.0.1:4444"
        , proxyUpstream = "127.0.0.1:6379"
        , proxyEnabled  = True
        , proxyToxics   = []
        }
  let latency = Toxic
        { toxicName       = "latency"
        , toxicType       = Latency
        , toxicStream     = Downstream
        , toxicToxicity   = 1
        , toxicAttributes = Map.fromList [("latency", 1000), ("jitter", 0)]
        }
  withProxy proxy $ \proxy ->
    withToxic proxy latency (timeSetGet (Redis.PortNumber 4444))
```

And of course, since we're sending two serial commands to redis, with our toxic activated
it takes two seconds.

```bash
$ stack repl example.hs
> timeSetGetWithLatency
2.008837s
```

## Writing Tests to Improve Resilience

How can you use [toxiproxy-haskell](https://github.com/jpittis/toxiproxy-haskell) to
improve your application's resilience?

1. Find an external TCP call that your application makes.
2. Write a test to ensure your application behaves correctly during network failure.

Let's see how [hedis](https://hackage.haskell.org/package/hedis) behaves when redis is
rejecting connections!

For example, let's write a wrapper around the `Redis.get` call that returns a fallback on
failure.

```haskell
import Control.Exception
import Data.ByteString

getWithFallback key fallback port = do
  result <- try run :: IO (Either SomeException
                                  (Either Redis.Reply (Maybe ByteString)))
  case result of
    Left _        -> fallback
    Right success -> return success
   where
    run = do
      conn <- Redis.checkedConnect Redis.defaultConnectInfo { Redis.connectPort = port }
      Redis.runRedis conn (Redis.get key)
```

How would we test this function out to ensure it behaves as we want when redis crashes?
Let's write a Toxiproxy test!

```haskell
main = hspec $
  describe "Redis Resilience" $
    it "getWithFallback returns a default value when redis is down" $ do
      let proxy = Proxy
            { proxyName     = "redis"
            , proxyListen   = "127.0.0.1:4444"
            , proxyUpstream = "127.0.0.1:6379"
            , proxyEnabled  = True
            , proxyToxics   = []
            }
      withProxy proxy $ \proxy ->
        withDisabled proxy $
          getWithFallback "haskell" (return . Right . Just $ "no") (Redis.PortNumber 4444)
            `shouldReturn` (Right . Just $ "no")
```

When implementing and using libraries that perform external calls, tests similar to the
one shown above can be used to ensure correct behaviour during network failure.

## Avoiding Regression

Though testing with `toxiproxy-cli` or one off scripts is better than nothing, generally
we want our Toxiproxy tests to be permanent. Writing an automatable hspec Toxiproxy test
is demonstrated above. This means that your test will be run on every release of your
application to avoid regression.

## What Toxiproxy Isn't

It's important to realize that Toxiproxy is a layer 4 proxy. It does not deal in IP
packets but instead at the TCP stream level. This makes it difficult to simulate timeouts
related to TCP connection establishment and time until `accept(2)`.

Toxiproxy is not currently protocol aware. It does not understand what HTTP is, instead it
just forwards the bytes. This can make it hard to test HTTPS because a client will think
it's connecting to Toxiproxy on host A but it will receive a certificate for the upstream
host B. Currently, this issue must be handled by the client.

Toxiproxy should not be used in production. It's only designed for use in linux, OSX and
windows test and development environments. I would not recommend it to be used to proxy
production traffic.

## Thanks

It would be great if the Haskell community could start using tools like Toxiproxy to
improve our applications resilience. The Haskell Toxiproxy client is only about 300 lines
of code. Pull requests and questions are welcome on Github or by email!

There's nothing worse than finding out how your application reacts to a network blip in
production! Find out in development and test using Toxiproxy!
