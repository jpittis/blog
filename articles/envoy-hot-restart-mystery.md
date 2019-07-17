# Envoy Hot Restart Mystery

**Apr 21, 2019**

I've been working on a tool that requires a way to reset the configuration of a local
development Envoy. The Envoy admin's `/quitquitquit` endpoint combined with the
`docker-compose` `restart: always` parameter does the trick but it's quite slow.
Fourtunately Envoy has a "hot restart" feature that can reset an Envoy's configuration
without the slow container restart.

We start Envoy using the provided
[envoy/restarter/hot-restarter.py](https://github.com/envoyproxy/envoy/blob/master/restarter/hot-restarter.py)
script which will restart Envoy when it receives a `SIGHUP`.

```
hot-restarter.py start-envoy.sh
```

Don't forget the include the `$RESTART_EPOCH` flag in your `start-envoy.sh` script.

```
--restart-epoch $RESTART_EPOCH
```

We can then send a `SIGHUP` signal to the container which will trigger a hot restart.

```
$ docker kill --signal=SIGHUP <your-proxy-container>
```

And now we can check the `restart_epoch` before and after the `SIGHUP` to see if Envoy has
restarted.

```
$ curl -s localhost:9901/server_info | grep restart_epoch
  "restart_epoch": 0,
$ docker kill --signal=SIGHUP <your-proxy-container>
$ curl -s localhost:9901/server_info | grep restart_epoch
  "restart_epoch": 1,
```

## The Mystery

My tool doesn't use `curl` to query Envoy's `restart_epoch`. Instead, I use Go's `http`
package and a bit of protobuf parsing. Something along the lines of this:

```go
client := &http.Client{
    Timeout: time.Second,
}
resp, err := client.Get("http://127.0.0.1:9901/server_info")
if err != nil {
    return nil, err
}
defer resp.Body.Close()

var serverInfo v2alpha.ServerInfo
err = jsonpb.Unmarshal(resp.Body, &serverInfo)
return &serverInfo, err
```

The mystery is that if I use the above code to query the restart epoch before and after a
hot restart, the client takes an absurdly long time to show the updated restart epoch.
(Think 20 seconds.) I can `curl` for the restart epoch and get the bumped epoch version
while my Go client is still returning the old one.

I suspect this is some issue where my Go client is still connected to the old Envoy rather
than the new one. Not sure how to fix this yet. Currently I'm shelling out to `curl`. I'll
update this post when I've found a less hacky solution.
