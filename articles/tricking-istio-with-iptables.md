# Tricking Istio into ignoring inbound packets

**Nov 20, 2018**

This post documents my adventure trying to get Istio to ignore inbound packets on a certain port. (I don't recommend doing this in production!) It ended up teaching me a bit about iptables and how Istio uses them to force all inbound and outbound traffic through it's proxy.

## What's Istio and how does it relate to iptables?

Istio is a Service Mesh which you can learn about [here](https://istio.io/docs/concepts/what-is-istio/).

A key idea of a service mesh is to stick a proxy in front of every application and force all inbound and outbound network traffic to flow through this proxy. This means all traffic automatically benefits off the features provided by this proxy. Istio uses a proxy called Envoy which sits in front of every application a bit like this:

<img src="https://github.com/jpittis/blog/raw/master/static/proxy.png" alt="Istio proxy intercepts all inbound IP traffic for the pod." style="max-width:30em;"/>

In Istio's case, this "forcing" of inbound and outbound network traffic to Envoy is accomplished using iptables.

## Figuring out what iptables rules Istio uses.

We're running Istio in Kubernetes where our applications run in separate Linux namespaces. Since iptables rules are namespaced we should be able to quickly find out what rules Istio added to our application's pod using the `iptables --list` command from within the pod.
  
Let's exec into our application's proxy and find out!  

```  
kubectl exec <pod> --container istio-proxy -it -- /bin/bash 
$ iptables --list  
iptables v1.6.0: can't initialize iptables table `filter': Permission denied (you must be root)
Perhaps iptables or your kernel needs to be upgraded.  
```

Oh no! I'm pretty sure this error occurs because the pod doesn't have the `NET_ADMIN` capability.

A quick `kubectl describe` shows that the `istio-proxy` pod doesn't have the `NET_ADMIN` capability. How the heck did Istio manage to add iptables rules then?

If you scan through the pod's init containers, you'll see that Istio created an init container with the `NET_ADMIN` capability. That's how it managed to muck with iptables rules!

You can find the iptables related script that runs in the Istio's init container [here](https://github.com/istio/istio/blob/ec7f44dc672830d47f46ca0c4b6a84b5e0188575/tools/deb/istio-iptables.sh).

In my case, rather than giving the pod the right privilege, I decided to ssh into the glcoud node it was running on and enter the namespace of the pod using `nsenter`.

```
$ gcloud compute ssh <node>
$ docker ps  
...
<container-id> ...
...
$ docker inspect -f '{{ .State.Pid }}' <container-id>  
<pid>
sudo nsenter --target <pid> --uts --ipc --net --pid  
```  

Istio creates it's rules in the `nat` table so we can list just that table with `-t nat`.

This time we get some useful output!  
  
```  
# iptables -t nat --list
Chain PREROUTING (policy ACCEPT)
target     prot opt source               destination
ISTIO_INBOUND  tcp  --  anywhere             anywhere

Chain INPUT (policy ACCEPT)
target     prot opt source               destination

Chain OUTPUT (policy ACCEPT)
target     prot opt source               destination
ISTIO_OUTPUT  tcp  --  anywhere             anywhere

Chain POSTROUTING (policy ACCEPT)
target     prot opt source               destination

Chain ISTIO_INBOUND (1 references)
target     prot opt source               destination
ISTIO_IN_REDIRECT  tcp  --  anywhere             anywhere             tcp dpt:http-alt
ISTIO_IN_REDIRECT  tcp  --  anywhere             anywhere             tcp dpt:8888

Chain ISTIO_IN_REDIRECT (2 references)
target     prot opt source               destination
REDIRECT   tcp  --  anywhere             anywhere             redir ports 15001

Chain ISTIO_OUTPUT (1 references)
target     prot opt source               destination
ISTIO_REDIRECT  all  --  anywhere            !localhost
RETURN     all  --  anywhere             anywhere             owner UID match 1337
RETURN     all  --  anywhere             anywhere             owner GID match 1337
RETURN     all  --  anywhere             localhost
ISTIO_REDIRECT  all  --  anywhere             anywhere

Chain ISTIO_REDIRECT (2 references)
target     prot opt source               destination
REDIRECT   tcp  --  anywhere             anywhere             redir ports 15001
```

## Following inbound packets.

The above shows iptables rules that are applied to packets when they arrive and leave the pod.

Because we're interested in inbound packets only, we can start at the `PREROUTING` rule.

```  
Chain PREROUTING (policy ACCEPT)
target     prot opt source               destination
ISTIO_INBOUND  tcp  --  anywhere             anywhere
```  
  
This says take all TCP traffic and apply the `ISTIO_INBOUND` rules.
  
So let's go see what `ISTIO_INBOUND` does.

```  
Chain ISTIO_INBOUND (1 references)
target     prot opt source               destination
ISTIO_IN_REDIRECT  tcp  --  anywhere             anywhere             tcp dpt:http-alt
ISTIO_IN_REDIRECT  tcp  --  anywhere             anywhere             tcp dpt:8888 
```  
  
`http-alt` in this case is another way of saying port `8080`. This rule says take all TCP traffic with a destination port of `8080` or `8888` and apply the `ISTIO_IN_REDIRECT` rule.

Why port `8080` and `8888`? Istio saw that our Kubernetes pod is exposing those two ports so it's setup iptables rules for both of them automatically.

```  
Chain ISTIO_IN_REDIRECT (2 references)
target     prot opt source               destination
REDIRECT   tcp  --  anywhere             anywhere             redir ports 15001
```  
  
`ISTIO_IN_REDIRECT` is even simpler. It just says to take all TCP traffic and redirect it to port `15001`!

Guess what port Envoy is running on!?

If you guessed `15001`, you're right!

So in summary, these rules are saying: Take all inbound TCP traffic for the listening ports of this Kubernetes pod and redirect them to Envoy!

## Tricking Istio!

So now we know `ISTIO_IN_REDIRECT` is redirecting our inbound traffic to the proxy. All we need to do is inject a rule that runs before it and `RETURN`s the packet early, skipping the redirect.

Here's the command to add our new rule:

```  
iptables -t nat -I PREROUTING -p tcp --dport 8888 -j RETURN
```  
  
I only want to trick Istio into not intercepting packets for port `8888`. I still want `8080` to go through the proxy. So I've selected a destination port of `8888`.

`-I PREROUTING` means to prepend the rule to the front of the `PREROUTING` chain. [Istio's script uses -A](https://github.com/istio/istio/blob/ec7f44dc672830d47f46ca0c4b6a84b5e0188575/tools/deb/istio-iptables.sh#L250) which appends it's rule to the end of `PREROUTING`. By using `-I`, we can ensure our rule runs before Istio's rule. 
  
If you list the `nat` table again, you'll find our rule has been added!

```  
Chain PREROUTING (policy ACCEPT)
target     prot opt source               destination
RETURN     tcp  --  anywhere             anywhere             tcp dpt:8888
ISTIO_INBOUND  tcp  --  anywhere             anywhere  
```  

In my case, I wanted to add this rule automatically to a bunch of Kubernetes pods. The easiest thing for me to do at the time was to create my own init container that ran our sneakily crafted iptables rule from above. Don't forget to give the init container `NET_ADMIN` so it's allowed to use iptables. (Make sure you're using a container base image that has the `iptables` command. I used `ubuntu:xenial` because that's what the Istio init container uses.)

```  
initContainers:
- name: jakes-sneaky-init-container
  image: <an-image-that-runs-our-script>
  securityContext:
    capabilities:
      add:
      - NET_ADMIN  
```

And there we have it! A quick reminder that I don't recommend making these changes to production Istio. The advantages of a service mesh are best met when all traffic flows through its proxies. (I'm thinking of security features like mTLS in this case but I'm sure there are lots of other reasons.)
