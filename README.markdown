## Alice

Official release post: <a href="http://willcodeforfoo.com/2009/07/13/announcing-alice/">http://willcodeforfoo.com/2009/07/13/announcing-alice/</a>

<p><a href="http://alicetheapp.com">http://alicetheapp.com</a>.</p>

As a queue server, <a href="http://www.rabbitmq.com/">RabbitMQ</a> is super cool, but my company is hesitant to use it without a nice front-end or access to statistics about the server. So we set out to develop the latest RabbitMQ REST interface, <a href="http://github.com/auser/alice">Alice</a>.

Alice is a RESTful interface to the RabbitMQ server that talks directly through erlang's native interface, <a href="http://www.erlang.org/doc/man/epmd.html">epmd</a>. The purely <a href="http://en.wikipedia.org/wiki/Representational_State_Transfer">RESTful</a> server responds to the same interface as the RabbitMQ's command-line interface and presents a native HTTP interface to the data. <a href="http://alicetheapp.com">Alice</a> is written with <a href="http://code.google.com/p/mochiweb/">Mochiweb</a>.

## Quickstart

How to get started.

<pre><code>
git clone git://github.com/auser/alice.git
cd alice
./start.sh
</code></pre>

You can pass a rabbithost where your rabbitmq-server sits by passing the options `rabbithost` in the command-line, like so:

<pre><code>
  ./start.sh -alice rabbithost "other.node.come"
</code></pre>

Note, you may have to set your cookie when starting Alice with a remote node by the -setcookie flag:

<pre><code>
  ./start.sh -alice rabbithost "other.node.com" -setcookie "mysecretcookie"
</code></pre>

<pre><code>
## Currently exposed RESTful routes
  /conn - Current connection information
  /exchanges - Current exchanges information
  /queues - Current queues
  /users - Current users
  /bindings - Current bindings
  /control - Access to the RabbitMQ control
  /permissions - Current permissions
  /vhosts - Current vhosts
</code></pre>

These endpoints all are exposed with the four verbs (get, post, put, delete) and respond in the <a href="http://www.json.org/">JSON</a> format, (except the root / endpoint which responds with text/html).

## Usage

## Users

<pre><code>
# List users
curl -i http://localhost:9999/users 
HTTP/1.1 200 OK
Server: MochiWeb/1.0 (Any of you quaids got a smint?)
Date: Tue, 04 Aug 2009 07:08:20 GMT
Content-Type: text/json
Content-Length: 19

{"users":["guest"]}

# Viewing a specific user
curl -i http://localhost:9999/users/guest
HTTP/1.1 200 OK
Server: MochiWeb/1.0 (Any of you quaids got a smint?)
Date: Tue, 04 Aug 2009 08:01:01 GMT
Content-Type: text/json
Content-Length: 17

{"users":"guest"}

# If the user is not a user:
curl -i http://localhost:9999/users/bob  
HTTP/1.1 400 Bad Request
Server: MochiWeb/1.0 (Any of you quaids got a smint?)
Date: Tue, 04 Aug 2009 08:01:20 GMT
Content-Type: text/json
Content-Length: 20

{"bob":"not a user"}

# Add a user
curl -i -XPOST \
        -d'{"username":"ari", "password":"weak password"}' \
        http://localhost:9999/users
        
HTTP/1.1 200 OK
Server: MochiWeb/1.0 (Any of you quaids got a smint?)
Date: Thu, 16 Jul 2009 00:10:35 GMT
Content-Type: text/json
Content-Length: 25

{"users":["ari","guest"]}

# Deleting a user
curl -i -XDELETE  http://localhost:9999/users/ari
HTTP/1.1 200 OK
Server: MochiWeb/1.0 (Any of you quaids got a smint?)
Date: Tue, 04 Aug 2009 07:19:24 GMT
Content-Type: text/json
Content-Length: 19

{"users":["guest"]}
</code></pre>

Notice that when we list the user that doesn't exist, bob from the second example above, the return is a 400. This is especially useful when you want to access the data programmatically. More on extending Alice below and how to get access to the return value of the requested route.

The same basic usage is applied to all the routes listed, as you can see:

## Connections
<pre><code>
# List connections
curl -i http://localhost:9999/conn
HTTP/1.1 200 OK
Server: MochiWeb/1.0 (Any of you quaids got a smint?)
Date: Tue, 04 Aug 2009 07:30:52 GMT
Content-Type: text/json
Content-Length: 287

{"conn":[{"pid":"...","ip":"127.0.0.1","port":"5672","peer_address":"127.0.0.1" ...}]}
</code></pre>

## Exchanges
<pre><code>
# List the current exchanges
curl -i http://localhost:9999/exchanges
HTTP/1.1 200 OK
Server: MochiWeb/1.0 (Any of you quaids got a smint?)
Date: Tue, 04 Aug 2009 07:34:14 GMT
Content-Type: text/json
Content-Length: 654

{"exchanges":[{"name":"amq.rabbitmq.log","type":"topic","durable":"true","auto_delete":...}
</code></pre>

## Queues
<pre><code>
# List the current queues
curl -i http://localhost:9999/queues   
HTTP/1.1 200 OK
Server: MochiWeb/1.0 (Any of you quaids got a smint?)
Date: Tue, 04 Aug 2009 07:35:42 GMT
Content-Type: text/json
Content-Length: 60

{"queues":[{"memory":"212988","name":"noises","vhost":"/"}]}
</code></pre>

## Bindings
<pre><code>
# List the current bindings
curl -i http://localhost:9999/bindings
HTTP/1.1 200 OK
Server: MochiWeb/1.0 (Any of you quaids got a smint?)
Date: Tue, 04 Aug 2009 07:36:13 GMT
Content-Type: text/json
Content-Length: 69

{"bindings":[{"queue":"noises","exchange":"","from_queue":"noises"}]}
</code></pre>

## Permissions
<pre><code>
# List permissions
curl -i http://localhost:9999/permissions
HTTP/1.1 200 OK
Server: MochiWeb/1.0 (Any of you quaids got a smint?)
Date: Tue, 04 Aug 2009 07:37:32 GMT
Content-Type: text/json
Content-Length: 42

{"permissions":{"vhosts":[{"name":"/", "users":[{"name":"guest","configure":".*","read":".*","write":".*"}]}]}}

# You can list permissions for a user
curl -i http://localhost:9999/permissions/amr
HTTP/1.1 200 OK
Server: MochiWeb/1.0 (Any of you quaids got a smint?)
Date: Tue, 04 Aug 2009 07:50:33 GMT
Content-Type: text/json
Content-Length: 42

{"permissions":{"name":"guest","vhosts":[{"name":"/","configure":".*","write":".*","read":".*"}]}}

# You can list permissions on a vhost too
curl -i http://localhost:9999/permissions/vhost/root
HTTP/1.1 200 OK
Server: MochiWeb/1.0 (Any of you quaids got a smint?)
Date: Tue, 04 Aug 2009 07:50:33 GMT
Content-Type: text/json
Content-Length: 42

{"permissions":{"name":"/","users":[{"name":"guest","configure":".*","write":".*","read":".*"}]}}

# Setting permissions
curl -i -XPOST -d '{"vhost":"/", "configure":".*", "read":".*", "write":".*"}' \
  http://localhost:9999/permissions/guest
HTTP/1.1 200 OK
Server: MochiWeb/1.0 (Any of you quaids got a smint?)
Date: Tue, 04 Aug 2009 07:55:33 GMT
Content-Type: text/json
Content-Length: 38

{"permissions":{"name":"guest","vhosts":[{"name":"/","configure":".*","write":".*","read":".*"}]}}

# Deleting permissions
curl -i -XDELETE -d '{"vhost":"/"}' http://localhost:9999/permissions/guest
HTTP/1.1 200 OK
Server: MochiWeb/1.0 (Any of you quaids got a smint?)
Date: Tue, 04 Aug 2009 07:55:33 GMT
Content-Type: text/json
Content-Length: 38

{"permissions":{"name":"guest","vhosts":[]}}
</code></pre>

## Vhosts
<pre><code>
# List vhosts
curl -i http://localhost:9999/vhosts
HTTP/1.1 200 OK
Server: MochiWeb/1.0 (Any of you quaids got a smint?)
Date: Tue, 04 Aug 2009 07:57:10 GMT
Content-Type: text/json
Content-Length: 16

{"vhosts":["/"]}

# Viewing a specific vhost
curl -i http://localhost:9999/vhosts/barneys%20list
HTTP/1.1 200 OK
Server: MochiWeb/1.0 (Any of you quaids got a smint?)
Date: Tue, 04 Aug 2009 07:59:29 GMT
Content-Type: text/json
Content-Length: 25

{"vhosts":"barneys list"}

# If it doesn't exist:
curl -i http://localhost:9999/vhosts/barneys%20listings
HTTP/1.1 400 Bad Request
Server: MochiWeb/1.0 (Any of you quaids got a smint?)
Date: Tue, 04 Aug 2009 07:59:59 GMT
Content-Type: text/json
Content-Length: 34

{"barneys listings":"not a vhost"}

# Add a vhost
curl -i http://localhost:9999/vhosts -XPOST -d'{"name":"barneys list"}'
HTTP/1.1 200 OK
Server: MochiWeb/1.0 (Any of you quaids got a smint?)
Date: Tue, 04 Aug 2009 07:58:09 GMT
Content-Type: text/json
Content-Length: 31

{"vhosts":["/","barneys list"]}

# Delete a vhost
curl -XDELETE -i http://localhost:9999/vhosts/barneys%20list
HTTP/1.1 200 OK
Server: MochiWeb/1.0 (Any of you quaids got a smint?)
Date: Tue, 04 Aug 2009 08:02:44 GMT
Content-Type: text/json
Content-Length: 16

{"vhosts":["/"]}
</code></pre>

Now, there is a module in the Alice called control. There are a lot of routes and a lot of functionality built-in here, so let's dig in.

### Control

<pre><code>
# Getting the status of the server
curl -i http://localhost:9999/control 
HTTP/1.1 200 OK
Server: MochiWeb/1.0 (Any of you quaids got a smint?)
Date: Tue, 04 Aug 2009 08:05:19 GMT
Content-Type: text/json
Content-Length: 151

{"status":[{"applications":["rabbit","mnesia","os_mon","sasl","stdlib","kernel"], \
"nodes":["rabbit@YPCMC05591"],"running_nodes":["rabbit@YPCMC05591"]}]}

# Stopping the rabbitmq-server
curl -XPOST -i http://localhost:9999/control/stop  
HTTP/1.1 200 OK
Server: MochiWeb/1.0 (Any of you quaids got a smint?)
Date: Tue, 04 Aug 2009 08:06:02 GMT
Content-Type: text/json
Content-Length: 20

{"status":"stopped"}

# Starting the rabbitmq-server application
curl -XPOST -i http://localhost:9999/control/start_app
HTTP/1.1 200 OK
Server: MochiWeb/1.0 (Any of you quaids got a smint?)
Date: Tue, 04 Aug 2009 08:06:50 GMT
Content-Type: text/json
Content-Length: 20

{"status":"started"}

# Stopping the rabbitmq-server application
curl -XDELETE -i http://localhost:9999/control/stop_app
HTTP/1.1 200 OK
Server: MochiWeb/1.0 (Any of you quaids got a smint?)
Date: Tue, 04 Aug 2009 08:15:56 GMT
Content-Type: text/json
Content-Length: 20

{"status":"stopped"}

# Reset the rabbitmq-server application
curl -XPOST -i http://localhost:9999/control/reset    
HTTP/1.1 200 OK
Server: MochiWeb/1.0 (Any of you quaids got a smint?)
Date: Tue, 04 Aug 2009 08:07:15 GMT
Content-Type: text/json
Content-Length: 18

{"status":"reset"}

# Or force-resetting the server
curl -XPOST -i http://localhost:9999/control/force_reset
HTTP/1.1 200 OK
Server: MochiWeb/1.0 (Any of you quaids got a smint?)
Date: Tue, 04 Aug 2009 08:07:27 GMT
Content-Type: text/json
Content-Length: 18

{"status":"reset"}

# Clustering a set of nodes
curl -XPOST -i http://localhost:9999/control/cluster -d'{"nodes":["bob@otherhost"]}'
HTTP/1.1 200 OK
Server: MochiWeb/1.0 (Any of you quaids got a smint?)
Date: Tue, 04 Aug 2009 08:14:10 GMT
Content-Type: text/json
Content-Length: 20

{"status":"cluster"}

# Rotating rabbit logs
curl -XPOST -i http://localhost:9999/control/rotate_logs -d'{"prefix":"mn_"}'
HTTP/1.1 200 OK
Server: MochiWeb/1.0 (Any of you quaids got a smint?)
Date: Tue, 04 Aug 2009 08:15:12 GMT
Content-Type: text/json
Content-Length: 25

{"status":"rotated_logs"}
</code></pre>


## Extending
Alice is written with the intention of being highly extensible and makes it easy to do so. The controllers respond only to the four verbs with pattern-matching on the routes. 

For instance, a very basic controller looks like this:

<pre><code>
-module (say).
-export ([get/1, post/2, put/2, delete/2]).

get([]) -> {"hello", <<"world">>};
get(_Path) -> {"error", <<"unhandled">>}.

post(_Path, _Data) -> {"error", <<"unhandled">>}.
put(_Path, _Data) -> {"error", <<"unhandled">>}.
delete(_Path, _Data) -> {"error", <<"unhandled">>}.
</code></pre>

There are the 4 RESTful verbs that the controller responds. Now, if you were to compile this in Alice (in src/rest_server/controllers), then the route http://localhost:9999/say would now be accessible. Cool!

<pre><code>
curl -i http://localhost:9999/say
HTTP/1.1 200 OK
Server: MochiWeb/1.0 (Any of you quaids got a smint?)
Date: Tue, 04 Aug 2009 08:20:57 GMT
Content-Type: text/json
Content-Length: 17

{"hello":"world"}
</code></pre>

Now let's add a route to say hello to someone:

<pre><code>
-module (say).
-export ([get/1, post/2, put/2, delete/2]).

get([Name]) -> {"hello", erlang:list_to_binary(Name)};
get([]) -> {"hello", <<"world">>};
% ....
</code></pre>

<pre><code>
curl -i http://localhost:9999/say/ari
HTTP/1.1 200 OK
Server: MochiWeb/1.0 (Any of you quaids got a smint?)
Date: Tue, 04 Aug 2009 08:21:54 GMT
Content-Type: text/json
Content-Length: 15

{"hello":"ari"}
</code></pre>

Finally, with every other verb than get, we are given data to extract. Let's see how to pull some data in a post. The data is given as a proplist with binary keys, we it's pretty easy to pull them out:

<pre><code>
% ...
post([], Data) ->
  Name = erlang:binary_to_list(proplists:get_value(<<"name">>, Data)),
  {"hello back", erlang:list_to_binary(Name)};
post([]) -> 
% ...
</code></pre>

Let's check it:

<pre><code>
curl -i http://localhost:9999/say -XPOST -d'{"name":"ari"}'
HTTP/1.1 200 OK
Server: MochiWeb/1.0 (Any of you quaids got a smint?)
Date: Tue, 04 Aug 2009 08:23:54 GMT
Content-Type: text/json
Content-Length: 20

{"hello back":"ari"}
</code></pre>


It's as easy as pie to extend Alice.

## Wonderland

Wonderland is the webUI to Alice. It is driven by the javascript framework <a href="http://code.quirkey.com/sammy">Sammy</a> and Alice in the backend. Because the framework is client-side and accesses the data through <a href="http://en.wikipedia.org/wiki/Ajax_(programming)">ajax</a>, Wonderland can be deployed nearly anywhere.

## Quickstart
<pre><code>
cd alice
make wonderland
</code></pre>

## Community
* <a href="http://github.com/auser/alice/issues">Issue tracker</a>
* <a href="http://groups.google.com/group/alice-and-wonderland?lnk=gcimv">Google group</a>
* <a href="irc://irc.freenode.net#poolpartyrb">irc: irc.freenode.net / #poolpartyrb </a>

Or feel free to ping me on email (arilerner dot mac dot com) if you have any questions.