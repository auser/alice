## Alice

<p><a href="http://alicetheapp.com">http://alicetheapp.com</a>.</p>

As a queue server, <a href="http://www.rabbitmq.com/">RabbitMQ</a> is super cool, but my company is hesitant to use it without a nice front-end or access to statistics about the server. So we set out to develop the latest RabbitMQ REST interface, <a href="http://github.com/auser/alice">Alice</a>.

Alice is a RESTful interface to the RabbitMQ server that talks directly through erlang's native interface, <a href="http://www.erlang.org/doc/man/epmd.html">epmd</a>. The purely <a href="http://en.wikipedia.org/wiki/Representational_State_Transfer">RESTful</a> server responds to the same interface as the RabbitMQ's command-line interface and presents a native HTTP interface to the data. <a href="http://alicetheapp.com">Alice</a> is written with <a href="http://code.google.com/p/mochiweb/">Mochiweb</a>.

#### Currently exposed RESTful routes
{% highlight make %}
  /conn - Current connection information
  /exchanges - Current exchanges information
  /queues - Current queues
  /users - Current users
  /bindings - Current bindings
  /control - Access to the RabbitMQ control
  /permissions - Current permissions
  /vhosts - Current vhosts
{% endhighlight %}

These endpoints all are exposed with the four verbs (get, post, put, delete) and respond in the <a href="http://www.json.org/">JSON</a> format, (except the root / endpoint which responds with text/html).

For example:

##### List all the vhosts:

{% highlight bash %}
auser $ curl -i http://localhost:9999/vhosts
HTTP/1.1 200 OK
Server: MochiWeb/1.0 (Any of you quaids got a smint?)
Date: Thu, 16 Jul 2009 00:08:43 GMT
Content-Type: text/json
Content-Length: 16

{"vhosts":["/"]}
{% endhighlight %}

##### Add a user

{% highlight bash %}
auser $ curl -i -XPOST \
        -d'{"username":"ari", "password":"weak password"}' \
        http://localhost:9999/users
        
HTTP/1.1 200 OK
Server: MochiWeb/1.0 (Any of you quaids got a smint?)
Date: Thu, 16 Jul 2009 00:10:35 GMT
Content-Type: text/json
Content-Length: 25

{"users":["ari","guest"]}
{% endhighlight %}


#### Extending
Alice is written with the intention of being highly extensible and makes it easy to do so. The controllers respond only to the four verbs with pattern-matching on the routes. 

#### Quickstart

How to get started.

{% highlight bash %}
git clone git://github.com/auser/alice.git
cd alice
./start.sh
{% endhighlight %}


## Wonderland

Wonderland is the webUI to Alice. It is driven by the javascript framework <a href="http://code.quirkey.com/sammy">Sammy</a> and Alice in the backend. Because the framework is client-side and accesses the data through <a href="http://en.wikipedia.org/wiki/Ajax_(programming)">ajax</a>, Wonderland can be deployed nearly anywhere.

#### Quickstart
{% highlight bash %}
cd alice
make wonderland
{% endhighlight %}

Check these two projects out on github at:<br />
<a href="http://github.com/auser/alice">http://github.com/auser/alice</a><br />
<a href="http://github.com/auser/wonderland">http://github.com/auser/wonderland</a>.

Alice, the app
<a href="http://alicetheapp.com">http://alicetheapp.com</a>
  
== Authors
  Ari Lerner

http://alicetheapp.com

== Thanks
  Nate Murray
  Michael Fairchild
  AT&T interactive's CloudTeam