== Alice

Alice is a REST and web front-end to http://www.rabbitmq.com

== Starting

To start Alice, type
  ./start.sh

This will compile the necessary components, if they are not already compiled and then start
the alice REST server.

You'll need to make sure that rabbitmq is running locally by running:
  sudo rabbitmq-server

Finally, if you want to use wonderland (http://github.com/auser/wonderland/tree/master), type:
  make wonderland

This will grab the latest wonderland source and install it in your web/ directory

== Extending

Alice assumes that the top level request path will be the "controller" that the response will be handled by. For instance, when calling http://allice.app/users, the user controller will respond. Controllers respond to the methods: get, put, post and delete. A controller template looks like (available in src/rest/controllers/template.erl):

    -module (template).
    -export ([get/1, post/2, put/2, delete/2]).

    get(_Path) -> {"error", <<"unhandled">>}.
    post(_Path, _Data) -> {"error", <<"unhandled">>}.
    put(_Path, _Data) -> {"error", <<"unhandled">>}.
    delete(_Path, _Data) -> {"error", <<"unhandled">>}.
  
== Authors
  Ari Lerner

http://alicetheapp.com

== Thanks
  Nate Murray
  Michael Fairchild
  AT&T interactive's CloudTeam