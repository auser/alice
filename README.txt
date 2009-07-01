== Alice

Alice is a REST and web front-end to <a href="http://www.rabbitmq.com/">rabbitmq</a>. 

== Starting
  To start Alice, type
    make
  and then 
    ./start.sh

  You'll need to make sure that rabbitmq is running locally as well.

== Extending

Alice assumes that the top level request path will be the "controller" that the response will be handled by. For instance, when calling http://allice.app/users, the user controller will respond. Controllers respond to the methods: get, put, post and delete. A controller template looks like (available in src/rest/controllers/template.erl):

    -module (template).
    -export ([get/1, post/2, put/2, delete/1]).

    get(_Path) -> {"error", <<"unhandled">>}.
    post(_Path, _Data) -> {"error", <<"unhandled">>}.
    put(_Path, _Data) -> {"error", <<"unhandled">>}.
    delete(_Path) -> {"error", <<"unhandled">>}.
  
== Authors
  Ari Lerner
  Nate Murray
  Michael Fairchild

http://alicetheapp.com