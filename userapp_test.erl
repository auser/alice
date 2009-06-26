-module(userapp_test).

-export([get_user/0, update_user/0, delete_user/0, create_user/0]).

get_user() ->
    http:request(get, {"http://localhost:8007/user/12345", []}, [], []).

update_user() ->
    Data = "<user xmlns=\"urn:userapp:user\"><id>12345</id><name>Testy McTester</name><email>testy@mctestor.com</email><password>testy123</password><website>http://www.mctestor.com/</website></user>",
    http:request(post, {"http://localhost:8007/user/12345", [], "application/xml", Data}, [], []).

delete_user() ->
    http:request(delete, {"http://localhost:8007/user/12345", []}, [], []).

create_user() ->
    Data = "<user xmlns=\"urn:userapp:user\"><name>Testy McTester</name><email>testy.mctestor@mctestor.com</email><password>testy123</password></user>",
    http:request(put, {"http://localhost:8007/user/", [], "application/xml", Data}, [], []).
