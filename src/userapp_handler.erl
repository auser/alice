-module(userapp_handler).

-export([out/1, handle_request/3, get_path/1, make_response/2]).
-export([make_response/3, make_all_response/3, validate_request/1]).
-export([write_record/1, text_or_default/3, object_counter/1]).
-export([record_to_xml/1, find_record/1, delete_record/1]).

-include("/usr/local/lib/yaws/include/yaws.hrl").
-include("/usr/local/lib/yaws/include/yaws_api.hrl").
-include_lib("xmerl/include/xmerl.hrl").
-include_lib("stdlib/include/qlc.hrl").
-include("userapp.hrl").

out(Arg) ->
    Req = Arg#arg.req,
    ReqPath = userapp_handler:get_path(Req),
    io:format("~p - ~p~n", [Req#http_request.method, ReqPath]),
    userapp_handler:handle_request(Req#http_request.method, ReqPath, Arg).

get_path(Req) -> {_, Path} = Req#http_request.path, Path.

handle_request('GET', "/user/" ++ UserID, _Arg) ->
    case userapp_handler:find_record(UserID) of
        [] -> userapp_handler:make_response(404, "<error>No user for that id.</error>");
        [User] ->
            XmlBody = userapp_handler:record_to_xml(User),
            userapp_handler:make_response(200, XmlBody)
    end;

handle_request('POST', "/user/" ++ UserID, Arg) ->
    case userapp_handler:find_record(UserID) of
        [] -> userapp_handler:make_response(404, "<error>No user for that id.</error>");
        [User] ->
            case userapp_handler:validate_request(Arg#arg.clidata) of
                {ok, Xml} ->
                    Name = userapp_handler:text_or_default(Xml, "/user/name/text()", "none"),
                    Email = userapp_handler:text_or_default(Xml, "/user/email/text()", "none"),
                    Password = userapp_handler:text_or_default(Xml, "/user/password/text()", "none"),
                    Website = userapp_handler:text_or_default(Xml, "/user/website/text()", "none"),
                    NewUser = User#user{
                        id = UserID, name = Name, email = Email,
                        password = Password, website = Website
                    },
                    case userapp_handler:write_record(NewUser) of
                        ok -> userapp_handler:make_response(201, userapp_handler:record_to_xml(NewUser));
                        _ -> userapp_handler:make_response(500, "<data>Error creating data.</data>")
                    end;
                _ -> userapp_handler:make_response(500, "<data>Error updating data.</data>")
            end
    end;

handle_request('PUT', "/user" ++ _, Arg) ->
    case userapp_handler:validate_request(Arg#arg.clidata) of
        {ok, Xml} ->
            UserID = case userapp_handler:text_or_default(Xml, "/user/id/text()", none) of
                none -> userapp_handler:object_counter(user);
                TmpId -> TmpId
            end,
            Name = userapp_handler:text_or_default(Xml, "/user/name/text()", "none"),
            Email = userapp_handler:text_or_default(Xml, "/user/email/text()", "none"),
            Password = userapp_handler:text_or_default(Xml, "/user/password/text()", "none"),
            Website = userapp_handler:text_or_default(Xml, "/user/website/text()", "none"),
            NewUser = #user{
                id = UserID, name = Name, email = Email,
                password = Password, website = Website
            },
            case userapp_handler:write_record(NewUser) of
                ok -> userapp_handler:make_response(201, userapp_handler:record_to_xml(NewUser));
                _ -> userapp_handler:make_response(500, "<data>Error creating data.</data>")
            end;
        _ -> userapp_handler:make_response(500, "<data>Error creating data.</data>")
    end;

handle_request('DELETE', "/user/" ++ UserID, _Arg) ->
    case userapp_handler:find_record(UserID) of
        [] -> userapp_handler:make_response(404, "<error>No user for that id.</error>");
        [User] ->
            case userapp_handler:delete_record(User#user.id) of
                ok -> userapp_handler:make_response(200, "<ok />");
                _ -> userapp_handler:make_response(500, "<data>Error deleting data.</data>")
            end
    end;

handle_request('GET', "/status", _Arg) ->
    % case userapp_handler:find_record(UserID) of
        % [] -> userapp_handler:make_response(404, "<error>No user for that id.</error>");
        % [User] ->
            % XmlBody = userapp_handler:record_to_xml(User),
            % userapp_handler:make_response(200, XmlBody)
    % end;
    io:format("hi people"),
    Res = call_rabbitmq_action(returnstatus), 
    Str = io_lib:format("~p~n", [Res]),
    userapp_handler:make_response(200, Str);



handle_request(_, _, _Arg) -> % catchall
    userapp_handler:make_response(501, "<error>Action not implemented.</error>").

make_response(Status, Message) ->
    userapp_handler:make_response(Status, "application/xml", Message).

make_response(Status, Type, Message) ->
    userapp_handler:make_all_response(Status, make_header(Type), Message).

make_header(Type) -> [{header, ["Content-Type: ", Type]}].

make_all_response(Status, Headers, Message) ->
    [{status, Status}, {allheaders, Headers}, {html, Message}].

validate_request(Xml) when is_binary(Xml) -> validate_request(binary_to_list(Xml));

validate_request(XmlBody) ->
    try xmerl_scan:string(XmlBody, [{validation, schema}, {schemaLocation, [{default, "./user.xsd"}]}]) of
        {XmlElem, _} -> {ok, XmlElem};
        _ -> {error, unknown}
    catch
        _:_ -> {error, throw}
    end.

text_or_default(Xml, Xpath, Default) ->
    case xmerl_xpath:string(Xpath, Xml) of
        [ #xmlText{value = Val} ] -> Val;
        _ -> Default
    end.

object_counter(Name) ->
    [OldRecord] = mnesia:activity(transaction, fun() -> mnesia:read(counter, Name, write) end),
    Count = OldRecord#counter.count + 1,
    NewRecord = OldRecord#counter{count = integer_to_list(Count)},
    % mnesia:write(NewRecord),
    mnesia:activity(transaction, fun() -> mnesia:write(NewRecord) end),
    integer_to_list(Count).

record_to_xml(Rec) ->
    Data = [
        {id, [], [Rec#user.id]},
        {name, [], [Rec#user.name]},
        {email, [], [Rec#user.email]},
        {password, [], [Rec#user.password]},
        {website, [], [Rec#user.website]}
    ],
    {RootEl, _} = xmerl_scan:string("<user xmlns=\"urn:userapp:user\" />"),
    #xmlElement{content = Content} = RootEl,
    NewContent = Content ++ lists:flatten([Data]),
    NewRootEl=RootEl#xmlElement{content=NewContent},    
    Export=xmerl:export_simple([NewRootEl], xmerl_xml),
    lists:flatten(Export).

write_record(Record) ->
    mnesia:activity(transaction, fun() -> mnesia:write(Record) end).

find_record(UserId) ->
    mnesia:activity(transaction, fun() -> qlc:e(qlc:q([R || R <- mnesia:table(user), R#user.id == UserId ])) end).

delete_record(UserId) ->
    mnesia:activity(transaction, fun() -> mnesia:delete(user, UserId, write) end).

call_rabbitmq_action(Command) ->
    userapp_control:action(Command, rabbit_misc:localnode(rabbit), [], fun(_Format, _Args1) -> ok end).
