desc "run"
task :run do
  puts "call application:start(userapp)."
  exec "sudo erl +A 1 +Ktrue -boot start_sasl +W w -sname userapp -pa ebin -pa src -pa /usr/local/lib/yaws/ebin -yaws embedded true -mnesia dir 'userapp.mnesia' -pa ../rabbitmq-server/ebin -I../rabbitmq-server/include"
end
