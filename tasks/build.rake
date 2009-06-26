# modified from http://21ccw.blogspot.com/2008/04/using-rake-for-erlang-unit-testing.html
require 'rake/clean'
require 'pp'

deps_dir   = Dir.pwd + "/deps"

INCLUDE    = File.dirname(__FILE__) + "/include"

DEPS       = Dir["#{deps_dir}/*"].select {|d| d if File.directory? d }
DEPS_FILES = DEPS.map {|d| "./deps/#{File.basename(d)}" }
EXTRA_ERLC = DEPS_FILES.map {|a| "-pa #{a}/ebin" }.join(" ")

ERLC_FLAGS = "-I#{INCLUDE} +warn_unused_vars +warn_unused_import -o ebin -W0 #{EXTRA_ERLC}"

SRC        = FileList["src/*.erl"]
SRC_OBJ    = SRC.pathmap("%{src,ebin}X.beam")

DEP        = DEPS_FILES.map {|d| FileList["#{d}/src/*.erl"]}
DEP_OBJ    = DEP.map {|d| d.pathmap("%{src,ebin}X.beam")}

TEST       = FileList['test/src/*.erl']
TEST_OBJ   = TEST.pathmap("%{src,ebin}X.beam")

CLEAN.include("ebin/*.beam", "test/ebin/*.beam")

directory 'ebin'
directory 'test/ebin'

rule( ".beam" => ["%{ebin,src}X.erl"] ) do |t|
  testing  = t.source =~ /test\// ? true : false
  eunit    = testing ? "-D EUNIT "  : ""
  ebin_dir = testing ? "test/ebin"  : "ebin"
  cmd = "erlc #{eunit}-pa ebin -W #{ERLC_FLAGS} -o #{ebin_dir} #{t.source}"
  puts cmd
  sh cmd
end

desc "Compile everything"
task :compile   => ["src:compile", "test:compile"]
task :recompile => ["clean", "src:compile", "test:compile"]

namespace :src do
  desc "Compile src"
  task :compile => ['ebin'] + SRC_OBJ
end

namespace :test do
  desc "Compile tests"
  task :compile => ['test/ebin'] + TEST_OBJ
end

desc "Run all tests"
task :run_tests => :compile do
  puts "Modules under test:"
  TEST_OBJ.each do |obj|
    obj[%r{.*/(.*).beam}]
    mod = $1
    test_cmd = "erl -pa ebin -pa test/ebin -run #{mod} test -run init stop"
    test_output = `#{test_cmd}`
    
    puts test_output if Rake.application.options.trace

    if /\*failed\*/ =~ test_output
      test_output[/(Failed.*Aborted.*Skipped.*Succeeded.*$)/]
    else
      test_output[/1>\s*(.*)\n/]
    end

    puts "#{mod}: #{$1}"
  end
end

desc "Clean the beams from the ebin directory"
task :clean do
  # cmd = "rm #{::File.dirname(__FILE__)}/ebin/*.beam"
  # Kernel.system cmd
end

desc "Recompile the sources"
task :recompile => [:clean, :compile]

desc "Compile with the DEBUG flag set to true"
task :compile_debug do
  Dir["#{::File.dirname(__FILE__)}/src/*.erl"].each do |t|
    Kernel.system "erlc -pa ebin -W #{ERLC_FLAGS} -Ddebug -o ebin #{t}"
  end
end

desc "Rebuild the boot scripts"
task :build_boot_scripts => [:recompile] do
  puts "Rebuilding boot scripts"
  
  root_dir = ::File.expand_path( ::File.join(::File.dirname(__FILE__)) )
  @version = ENV["VERSION"] || ENV["V"] || "0.1"
  @name = ENV["NAME"] || ::File.basename(::File.dirname( root_dir ))
  
  cmd = "erl -pa ./ebin/ -run packager start #{@name} #{@version} -run init stop -noshell"
  Kernel.system cmd
end

desc "Rebuild and repackage"
task :repackage => [:build_boot_scripts] do  
  cmd = "erl -pa ./ebin -s packager start -s init stop"
  Kernel.system cmd
end

desc "Shell command"
task :shell do
  cmd = "erl -pa ./ebin #{EXTRA_ERLC} -boot start_sasl"
  puts cmd if Rake.application.options.trace
  Kernel.system cmd
end

namespace(:deps) do
  desc "Compile deps"
  task :compile do
    DEPS_FILES.each do |dir|
      Kernel.system "cd #{dir} && rake compile"
    end
  end
  
  desc "Update deps/"
  task :update do
    update_cmd = "git remote update && git merge origin/master" # "git fetch && git rebase origin/master"
    DEPS_FILES.each do |dir|
      cmd = "cd #{dir} && #{update_cmd}"
      puts cmd if Rake.application.options.trace
      Kernel.system cmd
    end  
  end
  
  desc "Update and compile deps/"
  task :up => [:update, :compile]
  
  desc "Clean the deps"
  task :clean do
    DEPS_FILES.each do |dir|
      cmd = "cd #{dir}/ebin && rm *.beam"
      puts cmd if Rake.application.options.trace
      Kernel.system cmd
    end
  end
end

desc "Build eunit"
task :build_eunit do
  cmd =  "cd test/include/eunit && make"
  Kernel.system cmd
end

desc "Compile and get a shell"
task :compile_shell => [:compile, :shell]