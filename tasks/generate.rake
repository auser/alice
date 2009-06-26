desc "Generate a basic app file"
task :appfile do
  @name = ENV["NAME"].empty? ? ::File.basename(::File.dirname( root_dir )) : ENV["NAME"]
  Kernel.system "#{::File.dirname(__FILE__)}/../bin/appfile #{@name}"
end