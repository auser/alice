begin
  require 'config/requirements'
  require 'config/hoe' # setup Hoe + all gem configuration
rescue Exception => e
end

Dir['tasks/**/*.rake'].each { |rake| load rake }