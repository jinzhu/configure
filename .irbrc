#!/usr/bin/env ruby

if defined? Bundler
  Gem.post_reset_hooks.reject! { |hook| hook.source_location.first =~ %r{/bundler/} }
  Gem::Specification.reset
  load 'rubygems/custom_require.rb'
end

require 'irbtools/configure'
Irbtools.start

# require 'irb/completion'
# require 'irb/ext/save-history'
#
# IRB.conf[:PROMPT_MODE] = :SIMPLE
# IRB.conf[:SAVE_HISTORY] = 1000
# IRB.conf[:HISTORY_FILE] = "#{ENV['HOME']}/.irb_history"
#
#
# ## Rails
# ActiveRecord::Base.logger.level = 1 if defined? ActiveRecord::Base
#
# def enable_sql
#   ActiveRecord::Base.logger.level = 0 if defined? ActiveRecord::Base
# end
#
# def disable_sql
#   ActiveRecord::Base.logger.level = 0 if defined? ActiveRecord::Base
# end
#
# # Break out of the Bundler jail
# # from https://github.com/ConradIrwin/pry-debundle/blob/master/lib/pry-debundle.rb
#
# if defined? Rails
#   begin
#     require 'hirb'
#     Hirb.enable
#   rescue LoadError
#   end
# end
