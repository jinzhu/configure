# sudo gem install genki-irb_rocket
# interactive_editor
%w(rubygems).map {|x| require x }
#require "/usr/lib64/ruby/gems/1.8/gems/genki-irb_rocket-0.1.3/lib/irb_rocket.rb"
require "/usr/lib64/ruby/gems/1.8/gems/interactive_editor-0.0.3/lib/interactive_editor.rb"

IRB.conf[:SAVE_HISTORY] = 100
IRB.conf[:HISTORY_FILE] = "~/.irb_history"
 
IRB.conf[:PROMPT_MODE] = :SIMPLE
 
IRB.conf[:AUTO_INDENT] = true

class Object
  # list methods which aren't in superclass
  def local_methods(obj = self)
    (obj.methods - obj.class.superclass.instance_methods).sort
  end
  
  # print documentation
  #
  # ri 'Array#pop'
  # Array.ri
  # Array.ri :pop
  # arr.ri :pop
  def ri(method = nil)
    unless method && method =~ /^[A-Z]/ # if class isn't specified
      klass = self.kind_of?(Class) ? name : self.class.name
      method = [klass, method].compact.join('#')
    end
    puts `qri '#{method}'`
  end

  alias qri ri
end
 
load File.dirname(__FILE__) + '/.railsrc' if $0 == 'irb' && ENV['RAILS_ENV']
