# sudo gem install genki-irb_rocket
%w(rubygems irb_rocket).map {|x| require x}

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
