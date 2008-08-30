require 'irb/completion'

def ri(*names)
  system(%{qri #{names.map {|name| name.to_s}.join(" ")}})
end

alias qri ri

$LOAD_PATH << "/usr/lib/ruby/gems/1.8/gems/open4-0.9.6/lib/"
