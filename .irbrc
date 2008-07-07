require 'irb/completion'

def ri(*names)
  system(%{qri #{names.map {|name| name.to_s}.join(" ")}})
end
