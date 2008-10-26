require 'rake'

task :install do
  path = File.dirname(__FILE__)
  result = `find -maxdepth 1  -iname '\.*' -not -name '.git' -not -name '.'`
  result.to_a.each do |x|
    `ln -nfs #{(path + x.sub(/^\./,'')).rstrip} ~/`
  end
end
