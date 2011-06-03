IGNORES   = %w(.gitignore .git)
IRREGULAR = {'xmonad.desktop' => '/usr/share/xsessions/xmonad.desktop', 'ssh_config' => '~/.ssh/config', '20-thinkpad.conf' => '/etc/X11/xorg.conf.d/20-thinkpad.conf'}

FILES = Dir.entries('.').select do |x|
  x !~ /^\.*$/ && x =~ /^\./ && !(IGNORES||[]).include?(x)
end.inject({}) do |s,x|
  s.merge({x.to_s => File.join(ENV['HOME'],x.to_s)})
end.merge(IRREGULAR)

task :make_dwm do
  system('cd dwm && make && sudo make install') 
end

task :make_xmonad do
  system('xmonad --recompile') 
end

def exec(str)
  FILES.each do |k,v|
    command = str.gsub('KEY',File.join(Dir.pwd,k)).gsub('VALUE',v)
    puts("\e[32m#{command}\e[0m")
    system(command)
  end
end

task :install do
  exec("ln -nfs KEY VALUE")
  puts "\e[33mInstall Complete\e[0m"
end

task :remove do
  exec("rm -f VALUE")
  puts "\e[33mAll Removed\e[0m"
end
