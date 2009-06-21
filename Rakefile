require 'rake'
path  = File.dirname(__FILE__)
files = `find #{path} -maxdepth 1 -iname '\.?*' -not -name '.git'`.split("\n")

task :cp_xmonad do
  system('sudo cp xmonad.desktop /usr/share/xsessions/') 
end

task :make_dwm do
  system('cd dwm && make && sudo make install') 
  system('xmonad --recompile') 
end

task :install => {:make_dwm,:cp_xmonad} do
  files.each do |x|
    x.strip!
    system("ln -nfs #{x} ~/")
    puts "\e[32m#{File.basename(x)} Installed\e[0m"
  end
  puts "\e[33mInstall Complete\e[0m"
end

task :remove do
  files.each do |x|
    x.strip!
    system("rm -f ~/#{File.basename(x)}")
    puts "\e[32m#{File.basename(x)} Removed\e[0m"
  end
  puts "\e[33mAll Removed\e[0m"
end
