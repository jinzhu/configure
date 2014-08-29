#!/usr/bin/perl

# obmenu-generator - configuration file
# This file will be updated automatically every time when is needed.
# Any additional comment and/or indentation will be lost.

=for comment

|| FILTERING
    | skip_filename_re    : Skip a .desktop file if its name matches the regex.
                            Name is from the last slash to the end. (filename.desktop)
                            Example: qr/^(?:gimp|xterm)\b/,    # skips 'gimp' and 'xterm'

    | skip_entry          : Skip a destkop file if the value from a given key matches the regex.
                            Example: [
                                {key => 'Name', re => qr/(?:about|terminal)/i},
                                {key => 'Exec', re => qr/^xterm/},
                            ],

    | substitutions       : Substitute, by using a regex, in the values of the desktop files.
                            Example: [
                                {key => 'Exec', re => qr/xterm/, value => 'sakura'},
                                {key => 'Exec', re => qr/\\\\/,  value => '\\', global => 1},    # for wine apps
                            ],


|| ICON SETTINGS
    | icon_dirs_first     : When looking for icons, look in this directories first,
                            before looking in the directories of the current icon theme.
                            Example: [
                                "$ENV{HOME}/My icons",
                            ],

    | icon_dirs_second    : Look in this directories after looked in the directories of the
                            current icon theme. (Before /usr/share/pixmaps)
                            Example: [
                                "/usr/share/icons/gnome",
                            ],

    | icon_dirs_last      : Look in this directories at the very last, after looked in
                            /usr/share/pixmaps, /usr/share/icons/hicolor and some other
                            directories.
                            Example: [
                                "/usr/share/icons/Tango",
                            ],

    | strict_icon_dirs    : A true value will make the module to look only inside the directories
                            specified by you in either one of the above three options.

    | gtk_rc_filename     : Absolute path to the GTK configuration file.
    | missing_image       : Use this icon for missing icons (default: gtk-missing-image)


|| KEYS
    | name_keys           : Valid keys for the item names.
                            Example: ['Name[fr]', 'GenericName[fr]', 'Name'],   # french menu


|| PATHS
    | desktop_files_paths   : Absolute paths which contains .desktop files.
                              Example: [
                                '/usr/share/applications',
                                "$ENV{HOME}/.local/share/applications",
                                glob("$ENV{HOME}/.local/share/applications/wine/Programs/*"),
                              ],


|| NOTES
    | Regular expressions:
        * use qr/RE/ instead of 'RE'
        * use qr/RE/i for case insenstive mode

=cut

our $CONFIG = {
  "editor"              => "emacs",
  "Linux::DesktopFiles" => {
                             desktop_files_paths     => ["/usr/share/applications"],
                             gtk_rc_filename         => "/home/jinzhu/.gtkrc-2.0",
                             icon_dirs_first         => undef,
                             icon_dirs_last          => undef,
                             icon_dirs_second        => undef,
                             keep_unknown_categories => 1,
                             skip_entry              => undef,
                             skip_filename_re        => undef,
                             skip_svg_icons          => 1,
                             strict_icon_dirs        => undef,
                             substitutions           => undef,
                             terminalization_format  => "%s -e '%s'",
                             terminalize             => 1,
                             unknown_category_key    => "other",
                           },
  "missing_icon"        => "gtk-missing-image",
  "name_keys"           => ["Name"],
  "terminal"            => "gnome-terminal",
  "VERSION"             => 0.59,
}