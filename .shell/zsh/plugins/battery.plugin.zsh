error_msg='no battery'

function battery_pct_remaining() {
  if [[ $(acpi 3&>/dev/null | grep -c '^Battery.*Discharging') -gt 0 ]] ; then
    echo "$(acpi | cut -f2 -d ',' | tr -cd '[:digit:]')"
  else
    echo $error_msg
  fi
}

function battery_time_remaining() {
  if [[ $(acpi 2&>/dev/null | grep -c '^Battery.*Discharging') -gt 0 ]] ; then
    echo $(acpi | cut -f3 -d ',')
  else
    echo $error_msg
  fi
}

function battery_pct_prompt() {
  if [[ $(acpi 2&>/dev/null | grep -c '^Battery.*Discharging') -gt 0 ]] ; then
    b=$(battery_pct_remaining)
    if [ $b -gt 50 ] ; then
      color='green'
    elif [ $b -gt 20 ] ; then
      color='yellow'
    else
      color='red'
    fi
    echo "%{$fg[$color]%}[$(battery_pct_remaining)%%]%{$reset_color%}"
  else
    echo ''
  fi
}
