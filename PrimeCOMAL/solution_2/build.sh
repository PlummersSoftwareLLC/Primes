export TERM=xterm

socat - EXEC:'opencomal',pty,setsid,ctty > /dev/null << EOF
ENTER "opencomal.txt"
SAVE "opencomal.cml"
BYE
EOF