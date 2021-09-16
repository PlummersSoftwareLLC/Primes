export TERM=xterm

set -e

timeout -s 9 1m socat - EXEC:'opencomal',pty,setsid,ctty > /dev/null << EOF
ENTER "opencomal.txt"
SAVE "opencomal.cml"
BYE
EOF