export TERM=xterm

socat - EXEC:'opencomalrun opencomal.cml',pty,setsid,ctty < /dev/zero > /dev/null

cat output.txt
rm output.txt