export TERM=xterm

timeout -s 9 1m socat - EXEC:'opencomalrun opencomal.cml',pty,setsid,ctty < /dev/zero > /dev/null

if [ -f output.txt ]; then
    cat output.txt
    rm output.txt
fi