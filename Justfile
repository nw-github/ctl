run path *ARGS:
    cargo -q r -- {{path}} | clang -Wall -Wextra -std=c11 -lgc -I./ctl -x c - && ./a.out {{ARGS}}

print path:
    cargo -q r -- {{path}} | clang-format

clean:
    rm main.c
    rm a.out

code:
    cd ide/code && just yaml
    cd ide/code && rsync -av . ~/.vscode-oss/extensions/ctl --exclude node_modules --exclude .vscode
